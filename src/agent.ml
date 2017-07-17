(*{{{ Copyright (C) 2016, Yann Hamdaoui <yann.hamdaoui@centraliens.net>
  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all
  copies.
  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
  CONSEQUENTIAL
  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
  DATA
  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
  USE OR
  PERFORMANCE OF THIS SOFTWARE.
  }}}*)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

open Cohttp
open Cohttp_lwt_unix

type http_status_code = Code.status_code
type http_headers = Header.t

module HttpResponse = struct
  type t = Response.t * string 

  let status (response,_) =
    Response.status response

  let headers (response,_) =
    Response.headers response

  let content = snd

  let cohttp_response = fst
end

type response = HttpResponse.t

type proxy = {
  user : string option;
  password : string option;
  host : string;
  port : int
}

type t = {
  proxy : proxy option;
  cookie_jar : Cookiejar.t;
  client_headers : Header.t;
  max_redirect : int;
  redirect : int
}

type result = t * HttpResponse.t 

let default_max_redirect = 5

let init ?(max_redirect = default_max_redirect) _ =
  { proxy = None;
    cookie_jar = Cookiejar.empty;
    client_headers = Header.init ();
    max_redirect;
    redirect = 0}

let rec redirect (agent,(response,content)) =
  match Response.status response with
    | `Moved_permanently
    | `Found ->
      (match Header.get (Response.headers response) "Location" with
        | Some loc ->
          { agent with redirect = succ agent.redirect}
          |> get loc
        | None -> Lwt.return ({ agent with redirect = 0 },(response,content)) )
    | _ -> Lwt.return ({ agent with redirect = 0 },(response,content))

and update_agent uri agent (response,body) =
  let headers = Response.headers response in
  let agent = 
    {agent with cookie_jar = 
      Cookiejar.add_from_headers uri headers agent.cookie_jar}
  in
  body
  |> Cohttp_lwt_body.to_string
  >>= (function content -> 
    if agent.redirect < agent.max_redirect then
      redirect (agent,(response,content))
    else
      Lwt.return ({ agent with redirect=0 },(response,content)))

and get_uri uri agent =
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.get ~headers uri
  >>= update_agent uri agent

and get uri_string agent =
  get_uri (Uri.of_string uri_string) agent

let click link = link |> Page.Link.uri |> get_uri

let post_uri uri content agent =
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.post ~headers:headers ~body:(Cohttp_lwt_body.of_string content) uri
  >>= update_agent uri agent

let post uri_string content agent =
  post_uri (Uri.of_string uri_string) content agent

let submit form agent =
  let uri = Page.Form.action form in
  let params = Page.Form.values form in
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.post_form ~headers:headers ~params:params uri
  >>= update_agent uri agent

let save_content file data =
  Lwt_io.open_file Lwt_io.output file
  >>= (fun out ->
    Lwt_io.write out data
    |> ignore;
    Lwt_io.close out)

let save_image image file agent =
  let uri = Page.Image.uri image in
  agent
  |> get_uri uri
  >>= (function (agent,(response,content)) ->
    save_content file content
    >|= fun _ -> (agent,(response,content)))

let code_of_status = Code.code_of_status

let set_proxy ?user ?password ~host ~port agent =
  {agent with proxy = Some ({user = user; password = password;
    host = host; port = port})}

let disable_proxy agent = {agent with proxy = None}

let cookie_jar agent = agent.cookie_jar
let set_cookie_jar cookie_jar agent = {agent with cookie_jar = cookie_jar}
let add_cookie cookie agent =
  {agent with cookie_jar = Cookiejar.add cookie agent.cookie_jar}
let remove_cookie cookie agent =
  {agent with cookie_jar = Cookiejar.remove cookie agent.cookie_jar}

let client_headers agent = agent.client_headers
let set_client_headers headers agent = {agent with client_headers = headers}
let add_client_header header value agent =
  {agent with client_headers = Header.add agent.client_headers header value}
let remove_client_header header agent =
  {agent with client_headers = Header.remove agent.client_headers header}

let set_max_redirect max_redirect agent = {agent with max_redirect }

module Monad = struct
  type 'a m = t -> (t * 'a) Lwt.t
  type result = response * string

  let bind (x : 'a m) (f : 'a -> 'b m) =
    fun agent ->
      Lwt.bind (x agent) (fun (agent,result) ->
        f result agent)

  let (>>=) = bind

  let (<<=) x f = f >>= x

  let (>>) x y = x >>= (fun _ -> y)

  let (<<) y x = x >> y

  let return (x : 'a) = 
    fun agent -> Lwt.return (agent,x)

  let return_from_lwt (x : 'a Lwt.t) =
    fun agent ->
      Lwt.bind x (fun y ->
        Lwt.return (agent,y))

  let map (f : 'a -> 'b) (x : 'a m) =
    x >>= (function y ->
      f y
      |> return)

  let run (agent : t) (x : 'a m) =
    Lwt_main.run (x agent)

  let (>|=) (x : 'a m) (f : 'a -> 'b) = x |> map f
  let (<|=) f x = x |> map f

  let save_content data file =
    save_content data file
    |> return_from_lwt

  let monadic_get g =
    fun agent ->
      Lwt.return (agent, g agent)

  let monadic_set s =
    fun agent ->
      Lwt.return (s agent, ())

  (* let set_proxy ?user ?password ~host ~port = *)
  (*   set_proxy ~user ~password ~host ~port *)
  (*   |> monadic_set *)

  let cookie_jar = monadic_get cookie_jar

  let set_cookie_jar jar =
    set_cookie_jar jar
    |> monadic_set

  let add_cookie cookie =
    add_cookie cookie
    |> monadic_set

  let remove_cookie cookie =
    remove_cookie cookie
    |> monadic_set

  let client_headers = monadic_get client_headers

  let set_client_headers headers = 
    set_client_headers headers
    |> monadic_set

  let add_client_header key value =
    add_client_header key value
    |> monadic_set

  let remove_client_header key =
    remove_client_header key
    |> monadic_set

  let set_max_redirect n =
    set_max_redirect n
    |> monadic_set
end
