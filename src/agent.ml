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

type http_status_code = Cohttp.Code.status_code
type http_headers = Cohttp.Header.t
type http_meth = Cohttp.Code.meth

type response = Cohttp.Response.t
type result = t * (response * string)

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

let default_max_redirect = 5

let init ?(max_redirect = default_max_redirect) _ =
  { proxy = None;
    cookie_jar = Cookiejar.empty;
    client_headers = Header.init ();
    max_redirect;
    redirect = 0}

let rec redirect (agent,(response,content)) =
  match response.status with
    | `Moved_permanently
    | `Found ->
      (match Cohttp.Header.get response.headers "Location" with
        | Some loc ->
          { agent with redirect = succ agent.redirect}
          |> get loc
        | None -> Lwt.return ({ agent with redirect = 0 }, (response,content)) )
    | _ -> Lwt.return ({ agent with redirect = 0 }, (response,content))

and update_agent uri meth agent (response,content) =
  let code = Response.status response in
  let headers = Response.headers response in
  let agent = 
    {agent with cookie_jar = 
      Cookiejar.add_from_headers uri response.headers agent.cookie_jar}
  in
  if agent.redirect < agent.max_redirect then
    redirect agent
  else
    Lwt.return { agent with redirect=0 }

and get_uri uri agent =
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.get ~headers uri
  >>= function (response,content) ->
  
  >>= update_agent uri agent

and get uri_string agent =
  get_uri (Uri.of_string uri_string) agent

let load image agent =
  let page = agent.last_page in
  let uri = Page.Image.uri image in
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.get ~headers:headers uri
  >>= update_agent uri `GET agent
  >|= fun agent ->
    {agent with last_page = page}

let click link = link |> Page.Link.uri |> get_uri

let post_uri uri content agent =
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.post ~headers:headers ~body:(Cohttp_lwt_body.of_string content) uri
  >>= update_agent uri `POST agent

let post uri_string content agent =
  post_uri (Uri.of_string uri_string) content agent

let submit form agent =
  let uri = Page.Form.action form in
  let params = Page.Form.values form in
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.post_form ~headers:headers ~params:params uri
  >>= update_agent uri `POST agent

let save_content file agent =
  Lwt_io.open_file Lwt_io.output file
  >>= (fun out ->
    Lwt_io.write out agent.last_body
    |> ignore;
    Lwt_io.close out)

let save_image image file agent =
  let uri = Page.Image.uri image in
  agent
  |> get_uri uri
  >>= save_content file

let uri agent = agent.last_uri
let meth agent = agent.last_meth
let page agent = agent.last_page
let content agent = agent.last_body
let server_headers agent = agent.last_headers
let status_code agent = agent.last_status_code
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
