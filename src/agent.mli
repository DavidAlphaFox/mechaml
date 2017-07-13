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

(** Scraping agent

    Mechaml is a web agent that allows to :

      - Fetch and parse HTML pages
      - Analyze, fill and submit HTML forms
      - Manages cookies, headers and redirections
      - Use a proxy (soon to be implemented)

    It is build on top of Cohttp, Lwt and Lambdasoup.
*)


type t
type http_status_code = Cohttp.Code.status_code
type http_headers = Cohttp.Header.t
type http_meth = Cohttp.Code.meth

type response = Cohttp.Response.t
  (* = { *)
  (*   enconding: Cohttp.Transfer.enconding; *)
  (*   headers: Cohttp.Header.t; *)
  (*   version: Cohttp.Code.version; *)
  (*   status: http_status_code; *)
  (*   flush: bool; *)
  (* } *)

type result = t * response * string

(** {2 Main operations } *)

(** Create a new empty agent. [~max_redirect] indicates how many times the agent
   will automatically and consecutively follow the [Location] header in case of
   HTTP 302 or 303 response codes to avoid a redirect loop. Set
   to [0] to disable any automatic redirection.
 *)
val init : ?max_redirect:int -> unit -> t

(** Perform a get request to the specified URI *)

val get : string -> t -> result Lwt.t
val get_uri : Uri.t -> t -> result Lwt.t

(** Same as get, but work directly with links instead of URIs *)
val click : Page.Link.t -> t -> result Lwt.t

(** Send a raw post requet to the specified URI *)

val post : string -> string -> t -> result Lwt.t
val post_uri : Uri.t -> string -> t -> result Lwt.t

(** Submit a filled form *)
val submit : Page.Form.t -> t -> result Lwt.t

(** [save_image image "myfile.jpg" agent] load the image using [get], open
   [myfile.jpg] and write the received content.  *)
val save_image : Page.Image.t -> string -> t -> result Lwt.t

(** [save_content content "myfile.html"] write the specified content in a file *)
val save_content : string -> string -> unit Lwt.t

(** {3 Proxy} *)

(** Proxy are currently NOT SUPPORTED YET *)

val set_proxy : ?user:string
  -> ?password:string
  -> host:string
  -> port:int
  -> t -> t

val disable_proxy : t -> t

(** {4 Cookies} (see {!module:Cookiejar}) *)

(** Return the current Cookiejar *)
val cookie_jar : t -> Cookiejar.t

(** Change the current Cookiejar *)
val set_cookie_jar : Cookiejar.t -> t -> t

(** Add a single cookie to the current Cookiejar *)
val add_cookie : Cookiejar.Cookie.t -> t -> t

(** Remove a single cookie from the Cookiejar *)
val remove_cookie : Cookiejar.Cookie.t -> t -> t

(** {5 Headers} *)

(** Return the default headers sent when performing HTTP requests *)
val client_headers : t -> Cohttp.Header.t

(** Use the specified headers as new default headers *)
val set_client_headers : Cohttp.Header.t -> t -> t

(** Add a single pair key/value to the default headers *)
val add_client_header : string -> string -> t -> t

(** Remove a single pair key/value from the default headers *)
val remove_client_header : string -> t -> t

(** {6 Redirection} *)

(** Max redirection to avoid infinite loops (use 0 to disable automatic
   redirection) *)
val set_max_redirect : int -> t -> t

(** The default maximum consecutive redirections. Used to avoid redirect loops *)
val default_max_redirect : int
