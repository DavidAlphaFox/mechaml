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

(** Monad

    This module defines a monad that manages a state corresponding to the agent
    so that it is not needed to carry it everywhere explicitely as a parameter,
    all inside the Lwt.t monad. Morally, one can think of a state monad
    specialized and the Lwt.t monad stacked.
*)

type 'a m = Agent.t -> (Agent.t * 'a) Lwt.t

val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
val (>>) : 'a m -> 'b m -> 'b m
val return : 'a -> 'a m

val run : Agent.t -> 'a m -> (Agent.t * 'a)

val set_proxy : ?user:string
  -> ?password:string
  -> host:string
  -> port:int
  -> unit m

(** To use the monad operators, one needs to fix type mismatches for function
    defined in module {! Agent}. For example, the return type of {! Agent.get}
    is [type result = Agent.t response * string] while it should be [(response *
    string) m = Agent.t * (response * string)] to be usable. These types
    trivially isomorphic but not equal in Ocaml.

    For functions operating on the agent such as {! Agent.cookiejar} or {!
    Agent.set_cookie_jar}, one needs to wrap their type to match the monad
    constraint. For example, the first one go from [Agent.t -> Cookiejar.t] to
    [Agent.t -> (Agent.t * Cookiejar.t) Lwt.t] by just returning the agent
    unmodified together with the cookie jar, the whole result being wrapped in
    Lwt.return

    Note that the redefined functions have the same name as their counterpart,
    and thus will shadow or can be shadowed by them.
*)

val init : ?max_redirect:int -> unit -> unit m

val cookie_jar : Cookiejar.t m
val set_cookie_jar : Cookiejar.t -> unit m
val add_cookie : Cookiejar.Cookie.t -> unit m
val remove_cookie : Cookiejar.Cookie.t -> unit m

val client_headers : Cohttp.Header.t m
val set_client_headers : Cohttp.Header.t -> unit m
val add_client_headerM : string -> string -> unit m
val remove_client_header : string -> unit m

val set_max_redirect : int -> unit m
