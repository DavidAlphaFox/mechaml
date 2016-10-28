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

open OUnit2
open Mechaml
open Cohttp
open Infix.Option

let _ = Random.self_init ()

(** Helper functions for cookiejar testing **)

let random_char () =
  Random.int(26) + (Char.code 'a')
  |> Char.chr

let random_string n =
  String.init n (fun _ -> random_char ())

let between min max =
  Random.int(max-min) + min

let random_uri () =
  let host = random_string (between 5 10) in
  let ext = random_string 2 in
  let path = random_string (between 5 10) in
  Printf.sprintf "http://%s.%s/%s" host ext path
  |> Uri.of_string

let random_cookie domain =
  let name = random_string (between 5 10) in
  let value = random_string (between 5 10) in
  (* let domain = (random_string (between 5 10))^"."^(random_string 2) in *)
  Cookiejar.Cookie.make ~domain name value

let rec random_cookies domain = function
  | 0 -> []
  | n -> (random_cookie domain)::(random_cookies domain (n-1))

let jar_from =
  List.fold_left (fun j c -> Cookiejar.add c j) Cookiejar.empty

let uri = random_uri ()
let cookies = random_cookies (Uri.host uri |? "dunno.com") 10
let jar = jar_from cookies

let rec jar_mem c jar =
  let f x c =
    if x=c then
      raise Exit
    else
      c
  in match Cookiejar.fold f jar c with
    | exception Exit -> true
    | _ -> false

let rec jar_eq cookies jar =
  match cookies with
    | [] -> Cookiejar.is_empty jar
    | c::cs ->
      (match jar_mem c jar with
        | true -> jar_eq cs (Cookiejar.remove c jar)
        | false -> false)

let to_set_cookie cookie =
  let name = Cookiejar.Cookie.name cookie in
  let value = Cookiejar.Cookie.value cookie in
  let domain = Cookiejar.Cookie.domain cookie in
  Printf.sprintf "%s=%s; Domain=%s" name value domain

let to_set_cookies cookies =
  let rec f s = function | [] -> s
  | [c] ->
    s^(to_set_cookie c)
  | c::cs ->
    let s = Printf.sprintf "%s%s, " s (to_set_cookie c) in
    f s cs in
  f "" cookies

(** Helper functions for page testing **)

let page : string -> string =
  let table = Hashtbl.create 7 in
  let directory = "test/pages" in
  Sys.readdir directory
  |> Array.iter (fun file ->
    let contents = file |> Filename.concat directory |> Soup.read_file in
    Hashtbl.replace table file contents);

  fun page_name -> Hashtbl.find table page_name

module type PageElement = sig
  type t
  val to_node : t -> Soup.element Soup.node
end

let test_selector page f (module M : PageElement) node prefix selector expected_count =
  let nodes = f selector page in
  nodes
  |> List.fold (fun c _ -> succ c) 0
  |> assert_equal (prefix^msg) expected_count;
  nodes
  |> List.iter (fun x ->
    x
    |> M.to_node
    |> Soup.name
    |> assert_equal (prefix^"bad node type") node)

let suites = [
  "cookiejar" >::: [
    ("add" >:: fun _ ->
      jar
      |> jar_eq cookies
      |> assert_bool "mismatch between generated jar and original cookie list");

    ("remove" >:: fun _ ->
      List.fold_left (fun jar cookie -> Cookiejar.remove cookie jar) jar cookies
      |> Cookiejar.is_empty
      |> assert_bool "expected empty jar after removing cookies");

    ("add_from_headers" >:: fun _ ->
      let headers_single =
        Header.init_with "Set-Cookie" (to_set_cookies cookies) in
      Cookiejar.empty
      |> Cookiejar.add_from_headers uri headers_single
      |> jar_eq [(List.hd cookies)]
      |> assert_bool "Mismatch between jar generated from one \
        \"Set-Cookie\" header containing all cookies as a list and original cookie\
        list";

      let headers_mult =
        List.fold_left
          (fun h c -> Header.add h "Set-Cookie" (to_set_cookie c))
          (Header.init ()) cookies in
      Cookiejar.empty
      |> Cookiejar.add_from_headers uri headers_mult
      |> jar_eq cookies
      |> assert_bool "Mismatch between a jar generated from multiple \
        \"Set-Cookie\" headers and the first cookie of the original list";

      let cookie = random_cookie (Uri.host uri |? "dunno.com") in
      let name = Cookiejar.Cookie.name cookie in
      let value = Cookiejar.Cookie.value cookie in
      let domain = Cookiejar.Cookie.domain cookie in
      let cookie_uri = Printf.sprintf "http://%s/a.php" domain
      |> Uri.of_string in
      let header_nodomain =
        Header.init_with "Set-Cookie" (Printf.sprintf "%s=%s" name value) in
      Cookiejar.empty
      |> Cookiejar.add_from_headers cookie_uri header_nodomain
      |> jar_eq [cookie]
      |> assert_bool "Mismatch between jar generated from a domain-less \
        \"Set-Cookie\" header and the original cookie")

    (*("add_to_headers" >:: fun _ ->
      let _ = jar
        |> Cookiejar.add_to_headers uri (Header.init ())
        |> Header.iter (fun s ls ->
          Printf.printf("%s: %s") s @@ List.fold_left (^) "" ls
        ) in
      true |> assert_bool "Mismatch between the original jar and the jar generated \
        using headers")*)
  ]

  "page" >::: [
    ("forms" >:: fun _ ->
      let soup = page "index"
        |> Soup.parse
        |> Page.from_soup in
      let forms_with =
        test_selector page Page.forms_with (Page.Form) "form" "forms_with (expected count)";

      forms_with "[id=form-one]" 1;
      forms_with "[id=form-two]" 1;
      forms_with "[id=form-none]" 0;

      forms_with "form[id=form-one]" 1;
      forms_with "form[id=form-none]" 0;
      forms_with "li" 0;
      forms_with "li, form" 0;
      forms_with "li[id=form-one]" 0;

      forms_with ".noneclass" 0;

      forms_with "" 2;
      forms_with "*" 2;
      forms_with "form" 2;
      forms_with ".formclass" 2;
      forms_with "div > form" 1;
      forms_with "li ~ form" 1);
  
    ("links" >:: fun _ ->
      let soup = page "index"
        |> Soup.parse
        |> Page.from_soup in
      let links_with =
        test_selector page Page.links_with (Page.Link) "a" "links_with (expected count)" in

      links_with "[id=a-one]" 1;
      links_with "[id=a-two]" 1;
      links_with "[id=a-none]" 0;

      links_with "a[id=a-one]" 1;
      links_with "a[id=a-none]" 0;
      links_with "ul" 0;
      links_with "ul, a" 0;
      links_with "ul[id=a-one]" 0;

      links_with ".noneclass" 0;

      links_with "" 3;
      links_with "*" 3;
      links_with "[href^=https]" 1;
      links_with "[href$=.html]" 1;
      links_with "[href*=http]" 3;
      links_with "a" 3;
      links_with ".aclass" 2;
      links_with "div > a" 1;
      links_with "ul ~ a" 1);

    ("images" >:: fun _ ->
      let soup = page "index"
        |> Soup.parse
        |> Page.from_soup in
      let images_with =
        test_selector page Page.images_with (Page.Image) "a" "images_with (expected count)" in

      images_with "[id=img1]" 1;
      images_with "[id=img2]" 1;
      images_with "[id=imgnone]" 0;

      images_with "img[id=img1]" 1;
      images_with "img[id=imgnone]" 0;
      images_with "div" 0;
      images_with "div, img" 0;
      images_with "div[id=img1]" 0;

      images_with ".noneclass" 0;

      images_with "" 3;
      images_with "*" 3;
      images_with "[src^=https]" 1;
      images_with "[src$=.jpg]" 1;
      images_with "[src*=http]" 3;
      images_with "img" 3;
      images_with ".imgclass" 2;
      images_with "table > img" 1;
      images_with "div ~ img" 1);

    ("frames" >:: fun _ ->
      let soup = page "index"
        |> Soup.parse
        |> Page.from_soup in
      let frames_with =
        test_selector page Page.frames_with (Page.Frame) "a" "frames_with (expected count)" in

      frames_with "[id=frame-one]" 1;
      frames_with "[id=frame-two]" 1;
      frames_with "[id=frame-none]" 0;

      frames_with "frame[id=frame-one]" 1;
      frames_with "frame[id=frame-none]" 0;
      frames_with "div" 0;
      frames_with "div, frame" 0;
      frames_with "div[id=frame1]" 0;

      frames_with ".noneclass" 0;

      frames_with "" 2;
      frames_with "*" 2;
      frames_with "[src^=https]" 1;
      frames_with "[src$=.html]" 1;
      frames_with "[src*=http]" 2;
      frames_with "frame" 2;
      frames_with ".frameclass" 2;
      frames_with "table > frame" 1;
      frames_with "div ~ frame" 1)
  ]
]

let _ =
  suites |> List.iter run_test_tt_main
