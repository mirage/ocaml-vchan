(*
 * Copyright (c) 2013 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib.Std

type t = string [@@deriving sexp]

let of_string x =
  let valid_char = function
    | 'a'..'z'
    | 'A'..'Z'
    | '0'..'9'
    | '_' | '-' -> true
    | _ -> false in
  let rec loop n =
    (n = String.length x)
    || (valid_char x.[n] && loop (n + 1)) in
  if loop 0 && (String.length x > 0)
  then Ok x
  else
    let msg = Printf.sprintf "A Vchan port must match [a-zA-Z0-9_-]+; \
                              therefore %S is invalid." x
    in
    Error (`Msg msg)

let to_string t = t
