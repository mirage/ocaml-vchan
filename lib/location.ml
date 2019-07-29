(*
 * Copyright (c) 2014 Citrix Systems Inc
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

type offset =
  | First
  | Second
[@@deriving sexp]
(** Valid offsets within the shared page *)

let to_offset = function
  | First -> 1024
  | Second -> 2048

type t =
  | Within_shared_page of offset
  | External of int
[@@deriving sexp]

let to_length = function
  | Within_shared_page First -> 1024
  | Within_shared_page Second -> 2048
  | External x -> 1 lsl (x + 12)

let to_order = function
  | Within_shared_page First -> 10
  | Within_shared_page Second -> 11
  | External n -> n + 12

let of_order = function
  | 10 -> Ok (Within_shared_page First)
  | 11 -> Ok (Within_shared_page Second)
  | n when n >= 12 -> Ok (External (n - 12))
  | x -> Error (`Msg (Printf.sprintf "Invalid ring order: %d" x))

(* in increasing order of bytes *)
let all = [
  Within_shared_page First; Within_shared_page Second;
  External 0; External 1; External 2; External 3; External 4;
  External 5; External 6; External 7; External 8
]

let suitable_locations requested_size =
  match List.filter (fun bl -> to_length bl >= requested_size) all with
  | []     -> External 8 (* cap at the maximum *)
  | h::_   -> h

let of_lengths read_size write_size =
  match suitable_locations read_size, suitable_locations write_size with
  (* avoid clashes for the slots in the original page by giving the
   * other buffer a free length upgrade *)
  | Within_shared_page First, Within_shared_page First ->
    Within_shared_page First, Within_shared_page Second
  | Within_shared_page Second, Within_shared_page Second ->
    Within_shared_page First, External 0
  | n                      -> n
