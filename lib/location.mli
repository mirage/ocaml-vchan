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

type offset =
  | First
  | Second
[@@deriving sexp]
(** Valid offsets within the shared page *)

val to_offset: offset -> int
(** [to_offset x] converts x to bytes *)

type t =
  | Within_shared_page of offset (** within the shared page *)
  | External of int (** in separately granted pages *)
[@@deriving sexp]
(** Location of a data ring *)

val to_length: t -> int
(** [to_length t] gives the maximum number of available bytes at [t] *)

val to_order: t -> int
(** [to_order t] gives the 'order' which is shared via the metadata
    page and used to uniquely identify the location *)

val of_order: int -> (t, [> `Msg of string ]) result
(** [of_order x] parses the order *)

val of_lengths: int -> int -> t * t
(** [of_lengths read_size write_size] chooses distinct [t1,t2] to
    accommodate buffers of size [read_size] and [write_size] *)
