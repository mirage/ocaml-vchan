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

module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
module M = Vchan.Make(Unix_activations)(Xs)

(* Delete when I've got a working Cohttp *)
module type Cohttp_IO_S = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  type ic
  type oc

  val iter : ('a -> unit t) -> 'a list -> unit t
  val read_line : ic -> string option t
  val read : ic -> int -> string t
  val read_exactly : ic -> int -> string option t

  val write : oc -> string -> unit t
  val flush : oc -> unit t
end

module IO = struct
  type 'a t = 'a Lwt.t
  let ( >>= ) = Lwt.( >>= )
  let return = Lwt.return

  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel

  let iter = Lwt_list.iter_s

  let read_line = Lwt_io.read_line_opt

  let read ic count =
    Lwt.catch (fun () -> Lwt_io.read ~count ic)
       (function End_of_file -> return ""
        | e -> Lwt.fail e)

  let read_exactly ic buf off len =
    Lwt.catch (fun () -> Lwt_io.read_into_exactly ic buf off len >>= fun () ->  return true)
      (function End_of_file -> return false
       | e -> Lwt.fail e)

  let read_exactly ic len =
    let buf = String.create len in
    read_exactly ic buf 0 len >>= function
      | true -> return (Some buf)
      | false -> return None

  let write = Lwt_io.write

  let write_line = Lwt_io.write_line

  let flush = Lwt_io.flush

end
