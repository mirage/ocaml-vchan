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

module M: Vchan.S.ENDPOINT with type port = Vchan.Port.t

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

module IO : Cohttp_IO_S
 with type 'a t = 'a Lwt.t
 and type ic = Lwt_io.input_channel
 and type oc = Lwt_io.output_channel

val open_client :
    domid:int -> port:Vchan.Port.t
    -> ?buffer_size:int
    -> unit
    -> (Lwt_io.input_channel * Lwt_io.output_channel) Lwt.t
(** [open_client domid port ?buffer_size ()] creates a client
    connection to the server running on [domid] with port [port].
    This call will block until communication is established and it
    is safe to pass traffic. The underlying vchan connection will
    be disconnected when the input_channel is closed.
    If a ?buffer_size is given then 4 buffers of this size will be
    created: 2 for reading (vchan + Lwt_io) and 2 for writing.
 *)

val open_server :
    domid: int -> port:Vchan.Port.t
    -> ?buffer_size:int
    -> unit
    -> (Lwt_io.input_channel * Lwt_io.output_channel) Lwt.t
(** [open_server domid port ?buffer_size ()] creates a server
    connection to client [domid] with port [port]. If a ?buffer_size
    argument is given then 4 buffers of this size will be created:
    2 for reading (vchan + Lwt_io) and 2 for writing. *)

