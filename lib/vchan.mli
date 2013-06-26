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

(** Client and server interface for Xen's vchan protocol. *)

module type PAGE_ALLOCATOR = sig
  val get: int -> Cstruct.buffer
  (** [get n] shall allocate a memory block of size n pages *)

  val to_pages: Cstruct.buffer -> Cstruct.buffer list
  (** [to_pages block] shall return a list of [n] blocks of size one
      page each. *)
end
(** Type of a page allocator. *)

module Make : functor(Io_page: PAGE_ALLOCATOR) -> sig

  type t
  (** Type of a vchan handler. *)

  (** Type of the state of a connection between a vchan client and
      server. *)
  type state =
    | Exited (** when one side has called [close] or crashed *)
    | Connected (** when both sides are open *)
    | WaitingForConnection (** (server only) where no client has yet connected *)

  exception Not_connected of state
  (** Exception raised when trying to use a handler that is not
      currently connected to an endpoint. *)

  val server : domid:int -> xs_path:string -> read_size:int
    -> write_size:int -> allow_reconnection:bool -> t Lwt.t
  (** [server ~domid ~xs_path ~read_size ~write_size
      ~allow_reconnection] initializes a vchan server listening to
      connections from domain [~domid], using connection information
      from [~xs_path], with left ring of size [~read_size] and right
      ring of size [~write_size], which accepts reconnections
      depending on the value of [~allow_reconnection]. *)

  val client : domid:int -> xs_path:string -> t Lwt.t
  (** [client ~domid ~xs_path] initializes a vchan client to
      communicate with domain [~domid] using connection information
      from [~xs_path]. *)

  val close : t -> unit Lwt.t
  (** Close a vchan. This deallocates the vchan and attempts to free
      its resources. The other side is notified of the close, but can
      still read any data pending prior to the close. *)

  val read : t -> int -> string Lwt.t
  (** [read count vch] read at most [count] characters from [vch]. It
      returns [""] if insufficient data is available. *)

  val read_into : t -> string -> int -> int -> int Lwt.t
  (** [read_into vch buf off len] reads up to [len] bytes, stores them
      in [buf] at offset [off], and returns the number of bytes
      read. *)

  val read_into_exactly : t -> string -> int -> int -> unit Lwt.t
  (** [read_into_exactly vch buf off len] reads exactly [len] bytes from
      [vch] and stores them in [buf] at offset [off].

      Raises [End_of_file] if insufficient data is available. *)

  val write : t -> string -> unit Lwt.t
  (** [write vch buf] writes [buf] to the ring if enough space is
      available, or do not write anything and raises [End_of_file]
      otherwise. *)

  val write_from : t -> string -> int -> int -> int Lwt.t
  (** [write_from vch buf off len] writes up to [len] bytes of [buf]
      starting at [off] to [vch] and returns the number of bytes
      actually written. *)

  val write_from_exactly : t -> string -> int -> int -> unit Lwt.t
  (** [write_from_exactly vch buf off len] writes exactly [len] bytes
      to [vch] from buffer [buf] at offset [off] if enough space is
      available, or do not write anything and raises [End_of_file]
      otherwise. *)

  val state : t -> state
  (** [state vch] is the state of a vchan connection. *)

  val data_ready : t -> int
  (** [data_ready vch] is the amount of data ready to be read on [vch],
      in bytes. *)

  val buffer_space : t -> int
  (** [buffer_space vch] is the amount of data it is currently possible
      to send on [vch]. *)

end
