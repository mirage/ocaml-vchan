(*
 * Copyright (c) 2013,2014 Citrix Systems Inc
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

module type CONFIGURATION = sig

  type t = {
    ring_ref: string;
    event_channel: string;
  } with sexp

  val write:
     client_domid:int -> port:Port.t
  -> t
  -> unit Lwt.t

  val read:
     server_domid:int -> port:Port.t
  -> t Lwt.t

  val delete:
     client_domid:int -> port:Port.t
  -> unit Lwt.t

end

module type MEMORY = sig
  type grant with sexp

  val grant_of_int32: int32 -> grant
  val int32_of_grant: grant -> int32

  type share with sexp_of

  val grants_of_share: share -> grant list
  val buf_of_share: share -> Io_page.t

  val share: domid:int -> npages:int -> rw:bool -> share

  val unshare: share -> unit

  type mapping with sexp_of

  val buf_of_mapping: mapping -> Io_page.t

  val map: domid:int -> grant:grant -> rw:bool -> mapping

  val mapv: grants:(int * grant) list -> rw:bool -> mapping

  val unmap: mapping -> unit

end

module type EVENTS = sig

  type port with sexp_of
  (** an identifier for a source of events. Ports are allocated by calls to
      [listen], then exchanged out-of-band (typically by xenstore) and
      finally calls to [connect] creates a channel between the two domains.
      Events are send and received over these channels. *)

  val port_of_string: string -> [ `Ok of port | `Error of string ]
  val string_of_port: port -> string

  type channel with sexp_of
  (** a channel is the connection between two domains and is used to send
      and receive events. *)

  type event with sexp_of
  (** an event notification received from a remote domain. Events contain no
      data and may be coalesced. Domains which are blocked will be woken up
      by an event. *)

  val initial: event
  (** represents an event which 'fired' when the program started *)

  val recv: channel -> event -> event Lwt.t
  (** [recv channel event] blocks until the system receives an event
      newer than [event] on channel [channel]. If an event is received
      while we aren't looking then this will be remembered and the
      next call to [after] will immediately unblock. If the system
      is suspended and then resumed, all event channel bindings are invalidated
      and this function will fail with Generation.Invalid *)

  val send: channel -> unit
  (** [send channel] sends an event along [channel], to another domain
      which will be woken up *)

  val listen: int -> port * channel
  (** [listen domid] allocates a fresh port and event channel. The port
      may be supplied to [connect] *)

  val connect: int -> port -> channel
  (** [connect domid port] connects an event channel to [port] on [domid] *)

  val close: channel -> unit
  (** [close channel] closes this side of an event channel *)
end

module type ENDPOINT = sig
  type t with sexp_of
  (** Type of a vchan endpoint. *)

  type error = [
    `Unknown of string
  ]

  val server :
    domid:int ->
    port:Port.t ->
    ?read_size:int ->
    ?write_size:int ->
    unit -> t Lwt.t

  val client :
    domid:int ->
    port:Port.t ->
    unit -> t Lwt.t

  val close : t -> unit Lwt.t
  (** Close a vchan. This deallocates the vchan and attempts to free
      its resources. The other side is notified of the close, but can
      still read any data pending prior to the close. *)

  include V1_LWT.FLOW
    with type flow = t
    and  type error := error
    and  type 'a io = 'a Lwt.t
    and  type buffer = Cstruct.t
end
