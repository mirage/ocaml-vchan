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


module type ACTIVATIONS = sig

(** Event channels handlers. *)

type event
(** identifies the an event notification received from xen *)

val program_start: event
(** represents an event which 'fired' when the program started *)

val after: Eventchn.t -> event -> event Lwt.t
(** [next channel event] blocks until the system receives an event
    newer than [event] on channel [channel]. If an event is received
    while we aren't looking then this will be remembered and the
    next call to [after] will immediately unblock. If the system
    is suspended and then resumed, all event channel bindings are invalidated
    and this function will fail with Generation.Invalid *)
end

module type S = sig
  type t
  (** Type of a vchan handler. *)

  (** Type of the state of a connection between a vchan client and
      server. *)
  type state =
    | Exited (** when one side has called [close] or crashed *)
    | Connected (** when both sides are open *)
    | WaitingForConnection (** (server only) where no client has yet connected *)
  with sexp

  type error = [
    `Unknown of string
  ]

  val server :
    domid:int ->
    port:Port.t ->
    read_size:int ->
    write_size:int ->
    t Lwt.t

  val client :
    domid:int ->
    port:Port.t -> t Lwt.t

  val close : t -> unit Lwt.t
  (** Close a vchan. This deallocates the vchan and attempts to free
      its resources. The other side is notified of the close, but can
      still read any data pending prior to the close. *)

  include V1_LWT.FLOW
    with type flow = t
    and  type error := error
    and  type 'a io = 'a Lwt.t
    and  type buffer = Cstruct.t

  val state : t -> state
  (** [state vch] is the state of a vchan connection. *)

  val data_ready : t -> int
  (** [data_ready vch] is the amount of data ready to be read on [vch],
      in bytes. *)

  val buffer_space : t -> int
  (** [buffer_space vch] is the amount of data it is currently possible
      to send on [vch]. *)
end