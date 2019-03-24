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

type grant = int32 [@@deriving sexp]

let grant_of_int32 x = x
let int32_of_grant x = x

type page = Io_page.t
let sexp_of_page _ = Sexplib.Sexp.Atom "<buffer>"

type share = {
  grants: grant list;
  mapping: page;
} [@@deriving sexp_of]

let grants_of_share x = x.grants
let buf_of_share x = x.mapping

let gntshr_interface_open =
  let cache = ref None in
  fun () -> match !cache with
  | None ->
    let i = Gnt.Gntshr.interface_open () in
    cache := Some i;
    i
  | Some i -> i

let share ~domid ~npages ~rw =
  let i = gntshr_interface_open () in
  let s = Gnt.Gntshr.share_pages_exn i domid npages rw in
  { grants = List.map Int32.of_int s.Gnt.Gntshr.refs; mapping = s.Gnt.Gntshr.mapping }

let unshare s =
  let i = gntshr_interface_open () in
  let s' = { Gnt.Gntshr.refs = List.map Int32.to_int s.grants; mapping = s.mapping } in
  Gnt.Gntshr.munmap_exn i s';
  Lwt.return ()

let gnttab_interface_open =
  let cache = ref None in
  fun () -> match !cache with
  | None ->
    let i = Gnt.Gnttab.interface_open () in
    cache := Some i;
    i
  | Some i -> i

type page' = Gnt.Gnttab.Local_mapping.t
let sexp_of_page' = sexp_of_page

type mapping = {
  mapping: page';
  grants: (int * int32) list;
} [@@deriving sexp_of]

let buf_of_mapping m = Gnt.Gnttab.Local_mapping.to_buf m.mapping

let map ~domid ~grant ~rw =
  let i = gnttab_interface_open () in
  let mapping = Gnt.Gnttab.map_exn i { Gnt.Gnttab.domid; ref = Int32.to_int grant } rw in
  { mapping; grants = [ domid, grant ] }

let mapv ~grants ~rw =
  let i = gnttab_interface_open () in
  let mapping = Gnt.Gnttab.mapv_exn i (List.map (fun (domid, gntref) -> { Gnt.Gnttab.domid; ref = Int32.to_int gntref }) grants) rw in
  { mapping; grants }

let unmap m =
  let i = gnttab_interface_open () in
  Gnt.Gnttab.unmap_exn i m.mapping
