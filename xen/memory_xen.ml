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

module Gntref = OS.Xen.Gntref
module Export = OS.Xen.Export
module Import = OS.Xen.Import

type grant = Gntref.t

let grant_of_sexp x = Gntref.of_int32 @@ [%of_sexp : int32] x
let sexp_of_grant x = [%sexp_of : int32] (Gntref.to_int32 x)

let grant_of_int32 = Gntref.of_int32
let int32_of_grant = Gntref.to_int32

type page = Io_page.t
let sexp_of_page _ = Sexplib.Sexp.Atom "<buffer>"

type share = Export.t

type share_info = {
  grants: grant list;
  mapping: page;
} [@@deriving sexp_of]

let grants_of_share = Export.refs
let buf_of_share = Export.mapping

let sexp_of_share share =
  [%sexp_of : share_info] { grants = grants_of_share share; mapping = buf_of_share share }

let share ~domid ~npages ~rw =
  Export.share_pages_exn ~domid ~count:npages ~writable:rw

let unshare = Export.unshare ~release_refs:true

type mapping = Import.Local_mapping.t
let sexp_of_mapping = sexp_of_page

let buf_of_mapping m = Import.Local_mapping.to_buf m

let map ~domid ~grant ~rw =
  Import.map_exn { Import.domid; ref = grant } ~writable:rw

let mapv ~grants ~rw =
  Import.mapv_exn (List.map (fun (domid, gntref) -> { Import.domid; ref = gntref }) grants) ~writable:rw

let unmap = Import.Local_mapping.unmap_exn
