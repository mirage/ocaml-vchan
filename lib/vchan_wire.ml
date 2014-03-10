(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

(* left is client write, server read
   right is client read, server write *)

(* matches xen/include/public/io/libxenvchan.h:ring_shared *)
cstruct ring_shared {
  uint32_t cons;
  uint32_t prod
} as little_endian

(* matches xen/include/public/io/libxenvchan.h:vchan_interface *)
cstruct vchan_interface {
  uint8_t left[8];  (* ring_shared *)
  uint8_t right[8]; (* ring_shared *)
  uint16_t left_order;
  uint16_t right_order;
  uint8_t cli_live;
  uint8_t srv_live;
  uint8_t cli_notify;
  uint8_t srv_notify
} as little_endian

let get_ro v = get_vchan_interface_right_order v
let get_lo v = get_vchan_interface_left_order v
let get_lp v = get_ring_shared_prod (get_vchan_interface_left v)
let set_lp v = set_ring_shared_prod (get_vchan_interface_left v)
let get_rp v = get_ring_shared_prod (get_vchan_interface_right v)
let set_rp v = set_ring_shared_prod (get_vchan_interface_right v)
let get_lc v = get_ring_shared_cons (get_vchan_interface_left v)
let set_lc v = set_ring_shared_cons (get_vchan_interface_left v)
let get_rc v = get_ring_shared_cons (get_vchan_interface_right v)
let set_rc v = set_ring_shared_cons (get_vchan_interface_right v)


