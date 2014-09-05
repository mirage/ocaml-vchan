(*
 * Copyright (C) Citrix Systems Inc.
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

module Config = struct
  open Lwt

  type t = {
    ring_ref: string;
    event_channel: string;
  } with sexp

  let tbl: (Vchan.Port.t, t) Hashtbl.t = Hashtbl.create 16

  let c = Lwt_condition.create ()

  let write ~client_domid ~port t =
    Hashtbl.replace tbl port t;
    return ()

  let read ~server_domid ~port =
    let rec loop () =
      if Hashtbl.mem tbl port
      then return (Hashtbl.find tbl port)
      else
        Lwt_condition.wait c >>= fun () ->
        loop () in
    loop ()

  let delete ~client_domid ~port =
    Hashtbl.remove tbl port;
    return ()

  let assert_cleaned_up () =
    if Hashtbl.length tbl <> 0 then begin
      Printf.fprintf stderr "Stale config entries in xenstore\n%!";
      failwith "stale config entries in xenstore";
    end
end

module Memory = struct
  type grant = int32 with sexp

  let grant_of_int32 x = x
  let int32_of_grant x = x

  type page = Io_page.t
  let sexp_of_page _ = Sexplib.Sexp.Atom "<buffer>"

  type share = {
    grants: grant list;
    mapping: page;
  } with sexp_of

  let grants_of_share x = x.grants
  let buf_of_share x = x.mapping

  let get =
    let g = ref Int32.zero in
    fun () ->
      g := Int32.succ !g;
      Int32.pred !g

  let rec get_n n =
    if n = 0 then [] else get () :: (get_n (n-1))

  let individual_pages = Hashtbl.create 16
  let big_mapping = Hashtbl.create 16

  let share ~domid ~npages ~rw =
    let mapping = Io_page.get npages in
    let grants = get_n npages in
    let share = { grants; mapping } in
    let pages = Io_page.to_pages mapping in
    List.iter (fun (grant, page) -> Hashtbl.replace individual_pages grant page) (List.combine grants pages);
    Hashtbl.replace big_mapping (List.hd grants) mapping;
    share

  let remove tbl key =
    if Hashtbl.mem tbl key
    then Hashtbl.remove tbl key
    else begin
      Printf.fprintf stderr "Attempt to remove non-existing mapping\n%!";
      failwith "Attempt to remove non-existing mapping"
    end

  let unshare share =
    List.iter (fun grant -> remove individual_pages grant) share.grants;
    remove big_mapping (List.hd share.grants)

  type mapping = {
    mapping: page;
    grants: (int * int32) list;
  } with sexp_of

  let buf_of_mapping x = x.mapping

  let currently_mapped = Hashtbl.create 16

  let map ~domid ~grant ~rw:_ =
    let mapping = Hashtbl.find individual_pages grant in
    if Hashtbl.mem currently_mapped grant then begin
      Printf.fprintf stderr "map: grant %ld is already mapped\n%!" grant;
      failwith (Printf.sprintf "map: grant %ld is already mapped" grant);
    end;
    Hashtbl.replace currently_mapped grant ();
    { mapping; grants = [ domid, grant ] }

  let mapv ~grants ~rw:_ =
    if grants = [] then begin
      Printf.fprintf stderr "mapv called with empty grant list\n%!";
      failwith "mapv: empty list"
    end;
    let first = snd (List.hd grants) in
    let mapping = Hashtbl.find big_mapping first in
    if Hashtbl.mem currently_mapped first then begin
      Printf.fprintf stderr "mapv: grant %ld is already mapped\n%!" first;
      failwith (Printf.sprintf "mapv: grant %ld is already mapped" first);
    end;
    Hashtbl.replace currently_mapped first ();
    { mapping; grants }

  let unmap { mapping; grants } =
    let first = snd (List.hd grants) in
    if Hashtbl.mem currently_mapped first
    then Hashtbl.remove currently_mapped first
    else begin
      Printf.fprintf stderr "unmap called with already-unmapped grant\n%!";
      failwith "unmap: already unmapped"
    end

  let assert_cleaned_up () =
    if Hashtbl.length currently_mapped <> 0 then begin
      Printf.fprintf stderr "Some grants are still mapped in\n%!";
      failwith "some grants are still mapped in"
    end;
    if Hashtbl.length big_mapping <> 0 then begin
      Printf.fprintf stderr "Some grants are still active\n%!";
      failwith "some grants are still active"
    end
end

module Events = struct
  open Lwt

  type port = int with sexp_of

  let port_of_string x = `Ok (int_of_string x)
  let string_of_port = string_of_int

  type channel = int with sexp_of
  let get =
    let g = ref 0 in
    fun () ->
      incr g;
      !g - 1

  type event = int with sexp_of
  let initial = 0

  let channels = Array.make 1024 0
  let c = Lwt_condition.create ()

  let rec recv channel event =
    if channels.(channel) > event
    then return channels.(channel)
    else
      Lwt_condition.wait c >>= fun () ->
      recv channel event

  let connected_to = Array.make 1024 (-1)

  let send channel =
    let listening = connected_to.(channel) in
    if listening = -1 then begin
      Printf.fprintf stderr "send: event channel %d is closed\n%!" channel;
      failwith (Printf.sprintf "send: event channel %d is closed" channel);
    end;
    channels.(listening) <- channels.(listening) + 1;
    Lwt_condition.broadcast c ()

  let listen _ =
    let port = get () in
    port, port

  let connect _ port =
    let port' = get () in
    connected_to.(port') <- port;
    connected_to.(port) <- port';
    port'

  let close port =
    channels.(port) <- 0;
    connected_to.(port) <- -1

  let assert_cleaned_up () =
    for i = 0 to Array.length connected_to - 1 do
      if connected_to.(i) <> (-1) then begin
        Printf.fprintf stderr "Some event channels are still connected\n%!";
        failwith "some event channels are still connected"
      end
    done
end

let assert_cleaned_up () =
  Memory.assert_cleaned_up ();
  Config.assert_cleaned_up ();
  Events.assert_cleaned_up ()

module V = Vchan.Connection.Make(Events)(Memory)(Config)

let () =
  let module Check_flow_compatible(F: V1_LWT.FLOW) = struct end in
  let module Test = Check_flow_compatible(V) in
  ()

let port = match Vchan.Port.of_string "test" with
| `Error _ -> failwith "Failed to parse test port"
| `Ok x -> x

open Lwt

let setify x =
  let rec loop acc = function
  | [] -> acc
  | x :: xs -> loop (if List.mem x acc then acc else x :: acc) xs in
  loop [] x

let interesting_buffer_sizes =
  (* These are the cases which we know trigger different behaviour *)
  let core = [
    1024, 1024;
    1024, 2048;
    2048, 2048;
    4096, 4096;
  ] in
  let all = core in
  (* and the same again, flipped *)
  let all = all @ (List.map (fun (x, y) -> y, x) all) in
  (* and the same again, off-by-one *)
  let all = all @ (List.map (fun (x, y) -> x-1, y-1) all) in
  setify all

open OUnit

let test_connect (read_size, write_size) =
  Printf.sprintf "read_size = %d; write_size = %d" read_size write_size
  >:: (fun () ->
    Lwt_main.run (
      let server_t = V.server ~domid:1 ~port ~read_size ~write_size in
      let client_t = V.client ~domid:0 ~port in
      server_t >>= fun server ->
      client_t >>= fun client ->
      V.close client >>= fun () ->
      V.close server
    );
    assert_cleaned_up ()
  )

let (>>|=) m f = m >>= function
| `Ok x -> f x
| `Error (`Unknown x) -> fail (Failure x)
| `Eof -> fail (Failure "EOF")

let cstruct_of_string s =
  let cstr = Cstruct.create (String.length s) in
  Cstruct.blit_from_string s 0 cstr 0 (String.length s);
  cstr
let string_of_cstruct c = String.escaped (Cstruct.to_string c)

let test_write_read (read_size, write_size) =
  Printf.sprintf "read_size = %d; write_size = %d" read_size write_size
  >:: (fun () ->
    Lwt_main.run (
      let server_t = V.server ~domid:1 ~port ~read_size ~write_size in
      let client_t = V.client ~domid:0 ~port in
      server_t >>= fun server ->
      client_t >>= fun client ->
      V.write server (cstruct_of_string "hello") >>|= fun () ->
      V.read client >>|= fun buf ->
      try 
        assert_equal ~printer:(fun x -> x) "hello" (string_of_cstruct buf);
        V.close client >>= fun () ->
        V.close server
      with e ->
        Printf.fprintf stderr "client = %s\n%!" (Sexplib.Sexp.to_string_hum (V.sexp_of_t client));
        Printf.fprintf stderr "server = %s\n%!" (Sexplib.Sexp.to_string_hum (V.sexp_of_t server));
        raise e
    );
    assert_cleaned_up ()
  )

let test_read_write (read_size, write_size) =
  Printf.sprintf "read_size = %d; write_size = %d" read_size write_size
  >:: (fun () ->
    Lwt_main.run (
      let server_t = V.server ~domid:1 ~port ~read_size ~write_size in
      let client_t = V.client ~domid:0 ~port in
      server_t >>= fun server ->
      client_t >>= fun client ->
      let read_t = V.read client in
      V.write server (cstruct_of_string "hello") >>|= fun () ->
      read_t >>|= fun buf ->
      try 
        assert_equal ~printer:(fun x -> x) "hello" (string_of_cstruct buf);
        V.close client >>= fun () ->
        V.close server
      with e ->
        Printf.fprintf stderr "client = %s\n%!" (Sexplib.Sexp.to_string_hum (V.sexp_of_t client));
        Printf.fprintf stderr "server = %s\n%!" (Sexplib.Sexp.to_string_hum (V.sexp_of_t server));
        raise e
    );
    assert_cleaned_up ()
  )


let test_write_wraps () = Lwt_main.run (
  let size = 4096 in (* guaranteed to be exact via grant refs *)
  let server_t = V.server ~domid:1 ~port ~read_size:size ~write_size:size in
  let client_t = V.client ~domid:0 ~port in
  server_t >>= fun server ->
  client_t >>= fun client ->
  (* leave 2 bytes free at the end of the ring *)
  let ring = Cstruct.create (size - 2) in
  for i = 0 to Cstruct.len ring - 1 do Cstruct.set_char ring i 'X' done;
  V.write server ring >>|= fun () ->
          Printf.fprintf stderr "written %d\n%!" (size - 1);
  V.read client >>|= fun buf ->
          Printf.fprintf stderr "read %d\n%!" (size - 1);
  (* writing and reading 1 byte will ensure we have consumed the previous chunk
     (read doesn't perform a copy, see ack_up_to) *)
  V.write server (cstruct_of_string "!") >>|= fun () ->
  V.read client >>|= fun buf ->
  assert_equal ~printer:(fun x -> x) "!" (string_of_cstruct buf);
  (* there's 1 byte free before wraparound *)
  V.write server (cstruct_of_string "hello") >>|= fun () ->
          Printf.fprintf stderr "written hello\n%!";
  V.read client >>|= fun buf' ->
  assert_equal ~printer:(fun x -> x) "h" (string_of_cstruct buf');
          Printf.fprintf stderr "h\n%!";
  V.read client >>|= fun buf'' ->
  assert_equal ~printer:(fun x -> x) "ello" (string_of_cstruct buf'');
          Printf.fprintf stderr "ello\n%!";
  V.close client >>= fun () ->
  V.close server
); assert_cleaned_up ()

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test vchan protocol code";

  let suite = "vchan" >::: [
    "connect" >::: (List.map test_connect interesting_buffer_sizes);
    "write_read" >::: (List.map test_write_read interesting_buffer_sizes);
    "read_write" >::: (List.map test_read_write interesting_buffer_sizes);
    "test_write_wraps" >:: test_write_wraps;
  ] in
  run_test_tt ~verbose:!verbose suite

