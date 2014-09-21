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


module V = Vchan.In_memory
let sleep_time = 0.2

let () =
  let module Check_flow_compatible(F: V1_LWT.FLOW) = struct end in
  let module Test = Check_flow_compatible(V) in
  ()

let port = "test"

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
      let server_t = V.server ~domid:1 ~port ~read_size ~write_size () in
      let client_t = V.client ~domid:0 ~port () in
      server_t >>= fun server ->
      client_t >>= fun client ->
      V.close client >>= fun () ->
      V.close server >>= fun () ->
      Lwt_unix.sleep sleep_time
    );
    V.assert_cleaned_up ()
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

let with_connection read_size write_size f =
  let server_t = V.server ~domid:1 ~port ~read_size ~write_size () in
  let client_t = V.client ~domid:0 ~port () in
  server_t >>= fun server ->
  client_t >>= fun client ->
  Lwt.catch
    (fun () ->
       f client server >>= fun x ->
       V.close client >>= fun () ->
       V.close server >>= fun () ->
       return x
    ) (fun e ->
       Printf.fprintf stderr "client = %s\n%!"
         (Sexplib.Sexp.to_string_hum (V.sexp_of_flow client));
       Printf.fprintf stderr "server = %s\n%!"
         (Sexplib.Sexp.to_string_hum (V.sexp_of_flow server));
       fail e
    )

let test_write_read (read_size, write_size) =
  Printf.sprintf "read_size = %d; write_size = %d" read_size write_size
  >:: (fun () ->
    Lwt_main.run (
      with_connection read_size write_size
        (fun client server ->
          V.write server (cstruct_of_string "hello") >>|= fun () ->
          V.read client >>|= fun buf ->
          assert_equal ~printer:(fun x -> x) "hello" (string_of_cstruct buf);
          V.write client (cstruct_of_string "vchan world") >>|= fun () ->
          V.read server >>|= fun buf ->
          assert_equal ~printer:(fun x -> x) "vchan world" (string_of_cstruct buf);
          Lwt_unix.sleep sleep_time
        )
    );
    V.assert_cleaned_up ()
    )

let test_read_write (read_size, write_size) =
  Printf.sprintf "read_size = %d; write_size = %d" read_size write_size
  >:: (fun () ->
    Lwt_main.run (
      with_connection read_size write_size
        (fun client server ->
          let read_t = V.read client in
          V.write server (cstruct_of_string "hello") >>|= fun () ->
          read_t >>|= fun buf ->
          assert_equal ~printer:(fun x -> x) "hello" (string_of_cstruct buf);
          let read_t = V.read server in
          V.write client (cstruct_of_string "vchan world") >>|= fun () ->
          read_t >>|= fun buf ->
          assert_equal ~printer:(fun x -> x) "vchan world" (string_of_cstruct buf);
          Lwt_unix.sleep sleep_time
        )
    );
    V.assert_cleaned_up ()
  )


let test_write_wraps () = Lwt_main.run (
  let size = 4096 in (* guaranteed to be exact via grant refs *)
  with_connection size size
    (fun client server ->
      (* leave 2 bytes free at the end of the ring *)
      let ring = Cstruct.create (size - 2) in
      for i = 0 to Cstruct.len ring - 1 do Cstruct.set_char ring i 'X' done;
      V.write server ring >>|= fun () ->
      V.read client >>|= fun buf ->
      (* writing and reading 1 byte will ensure we have consumed the previous chunk
         (read doesn't perform a copy, see ack_up_to) *)
      V.write server (cstruct_of_string "!") >>|= fun () ->
      V.read client >>|= fun buf ->
      assert_equal ~printer:(fun x -> x) "!" (string_of_cstruct buf);
      (* there's 1 byte free before wraparound *)
      V.write server (cstruct_of_string "hello") >>|= fun () ->
      V.read client >>|= fun buf' ->
      assert_equal ~printer:(fun x -> x) "h" (string_of_cstruct buf');
      V.read client >>|= fun buf'' ->
      assert_equal ~printer:(fun x -> x) "ello" (string_of_cstruct buf'');
      Lwt_unix.sleep sleep_time
    )
); V.assert_cleaned_up ()

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
