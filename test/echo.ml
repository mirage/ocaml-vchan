open V1_LWT

let (>>=) = Lwt.bind

let (>>|=) m f = m >>= function
| `Ok x -> f x
| `Eof -> Lwt.fail (Failure "End of file")
| `Error (`Not_connected state) -> Lwt.fail (Failure (Printf.sprintf "Not in a connected state: %s" (Sexplib.Sexp.to_string (Node.V.sexp_of_state state))))

module Main (C:CONSOLE) = struct
  let buf = String.create 5000

  let rec echo vch =
    Node.V.read vch >>|= fun input_line ->
    let line = String.uppercase (Cstruct.to_string input_line) in
    let buf = Cstruct.create (String.length line) in
    Cstruct.blit_from_string line 0 buf 0 (String.length line);
    Node.V.write vch buf >>|= fun () ->
    echo vch

  let start c =
    lwt () = Node.with_vchan Node.Server (Eventchn.init ()) 0 "data/vchan" echo in
    Lwt.return ()

end
