open V1_LWT

let (>>=) = Lwt.bind

module Main (C:CONSOLE) = struct
  let buf = String.create 5000

  let rec echo vch =
    Node.V.read_into vch buf 0 5000 >>= fun nb_read ->
    Node.V.write_from_exactly vch buf 0 nb_read >>= fun () ->
    echo vch

  let start c =
    lwt () = Node.with_vchan Node.Server (Eventchn.init ()) 0 "data/vchan" echo in
    Lwt.return ()

end
