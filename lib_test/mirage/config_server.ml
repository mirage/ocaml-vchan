open Mirage

let main = foreign ~packages:[package "vchan-xen"; package "duration"] "Unikernel.Server" (console @-> job)

let () =
  register "vchan_server" [ main $ default_console ]
