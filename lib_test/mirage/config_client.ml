open Mirage

let main = foreign ~packages:[package "vchan-xen"; package "duration"] "Unikernel.Client" (console @-> job)

let () =
  register "vchan_client" [ main $ default_console ]
