[![Build Status](https://travis-ci.org/mirage/ocaml-vchan.svg?branch=master)](https://travis-ci.org/mirage/ocaml-vchan)
[![Coverage Status](https://coveralls.io/repos/mirage/ocaml-vchan/badge.svg?branch=master)](https://coveralls.io/r/mirage/ocaml-vchan?branch=master)

This is an implementation of the Xen "libvchan" or "vchan" communication
protocol in OCaml. It allows fast inter-domain communication using shared
memory.

Linux configuration
-------------------

Make sure your systems are properly configured. You may need to:
```
sudo modprobe xen-evtchn
sudo modprobe xen-gntdev
sudo modprobe xen-gntalloc
mount -t xenfs xenfs /proc/xen
```

To use in Linux
---------------

To connect as a server to a client with domid 'domid' and using
the string 'port' to denote the connection:

```
open Vchan_lwt_unix

open_server ~domid ~port ()
>>= fun (ic, oc) ->
Lwt_io.write_line oc "hello"
>>= fun () ->
Lwt_io.flush oc
>>= fun () ->
Lwt_io.close ic
>>= fun () ->
Lwt_io.close oc
```

To connect as a client, replace ```open_server``` with ```open_client```.

To use the command-line
-----------------------


On both of your VMs, find their domain ids:
```
xenstore-read domid
```

On the domain with domid ```<server domid>```, listen for a single connection from
```<client domid>``` on ```<port>```:
```
xencat -l <client domid> <port>
```

On the domain with domid ```<client domid>```, connect to ```<server domid>```:
```
xencat <server domid> <port>
```

So to transfer a file ```foo``` from domid 1 to domid 2:

On domain 2, listen for the connection and retrieve the file:

```
xencat -l 1 foo > copy-of-foo
```

On domain 1, transmit the file:
```
cat foo | xencat 2 foo
```

