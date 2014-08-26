This is an implementation of the Xen "libvchan" or "vchan" communication
protocol in OCaml. It allows fast inter-domain communication using shared
memory.

To use the command-line
-----------------------

Make sure your systems ae properly configured. You may need to:
```
sudo modprobe xen-evtchn
sudo modprobe xen-gntdev
sudo modprobe xen-gntalloc
mount -t xenfs xenfs /proc/xen
```

On both of your VMs, find their domain ids:
```
xenstore-read domid
```

On the domain with domid <server domid>, listen for a single connection from
<client domid> on port <port>:
```
xencat -l <client domid> <port>
```

On the domain with domid <client domid>, connect to <server domid>:
```
xencat <server domid> <port>
```
