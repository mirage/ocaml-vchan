To run the tests:

0. Pre-reqs: make sure you have Xen (obviously!).

I notice that on Ubuntu 20.04 LTS the packages are broken and I have to run manually:
```
/usr/sbin/xenconsoled
xenstore-write domid 0
```

For xen-4.10+ the .xl file has replaced the `builder` key with a `type`. For released versions
of Mirage you can support this with
```
opam pin add mirage git://github.com/djs55/mirage#3.7.7-type-pv
```

1. Build and setup everything:
```
./setup.sh
```

2. Run a server
```
sudo xl create -c server/vchan_server.xl
```

3. Run a client
```
sudo xl create -c client/vchan_client.xl
```

4. Observe on the server console
```
Server initialising
starting wait
safe: reading /conduit/foo_server
readdir restarting
starting wait
safe: reading /conduit/foo_server
readdir restarting
starting wait
safe: reading /conduit/foo_server
readdir restarting
starting wait
safe: reading /conduit/foo_server
readdir returning foo_client
found a name foo_client!
safe: reading /conduit/foo_server/foo_client
readdir returning flibble
port flibble
remote domid is 5 and port is flibble
creating server
num is 0
num is 1
num is 2
num is 3
```
These `num is ..` are strings sent from the client.

