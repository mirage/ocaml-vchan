## v5.0.0 (2019-11-01)
* Adapt to mrage-xen 5.0.0 changes (#134 @hannesm)
* Adapt to mirage-flow 2.0.0 changes (#134 @hannesm)

## v4.0.3 (2019-06-27)

* Use mirage-xen 4.0.0 `Os_xen` interface (#128 @TheLortex)
* Update opam metadata to remove `{build}` for ppx (@avsm)

## v4.0.2 (2019-03-25)
* Use new OS.Xen grant API (#125 @talex5)
* Expose type port = Vchan.Port.t in Unix API (#124 @talex5)

## v4.0.1 (2019-02-08)
* Use `io-page-unix` dependency instead of the deprecated
  `io-page.unix` alias (@avsm)

## 4.0.0 (2018-12-11)
* Modernise build for OCaml 4.04 and port to Dune (#120 @talex5 @avsm)
* Upgrade opam metadata files to 2.0 (#120 @avsm @talex5)
* Remove mli only module (#114 @rgrinberg @samoht)
* Fix deprecation warning on String.create (#112 @samoht)
* Make OCaml 4.04 the minimum version (#111 @cfcs @hannesm)
* Update test cases for Mirage 3 (#110 @mneilsen)
 
## 3.0.0 (2017-06-08)
* Split into 3 opam packages:
  * vchan: the platform-independent protocol code
  * vchan-unix: the Unix user-space implementation
  * vchan-xen: the Xen kernel implementation
* Switch to jbuilder and topkg

## 2.3.1 (2017-06-02)
* Depend on new xen-{gnt,evtchn}-unix
* Modernize Dockerfile and travis configuration

## 2.3.0 (2017-01-24)
* add archlinux dependencies
* build against mirageos version 3, and drop support for earlier versions

## 2.2.0 (2016-07-21)
* remove ppx as a runtime dependency
* configure supports --{en,dis}able-xen{,ctrl} arguments
* supports but does not require FLOW.disconnect

## 2.1.0 (2016-04-07)
* now requires cstruct.1.9.0 and OCaml 4.02 for ppx

## 2.0.3 (2015-09-03)
* ounit is now a test dependency only
* add coverage testing via travis and coveralls.io
* doc: automatically upload the API docs to gh-pages
* add js stubs
* depend on lwt >= 2.5.0

## 2.0.2 (2015-01-27)
* Support the `io-page` 1.3.0 interface.

## 2.0.1 (2015-01-23)
* add an `error_message` function to convert a Vchan error
  into a human-readable string. (#60)
* Improve error messages from the `configure` output (#61).
* Use modern centrally sourced Travis script for OPAM 1.2.

## 2.0.0 (2014-11-03)
* add `Vchan_lwt_unix` with instantiation of functor
* make Vchan.Port.t abstract (previously was a string)
* use the same Xenstore path convention as `libxenvchan.h`
* support channel closing, `Eof etc
* define an `ENDPOINT` signature for a Vchan.

## 1.0.0 (2014-07-15)
* test VM: uses the V1_LWT.FLOW signature

## 0.9.7 (2014-06-18)
* cli: server: choose a sensible default xenstore path
* cli: server: set the xenstore permissions correctly
* cli: client: don't assume we have perms to read the directory
* Implement Mirage V1_LWT.FLOW signature

## 0.9.6 (2014-06-14)
* we should depend on mirage-types.lwt, not mirage

## 0.9.5 (2014-06-14)
* build the CLI by default

## 0.9.4 (2014-04-29):
* Update to mirage-1.1.0.

## 0.9.3 (2013-10-09):
* Fix an overflow in a client read from the vchan buffer.

## 0.9.2 (2013-10-02):
* Add Travis continuous integration scripts.
* Add explicit dependency on OCaml 4.00+

## 0.9.1 (2013-09-27):
* Remove 'blocking' parameter

## 0.9 (2013-08-23):
* Initial public release.
