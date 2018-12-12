
.PHONY: build clean test

build:
	dune build @install 

test:
	dune runtest 

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build

IMAGE?=ocaml-vchan

xen-depends: Dockerfile
	docker build -t $(IMAGE) .

xen-build: xen-depends clean
	docker run -v $(shell pwd):/src $(IMAGE) opam install vchan -y

JS_DIR ?= $(shell ocamlfind query vchan)

PHONY: js-install js-uninstall
js-install:
	install -m 0644 js/vchan.js $(JS_DIR)

js-uninstall:
	rm -f $(JS_DIR)/vchan.js
