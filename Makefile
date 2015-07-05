.PHONY: all clean install build
all: build doc

NAME=vchan
J=4

export OCAMLRUNPARAM=b

-include config.mk

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure $(ENABLE_TESTS) $(ENABLE_XENCTRL) $(ENABLE_XEN)

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

install: setup.bin
	@./setup.bin -install

test:
	@rm -f setup.data
	@make ENABLE_TESTS=--enable-tests build
	@./test.native -runner sequential

reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall

uninstall:
	@ocamlfind remove $(NAME) || true

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin

gh-pages:
	bash .docgen.sh

coverage:
	bash .coverage.sh

JS_DIR ?= $(shell ocamlfind query vchan)

PHONY: js-install js-uninstall
js-install:
	install -m 0644 js/vchan.js $(JS_DIR)

js-uninstall:
	rm -f $(JS_DIR)/vchan.js
