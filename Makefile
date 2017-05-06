META: pkg/META.in
	cp pkg/META.in pkg/META

native-code: META
	ocaml pkg/build.ml native=true native-dynlink=true

byte-code: META
	ocaml pkg/build.ml native=false

build: native-code

derive = $(shell ocamlfind query ppx_deriving.show)
dry_run: build
	ocamlfind ppx_tools/rewriter "ocamlfind ppx_deriving/ppx_deriving ${derive}/ppx_deriving_show.cma /home/chrismamo1/.opam/4.04.0/lib/ppx_deriving_yojson/ppx_deriving_yojson.cma _build/ppx_netblob.cma" example/quine.ml

test: build example/example.ml
	rm -rf _build/src_test/
	ocamlbuild -pkgs lwt,cohttp.lwt,ppx_deriving,ppx_deriving_yojson,ppx_netblob -j 0 -use-ocamlfind -classic-display \
						 example/example.native

clean:
	ocamlbuild -clean

.PHONY: build test doc clean

install: build
	ocamlfind install ppx_netblob pkg/META _build/src/*

uninstall:
	ocamlfind remove ppx_netblob

reinstall: uninstall install
