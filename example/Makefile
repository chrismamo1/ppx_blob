SOURCES = quine.ml

#Uncomment this, and remove -ppx ../ppx_netblob below to use the installed version
#PACKS = ppx_netblob
PACKS = cohttp.lwt ppx_deriving ppx_deriving_yojson ppx_netblob

RESULT = quine

OCAMLPPXFLAGS = -ppx ../ppx_netblob

OCAMLNCFLAGS = -g -w -3 $(OCAMLPPXFLAG) -linkall
OCAMLBCFLAGS = -g -w -3 $(OCAMLPPXFLAG) -linkall
OCAMLLDFLAGS = -g

all : native-code

-include OCamlMakefile

