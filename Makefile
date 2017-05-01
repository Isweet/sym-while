SHELL := /bin/bash

EXE_NAME  := symwhile
TEST_NAME := mike_example

OCAMLBUILD_FLAGS := -use-ocamlfind -pkgs 'z3' -I src
OCAMLBUILD       := ocamlbuild $(OCAMLBUILD_FLAGS)

all: native byte

clean:
	$(OCAMLBUILD) -clean

native: sanity_check
	$(OCAMLBUILD) '$(EXE_NAME).native'

byte: sanity_check
	$(OCAMLBUILD) '$(EXE_NAME).byte'

sanity_check:
	which z3

test: native
	./$(EXE_NAME).native --semantics symbolic <(echo "input(x); input(y); assert not (y = 0); result := x / y")

.PHONY: all clean native byte sanity
