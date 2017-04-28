USE_OPT = 1

EXE := sym-while

OCAMLFIND := ocamlfind
OCAMLLEX  := ocamllex
OCAMLYACC := ocamlyacc

ifdef USE_OPT
	OEXT     := cmx
	IEXT     := cmi
	LOEXT    := cmxa
	OCAMLC   := $(OCAMLFIND) ocamlopt -g
	OCAMLDEP := $(OCAMLFIND) ocamldep -native
else
	OEXT     := cmo
	IEXT     := cmi
	LOEXT    := cma 
	OCAMLC   := $(OCAMLFIND) ocamlc
	OCAMLDEP := $(OCAMLFIND) ocamldep
endif

OBJS_EXE := \
	lexer.$(OEXT) \
	ast.$(OEXT) \
	parser.$(OEXT) \
	symbol.$(OEXT) \
	concrete.$(OEXT) \
	symbolic.$(OEXT) \
	main.$(OEXT) \

sym-while: $(OBJS_EXE)
	$(OCAMLC) $(LINK_FLAGS) -o $@ $(OBJS_EXE)

main.cmx: parser.cmi lexer.cmx semantics.cmi

main.cmo : parser.cmi lexer.cmo semantics.cmi

concrete.cmx: semantics.cmi

concrete.cmo: semantics.cmi

symbolic.cmx: semantics.cmi symbol.cmx

symbolic.cmo: semantics.cmi symbol.cmo

lexer.cmx: parser.cmx

lexer.cmo: parser.cmi

parser.cmi: parser.mli ast.cmo

parser.cmx: parser.cmi ast.cmx

parser.cmo: parser.cmi ast.cmo

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $<

%.cmi: %.mli
	$(OCAMLC) -c $(BUILD_FLAGS) -o $@ $<

%.cmx: %.ml
	$(OCAMLC) -c $(BUILD_FLAGS) -o $@ $<

%.cmo: %.ml
	$(OCAMLC) -c $(BUILD_FLAGS) -o $@ $<

clean:
	rm -rf $(EXE)
	rm -rf lexer.ml
	rm -rf parser.ml parser.mli
	rm -rf *.cmi
	rm -rf *.cmo
	rm -rf *.cmx
	rm -rf *.o
