all::	_build/escape.byte _build/escape.opt
clean::
	rm -f *~ *.cm*
install::

CAMLFLAGS	= -g
#CAMLFLAGS	= -g -I $(shell ocamlfind query xml-light)
_build/escape.byte:	_build/escape.cmo
	ocamlc $(CAMLFLAGS) -o $@ $<
_build/escape.opt:	_build/escape.cmx
	ocamlopt $(CAMLFLAGS) -o $@ $<
_build/escape.cmo:	escape.ml
	@mkdir -p _build
	ocamlc $(CAMLFLAGS) -c $< -o $@
_build/escape.cmx:	escape.ml
	@mkdir -p _build
	ocamlopt $(CAMLFLAGS) -c $< -o $@

veryclean:	clean
	rm -f _build/*
