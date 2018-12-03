all::	_build/escape.byte _build/escape.opt
clean::
	rm -f *~ *.cm*
install::

OPAM_PREFIX	= $(shell opam config var prefix)
OCAMLFIND	= $(OPAM_PREFIX)/bin/ocamlfind
CAMLC		= $(OCAMLFIND) ocamlc
CAMLOPT		= $(OCAMLFIND) ocamlopt
CAMLCFLAGS	= -g
CAMLOPTFLAGS	= -g

_build/escape.byte:	_build/escape.cmo
	$(CAMLC) $(CAMLCFLAGS) -o $@ $<
_build/escape.opt:	_build/escape.cmx
	$(CAMLOPT) $(CAMLOPTLAGS) -o $@ $<
_build/escape.cmo:	escape.ml
	@mkdir -p _build
	$(CAMLC) $(CAMLCFLAGS) -c $< -o $@
_build/escape.cmx:	escape.ml
	@mkdir -p _build
	$(CAMLOPT) $(CAMLOPTFLAGS) -c $< -o $@

veryclean:	clean
	rm -f _build/*
