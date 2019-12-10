# $Id: Makefile,v 1.1 2018/10/01 06:28:18 sato Exp sato $

usage::
	@echo "usage: make <target>"
	@echo "where <target> ::= all | images | test"

all::

clean::;	find . -name '*~' | xargs rm -f
veryclean::	clean
	rm -f out/*
install::

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)
include ../Makefile.common

SPECS	=	tx_single-spec tx_composite-spec tx_composite_err-spec
MODELS	=	tx_composite-model\
		htlc_single-model htlc_composite-model

MODELS	+=	tx_composite-model_enforced

all::;	@mkdir -p out
all::	$(SPECS:%=out/%.dfa.xml)
all::	$(MODELS:%=out/%.scxml)

images::	$(SPECS:%=out/%.dfa.png)
images::	$(MODELS:%=out/%.png) $(MODELS:%=out/%.eps) $(MODELS:%=out/%.tex)

#
extended/%-model_enforced.dsl:	extended/%-model_enforced.dsl.cpp %-model.dsl %-spec.dsl
	gcc $< -E | grep -v '^#' > $@

#
out/%.dsl:	extended/%.dsl
	cp $< $@
out/%.dsl:	%.dsl
	cp $< $@

%.scxml:	%.dsl
	$(RULES2SCXML) $< -o $@ --auto-exit

#
test:
	shelltest tx.conf
