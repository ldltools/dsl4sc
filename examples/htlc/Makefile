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

SPECS	=	tx_single-spec tx_composite-spec\
		htlc_composite-spec
MODELS	=	tx_composite-model\
		htlc_single-model htlc_composite-model

all::;	@mkdir -p out
all::	$(SPECS:%=out/%.dfa.xml)
all::	$(MODELS:%=out/%.scxml)

images::	all
images::	$(SPECS:%=out/%.dfa.png)
images::	$(MODELS:%=out/%.png) $(MODELS:%=out/%.eps) $(MODELS:%=out/%.tex)

out/%.dsl:	%.dsl
	cp $< $@

%.scxml:	%.dsl
	$(RULES2SCXML) $< -o $@ --auto-exit
%.puml:		%.dsl
	$(RULES2SCXML) $< -o $@ --auto-exit -u puml

.PRECIOUS:	out/%.dsl out/%.scxml

# spec enforcement
all images test::
	make -C enforced $@
