# $Id: Makefile,v 1.1 2018/10/01 06:28:18 sato Exp sato $

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "  scxml\t\tgenerate \"$(testcase).scxml\" (in ./out)"
	@echo -e "  run\t\trun \"$(testcase).scxml\" (needs: scxmlrun)"

clean::;	rm -f *~
veryclean::
install::

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)
include ../Makefile.common

MODELS	= ping pong
MODELS	+= monitor monitor2

%.scxml:	%.rules
	accept_transition=" " rules2scxml $< -o $@

scxml:	$(MODELS:%=%.scxml)
svg:	$(MODELS:%=%.svg)

run:
	shelltest ping_pong.conf

