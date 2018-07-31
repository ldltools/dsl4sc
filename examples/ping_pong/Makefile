# $Id: Makefile,v 1.1 2017/11/14 07:36:27 sato Exp sato $

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tdfa\tgenerate \"$(testcase).dfa.png\" (in ./out)"
	@echo -e "\tscxml\tgenerate \"$(testcase).scxml\" (in ./out)"
	@echo -e "\trun\trun \"$(testcase).scxml\""

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)

include ../Makefile.common

testcase = ping_pong

dfa:	out/$(testcase).dfa.png
scxml:	out/$(testcase).scxml
uml:	out/$(testcase).uml
svg:	out/$(testcase).svg
run:
	scxml=$${scxml:-out/ping_pong.scxml}; test -f $$scxml || exit 1;\
	input='["_init"]\n["ping"]\n["ping"]';\
	echo -e $$input | $(SCXMLRUN) $$scxml 2> /dev/null | sed '/^{/d'

install::
clean::
veryclean::
