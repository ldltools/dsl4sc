# $Id: Makefile,v 1.1 2018/10/01 06:28:18 sato Exp sato $

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tscxml\tgenerate \"$(testcase).scxml\" (in ./out)"
	@echo -e "\trun\trun \"$(testcase).scxml\" (needs: scxmlrun)"

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)

include ../Makefile.common

testcase = ping_pong2

dfa:	out/$(testcase).dfa.png
scxml:	out/$(testcase).scxml
uml:	out/$(testcase).uml
svg:	out/$(testcase).svg

run:
	@test -f $(testcase).scxml -a -f $(testcase).in || { echo "input file(s) missing" > /dev/stderr; exit 1; }
	$(SCXMLRUN) $(testcase).scxml $(testcase).in
run_ccflow:
	scxml=$${scxml:-out/ping_pong.scxml}; test -f $$scxml || exit 1;\
	input='["_init"]\n["ping"]\n["ping"]';\
	echo -e $$input | $(SCXMLRUN) $$scxml 2> /dev/null | sed '/^{/d'

install::
clean::
veryclean::
