# $Id: Makefile,v 1.1 2017/11/14 07:36:27 sato Exp sato $

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tscxml\tgenerate \"$(testcase).scxml\" (in ./out)"
	@echo -e "\trun\trun \"$(testcase).scxml\""

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)

-include ../Makefile.common

testcase = echo

dfa:	out/$(testcase).dfa.png
scxml:	out/$(testcase).scxml
uml:	out/$(testcase).uml
svg:	out/$(testcase).svg

run:
	@test -f echo.scxml -a -f echo.in || { echo "input file(s) missing" > /dev/stderr; exit 1; }
	$(SCXMLRUN) echo.scxml echo.in
run_ccflow:
	scxml=$${scxml:-out/echo.scxml}; test -f $$scxml || exit 1;\
	input='["_init"]\n["echo", "hello"]\n["echo", "world"]';\
	echo -e $$input | $(SCXMLRUN) $$scxml 2> /dev/null

install::
clean::
veryclean::
