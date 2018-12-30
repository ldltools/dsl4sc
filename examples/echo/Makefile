# $Id: Makefile,v 1.1 2017/11/14 07:36:27 sato Exp sato $

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tscxml\tgenerate \"$(testcase).scxml\" (in ./out)"
	@echo -e "\trun\trun \"$(testcase).scxml\""

testcase = echo

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)
-include ../Makefile.common

scxml:	out/$(testcase).scxml
puml:	out/$(testcase).puml
svg:	out/$(testcase).svg

run:
	@test -f $(testcase).scxml -a -f $(testcase).in || { echo "input file(s) missing" > /dev/stderr; exit 1; }
	$(SCXMLRUN) $(testcase).scxml $(testcase).in

install::
clean::
veryclean::
