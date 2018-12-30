# $Id: Makefile,v 1.1 2018/10/01 06:28:18 sato Exp sato $

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "  scxml\t\tgenerate \"$(testcase).scxml\" (in ./out)"
	@echo -e "  run\t\trun \"$(testcase).scxml\" (needs: scxmlrun)"

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)
include ../Makefile.common

testcase = ping_pong2

scxml:	out/$(testcase).scxml
puml:	out/$(testcase).puml
svg:	out/$(testcase).svg

run:
	@test -f $(testcase).scxml -a -f $(testcase).in || { echo "input file(s) missing" > /dev/stderr; exit 1; }
	$(SCXMLRUN) $(testcase).scxml $(testcase).in

install::
clean::
veryclean::
