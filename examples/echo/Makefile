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

out/echo.scxml:	echo.rules
	mkdir -p out && rules2scxml $< -o $@ --auto-exit

scxml:	out/echo.scxml
run:
	shelltest echo.conf

install::
clean::
veryclean::
