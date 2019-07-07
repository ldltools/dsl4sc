#

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tscxml\tgenerate \"$(model).scxml\" (in ./out)"
	@echo -e "\trun\trun \"$(model).scxml\""

#model = deuce
model = deuce2

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)
-include ../Makefile.common

out/deuce.scxml:	deuce.spec
	mkdir -p out && rules2scxml $< -o $@ --auto-exit

out/deuce2.scxml:	deuce2.spec
	mkdir -p out && rules2scxml $< -o $@ --auto-exit

scxml:	out/$(model).scxml
run:
	shelltest $(model).conf

install::
clean::
veryclean::
