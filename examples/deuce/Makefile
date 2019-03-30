#

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tscxml\tgenerate \"$(model).scxml\" (in ./out)"
	@echo -e "\trun\trun \"$(model).scxml\""

model = deuce

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)
-include ../Makefile.common

out/$(model).scxml:	$(model).dsl
	mkdir -p out && rules2scxml $< -o $@ --auto-exit

scxml:	out/$(model).scxml
run:
	shelltest $(model).conf

install::
clean::
veryclean::
