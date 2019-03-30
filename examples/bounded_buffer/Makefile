
usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tscxml\tgenerate \"$(testcase).scxml\" (in ./out)"
	@echo -e "\trun\trun \"$(testcase).scxml\""

testcase = bounded_buffer

# overrides defs in ..//Makefile.common
TOP_DIR	= $(shell readlink -f ../..)
-include ../Makefile.common

scxml:	out/bounded_buffer.scxml
out/bounded_buffer.scxml:	bounded_buffer.rules
	mkdir -p out && rules2scxml $< -o $@

run:
	shelltest bounded_buffer.conf

install::
clean::
veryclean::
