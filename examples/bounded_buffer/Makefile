
usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tscxml\tgenerate \"$(testcase).scxml\" (in ./out)"
	@echo -e "\trun\trun \"$(testcase).scxml\""

testcase = bounded_buffer

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
