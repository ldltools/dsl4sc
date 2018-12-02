# $Id: $

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tlist\tshow the test cases in this directory"
	@echo -e "\tscxml\tgenerate SCXMLs (in ./out)"
	@echo -e "\ttest\trun tests"

list:
	@for f in test*rules; do echo -ne "`basename $$f .rules`\t"; cat $$f | sed '{2,$$d;s/^\/\/ *//g}'; done

TOP_DIR	= $(shell readlink -f ..)
include $(TOP_DIR)/tests/Makefile.common

# shelltest (https://github.com/simonmichael/shelltestrunner) can be installed
# by running "apt-get install shelltestrunner"
test::
	shelltest tests.conf

# examples
test::	examples.conf
	shelltest $<

examples.conf:	echo.conf ping_pong.conf accumulator.conf bounded_buffer.conf
	cat $^ > $@

# ----------
out/%.scxml:	%.rules
	@mkdir -p out
	$(RULES2SCXML) $< -o $@

.PRECIOUS:	out/%.rules.xml out/%.ldl out/%.map out/%.mso
.PRECIOUS:	%.dfa.dot %.dfa.xml

TESTCASES_RULES	= $(shell echo test??.rules)
TESTCASES_SCXML	= $(TESTCASES_RULES:%.rules=out/%.scxml)
TESTCASES_SCXML	:= $(filter-out out/test17.scxml,$(TESTCASES_SCXML))
TESTCASES_PUML	= $(TESTCASES_SCXML:%.scxml=%.puml)
TESTCASES_SVG	= $(TESTCASES_PUML:%.puml=%.svg)

.PRECIOUS:	out/%.ldl out/%.re out/%.mso

scxml:	$(TESTCASES_SCXML)
puml:	$(TESTCASES_PUML)
svg:	$(TESTCASES_SVG)

install::
clean::
	rm -f *~ *.png
veryclean::	clean
	rm -f out/*
