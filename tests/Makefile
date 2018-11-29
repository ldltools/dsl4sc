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
	@mkdir -p out
	shelltest tests_scxml.conf

# examples
test::
	@mkdir -p out
	shelltest echo.conf
	shelltest ping_pong.conf
	shelltest accumulator.conf

# ----------
out/%.ldl:	%.rules
	@mkdir -p out
	$(RULES2LDL) $< -o out/$*.ldl -t json --map out/$*.map
out/%.scxml:	%.rules
	@mkdir -p out
	$(RULES2SCXML) $< -o $@

.PRECIOUS:	out/%.rules.xml out/%.ldl out/%.map out/%.mso
.PRECIOUS:	%.dfa.dot %.dfa.xml

TESTCASES_RULES	= $(shell echo test??.rules)
TESTCASES_LDL	= $(TESTCASES_RULES:%.rules=%.ldl)
TESTCASES_DFA	= $(TESTCASES_RULES:%.rules=out/%.dfa.png)
TESTCASES_AFW	= $(TESTCASES_RULES:%.rules=out/%.afw.png)

TESTCASES_SCXML	= $(TESTCASES_RULES:%.rules=out/%.scxml)
TESTCASES_SCXML	:= $(filter-out out/test17.scxml out/test21.scxml,$(TESTCASES_SCXML))
TESTCASES_UML	= $(TESTCASES_SCXML:%.scxml=%.puml)
TESTCASES_SVG	= $(TESTCASES_UML:%.uml=%.svg)

.PRECIOUS:	out/%.ldl out/%.re out/%.mso

dfa:	$(TESTCASES_DFA)
afw:	$(TESTCASES_AFW)
scxml:	$(TESTCASES_SCXML)
puml:	$(TESTCASES_UML)
svg:	$(TESTCASES_SVG)

install::
clean::
	rm -f *~ *.png
veryclean::	clean
	rm -f out/*
