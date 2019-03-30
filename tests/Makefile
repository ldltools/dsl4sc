#

usage:
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tlist\tshow the test cases in this directory"
	@echo -e "\ttest\trun tests"

list:
	@for f in test*rules; do echo -ne "`basename $$f .rules`\t"; cat $$f | sed '{2,$$d;s/^\/\/ *//g}'; done

test::
	shelltest arith.conf
	shelltest equality.conf

test::
	rm -f out/test*.scxml
	shelltest tests.conf

EXAMPLES =	echo ping_pong deuce bounded_buffer
define TESTRULE
test::	test_$(1)
test_$(1):
	cd ../examples/$(1); test -f $(1).conf && shelltest $(1).conf
endef
$(foreach ex,$(EXAMPLES),$(eval $(call TESTRULE,$(ex))))

all::
install::
clean::;	rm -f *~
veryclean::	clean
	rm -f out/*
