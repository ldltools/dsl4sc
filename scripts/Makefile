# $Id: $

TOP_DIR	= $(shell readlink -f ..)
PREFIX	?= $(TOP_DIR)/_build/install
BINS	= rulessat rulesmc

all::
install::	all
	for f in $(BINS); do \
	test -x $$f.sh || exit 1; \
	cp -pf $$f.sh $(PREFIX)/bin/$$f; \
	done

clean::;	find . -name '*~' | xargs rm -f
veryclean::	clean
