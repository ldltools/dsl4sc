# $Id: $

TOP_DIR		= $(shell readlink -f ../..)
PREFIX		?= $(TOP_DIR)/_build/install
SUBDIRS		= dfa2scxml docs

BINS		= rules2scxml

all::
install::	all
	@mkdir -p $(PREFIX)/bin
	for f in $(BINS); do \
	test -x $$f.sh || exit 1; \
	cp -pf $$f.sh $(PREFIX)/bin/$$f; \
	chmod +x $(PREFIX)/bin/$$f; \
	done

clean::;	find . -name '*~' | xargs rm -f
veryclean::	clean

#
all::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

install::	all
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) $@; done

clean::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

veryclean::	clean
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
