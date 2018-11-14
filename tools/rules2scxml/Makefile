# $Id: $

TOP_DIR		= $(shell readlink -f ../..)
PREFIX		?= $(TOP_DIR)/_build/install
SUBDIRS		= rules2dfa dfa2scxml docs

BINS		=
BINS		+= rules2scxml ldl2scxml

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
	for d in $(SUBDIRS); do make -C $$d $@; done

install::	all
	for d in $(SUBDIRS); do make -C $$d PREFIX=$(PREFIX) $@; done

clean::
	for d in $(SUBDIRS); do make -C $$d $@; done

veryclean::	clean
	for d in $(SUBDIRS); do make -C $$d $@; done
