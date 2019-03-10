# $Id: $

PREFIX		?= $(shell readlink -f ../_build/install)
SUBDIRS		= rules2dfa modelgen rules2scxml

all::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

install::	all
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) $@; done

clean::
	find . -name '#*' -or -name '*~' | xargs rm -f
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

veryclean::	clean
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
