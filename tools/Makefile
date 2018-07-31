# $Id: $

PREFIX		?= $(shell readlink -f ../_build/install)
SUBDIRS		= dfa2dfa rules2scxml

all::
	for d in $(SUBDIRS); do make -C $$d $@; done

install::	all
	for d in $(SUBDIRS); do make -C $$d PREFIX=$(PREFIX) $@; done

clean::
	find . -name '#*' -or -name '*~' | xargs rm -f
	for d in $(SUBDIRS); do make -C $$d $@; done

veryclean::	clean
	for d in $(SUBDIRS); do make -C $$d $@; done
	rm -rf _build/*
