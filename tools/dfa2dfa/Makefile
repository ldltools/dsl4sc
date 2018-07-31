# $Id: $

TOP_DIR		= $(shell readlink -f ../..)
PREFIX		?= $(TOP_DIR)/_build/install
SUBDIRS		= src

all::
	for d in $(SUBDIRS); do make -C $$d $@; done

install::	all
	for d in $(SUBDIRS); do make -C $$d PREFIX=$(PREFIX) $@; done

clean::
	for d in $(SUBDIRS); do make -C $$d $@; done

veryclean::	clean
	for d in $(SUBDIRS); do make -C $$d $@; done
