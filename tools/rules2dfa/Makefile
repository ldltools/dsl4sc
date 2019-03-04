# $Id: $

TOP_DIR		= $(shell readlink -f ../..)
PREFIX		?= $(TOP_DIR)/_build/install
LIBDIR		= $(PREFIX)/lib/dsl4sc/tools
SUBDIRS		= ldl2dfa

SCRIPTS		= rules2dfa.sh
HELPER_DIRS	= rules2dfa_helpers

all::
clean::;	find . -name '*~' | xargs rm -f
veryclean::	clean

install::
	@mkdir -p $(LIBDIR)
	rsync -avzop $(SCRIPTS) $(LIBDIR)

install::
	for d in $(HELPER_DIRS); do \
	mkdir -p $(LIBDIR)/$$d && rsync -avzop $$d/ $(LIBDIR)/$$d;\
	done

#
all::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

install::	all
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) $@; done

clean::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

veryclean::	clean
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
