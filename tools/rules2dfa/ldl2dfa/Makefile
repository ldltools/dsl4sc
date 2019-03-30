#

TOP_DIR		= $(shell readlink -f ../../..)
PREFIX		?= $(TOP_DIR)/_build/install
LIBDIR		= $(PREFIX)/lib/dsl4sc/tools
SUBDIRS		= mso2dfa
SCRIPTS		= ldl2dfa.sh dfa2dot.sh

all::
clean::

install::
	@mkdir -p $(LIBDIR)
	rsync -avzop $(SCRIPTS) $(LIBDIR)

#
all::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

install::	all
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) $@; done

clean::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

veryclean::	clean
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
