all::
clean::
install::

TOP_DIR		= $(shell readlink -f ../../..)
PREFIX		?= $(TOP_DIR)/_build/install
SUBDIRS		= man

all::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
clean::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
install::	all
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) $@; done
