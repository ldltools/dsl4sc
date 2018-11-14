all::
clean::
install::

TOP_DIR		= $(shell readlink -f ../../..)
PREFIX		?= $(TOP_DIR)/_build/install
SUBDIRS		= man

all::
	for d in $(SUBDIRS); do make -C $$d $@; done
clean::
	for d in $(SUBDIRS); do make -C $$d $@; done
install::	all
	for d in $(SUBDIRS); do make -C $$d PREFIX=$(PREFIX) $@; done
