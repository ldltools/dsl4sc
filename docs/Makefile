#
all::
clean::;	rm -f *~
install::

TOP_DIR	= $(shell readlink -f ..)
PREFIX	?= $(TOP_DIR)/_build/install
SUBDIRS	= grammar man

all::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@ || exit 1; done
install::	all
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) $@; done
clean::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@ || exit 1; done
veryclean::
