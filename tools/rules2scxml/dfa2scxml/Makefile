# $Id: $

TOP_DIR		= $(shell readlink -f ../../..)
PREFIX		?= $(TOP_DIR)/_build/install
SUBDIRS		= dfa2scxml_helpers

SCRIPTS =
SCRIPTS +=	dfa2dot.sh dfa2scxml.sh
HELPERS	:=	$(shell echo dfa2scxml_helpers/*.xq dfa2scxml_helpers/_build/escape.{byte,opt})

all::
clean::;	rm -f *~

install::	all
	@mkdir -p $(PREFIX)/lib/dsl4sc/tools{,/dfa2scxml_helpers}
	rsync -avzop $(SCRIPTS) $(PREFIX)/lib/dsl4sc/tools
	rsync -avzop $(HELPERS) $(PREFIX)/lib/dsl4sc/tools/dfa2scxml_helpers

all::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
clean::
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
veryclean::	clean
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
