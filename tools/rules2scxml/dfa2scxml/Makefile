# $Id: $

TOP_DIR		= $(shell readlink -f ../../..)
PREFIX		?= $(TOP_DIR)/_build/install

SCRIPTS =
SCRIPTS +=	dfa2dot.sh dfa2scxml.sh
HELPERS	:=	$(shell echo dfa2scxml_helpers/*.xq)

all::

install::	all
	@mkdir -p $(PREFIX)/lib/dsl4sc/tools{,/dfa2scxml_helpers}
	rsync -avzop $(SCRIPTS) $(PREFIX)/lib/dsl4sc/tools
	rsync -avzop $(HELPERS) $(PREFIX)/lib/dsl4sc/tools/dfa2scxml_helpers

clean::
veryclean::	clean
