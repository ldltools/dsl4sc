# $Id: $

TOP_DIR		= $(shell readlink -f ../../..)
PREFIX		?= $(TOP_DIR)/_build/install

SCRIPTS =
SCRIPTS +=	ldl2dfa.sh
HELPERS =	adjust_labels.xq
HELPERS	:=	$(HELPERS:%=mso2dfa_helpers/%)

all::

install::	all
	@mkdir -p $(PREFIX)/lib/dsl4sc/tools
	rsync -avzop ldl2dfa.sh $(PREFIX)/lib/dsl4sc/tools
	@mkdir -p $(PREFIX)/lib/dsl4sc/tools/ldl2dfa_helpers
	rsync -avzop mso2dfa.sh $(PREFIX)/lib/dsl4sc/tools/ldl2dfa_helpers
	@mkdir -p $(PREFIX)/lib/dsl4sc/tools/ldl2dfa_helpers/mso2dfa_helpers
	rsync -avzop mso2dfa_helpers/*xq $(PREFIX)/lib/dsl4sc/tools/ldl2dfa_helpers/mso2dfa_helpers

clean::
veryclean::
