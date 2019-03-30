#

TOP_DIR		= $(shell readlink -f ../../../..)
PREFIX		?= $(TOP_DIR)/_build/install
LIBDIR		= $(PREFIX)/lib/dsl4sc/tools

SCRIPTS		= mso2dfa.sh
HELPER_DIRS	= mso2dfa_helpers

all::
clean::

install::
	@mkdir -p $(LIBDIR)
	rsync -avzop $(SCRIPTS) $(LIBDIR)

install::
	for d in $(HELPER_DIRS); do \
	mkdir -p $(LIBDIR)/$$d && rsync -avzop $$d/ $(LIBDIR)/$$d;\
	done
