# $Id: $

TOP_DIR		= $(shell readlink -f ../../..)
PREFIX		?= $(TOP_DIR)/_build/install

BINS =		safeguard
SCRIPTS =	guardgen.sh
HELPERS	:=	$(shell echo guardgen_helpers/*.xq)

all::
clean::;	rm -f *~
veryclean::	clean

install::	all install_bins install_scripts
install_bins::
	@mkdir -p $(PREFIX)/bin
	for f in $(BINS); do \
	test -x $$f.sh || exit 1; \
	cp -pf $$f.sh $(PREFIX)/bin/$$f; \
	chmod +x $(PREFIX)/bin/$$f; \
	done
install_scripts::
	@mkdir -p $(PREFIX)/lib/dsl4sc/tools{,/guardgen_helpers}
	rsync -avzop $(SCRIPTS) $(PREFIX)/lib/dsl4sc/tools
	rsync -avzop $(HELPERS) $(PREFIX)/lib/dsl4sc/tools/guardgen_helpers

