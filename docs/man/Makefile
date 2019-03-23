all::
clean::;	rm -f *~ \#*
install::

TOP_DIR		= $(shell readlink -f ../..)
PREFIX		?= $(TOP_DIR)/_build/install
SUBDIRS		=

%.man:	%.tex
	latex2man $< $@
%.html:	%.tex
	latex2man -H: $< $@

COMMANDS	= rulessat rulesmc rules2ldl
MANPAGES	= $(COMMANDS:%=%.man)
HTMLPAGES	= $(COMMANDS:%=%.html)

man:	$(MANPAGES)
html:	$(HTMLPAGES)

install::	all
	@dir=$(PREFIX)/share/man/man1; mkdir -p $$dir && test -w $$dir || exit 1
	for f in $(MANPAGES); do \
	test -f $$f || continue; \
	dest="$(PREFIX)/share/man/man1/`basename $$f .man`.1.gz"; \
	test -f $$dest -a $$dest -nt $$f && continue; \
	gzip -c $$f > $$dest; \
	done
