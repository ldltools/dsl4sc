all::
clean::;	rm -f *~
install::

TOP_DIR		= $(shell readlink -f ../../../..)
PREFIX		?= $(TOP_DIR)/_build/install
SUBDIRS		=

%.man:	%.tex
	latex2man $< $@

man:	rules2scxml.man

install::	all
	@dir=$(PREFIX)/share/man/man1; mkdir -p $$dir && test -w $$dir || exit 1
	gzip -c rules2scxml.man > $(PREFIX)/share/man/man1/rules2scxml.1.gz
