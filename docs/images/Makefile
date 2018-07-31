all::

%.png:	%.dot
	dot -Tpng $< > $@
%.svg:	%.dot
	dot -Tsvg $< > $@

all::	flow.png flow.svg
clean::;	rm -f *~
