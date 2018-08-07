#

all::
clean::;	rm -f *~ \#*
veryclean::	clean

.PRECIOUS:	%.dvi %.xml

# pdf
%.pdf:	%.tex
	pdflatex $<

# svg
%.svg:	%.dvi
	dvisvgm $<
%.dvi:	%.tex
	latex $<

# html
%.html:	%.xml
#	latexmlpost --format=html4 $< > $@
#	latexmlpost --format=html5 --mathsvg --nodefaultcss $< > $@
	latexmlpost --format=html5 --nodefaultcss --noplane1 $< > $@
%.xml:	%.tex
	latexml $< > $@

TARGETS = $(shell echo {core_,}grammar.{pdf,svg,html})
all::	$(TARGETS)
install::

clean::
	rm -f *.aux *.log *.out
veryclean::
	rm -f *.xml *.html *.pdf
