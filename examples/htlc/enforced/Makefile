#

all::
clean::;	rm -f *~

TOP_DIR	= $(shell readlink -f ../../..)
include ../../Makefile.common

MODELS	=	tx_composite-model_enforced\
		htlc_composite-model_enforced

%-model_enforced.dsl:	%-model_enforced.dsl.cpp ../%-model.dsl ../%-spec.dsl
	gcc $< -E | grep -v '^#' > $@

all::;	mkdir -p ../out
all::	$(MODELS:%=%.dsl) $(MODELS:%=../out/%.scxml)

#
../out/%.dsl:	%.dsl
	cp $< $@

.PRECIOUS:	%.dsl

%.scxml:	%.dsl
	$(RULES2SCXML) $< -o $@ --auto-exit
%.puml:		%.dsl
	$(RULES2SCXML) $< -o $@ --auto-exit -u puml

#
images::	$(MODELS:%=../out/%.png) $(MODELS:%=../out/%.eps) $(MODELS:%=../out/%.tex)
test:	all
	shelltest tx.conf

#
veryclean:	clean
	rm -f *.dsl
