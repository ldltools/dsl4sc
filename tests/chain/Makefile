#

all::
clean::;	rm -f *~

N = 10 20 30 40
MODELS = $(N:%=chain%p) $(N:%=chain%r)

chain%p.dsl:
	./chaingen.sh $* -p -o $@
chain%r.dsl:
	./chaingen.sh $* -r -o $@

out/%.dfa.dot:	%.dsl
	rules2scxml $< -o $@ -u dfadot
out/%.scxml:	%.dsl
	rules2scxml $< -o $@ --auto-exit
%.ldl:	%.dsl
	rules2ldl $< -o $@
%.mso:	%.ldl
	ldl2mso $< -o $@
%.puml:	%.scxml
	scxml2uml.sh $< -o $@ -landscape
%.svg:	%.puml
	plantuml.sh $< -tsvg
%.svg:	%.dot
	dot -Tsvg $< -o $@

.PRECIOUS:	%.scxml %.puml %.svg

all::	$(MODELS:%=%.dsl)
test::	all
	shelltest chain.conf
veryclean::	clean
#	rm -f $(MODELS:%=%.dsl)
	rm -f out/*
