#

usage::
	@echo "usage: make <target>"
	@echo "where <target> can be either of the following"
	@echo -e "\tall\tattach spec to js code (resluts to be put into ./out)"
	@echo -e "\ttest\trun tests (using jest)"

all::
test::	all check
clean::;	rm -f *~

RULES2SCXML	?= rules2scxml
SAFEGUARD	?= safeguard
TSC		?= $(shell npm bin)/tsc
JEST		?= $(shell npm bin)/jest

check:
	@test $$(npm bin) = "$$(readlink -f $$PWD/node_modules/.bin)" || exit 1
	@test -x $(JEST) || exit 1

setup:
	@test -f package.json || npm init -y
	@test -d node_modules || mkdir -p node_modules/.bin
	@for pkg in jest babel-jest babel-plugin-transform-es2015-modules-commonjs;\
	 do test -d node_modules/$$pkg || npm i -D $$pkg; done

out/%.sg:	%.js %.scxml
	@mkdir -p out
	$(SAFEGUARD) $< -o /dev/null --confout $@
out/%.ast:	%.js %.scxml
	@mkdir -p out
	$(SAFEGUARD) $< -o $@ --parse-only
out/%.puml:	%.scxml
	@mkdir -p out
	scxml2uml.sh $< -o $@
%.svg:	%.puml
	plantuml.sh $< -tsvg 

veryclean::	clean
	rm -f *.scxml *.puml
	rm -rf out

# --------------------------------------------------------------------------------
# case: ping_pong

all::	out/ping_pong_guarded.js

ping_pong.scxml:	ping_pong.spec
	$(RULES2SCXML) $< -o $@ --exit=quit
out/ping_pong_guarded.js:	ping_pong.js ping_pong.scxml
	@mkdir -p out
	$(SAFEGUARD) $< -o $@

test::	test_ping_pong
test_ping_pong:	ping_pong.test.js out/ping_pong_guarded.js
	$(JEST) $< --silent

# --------------------------------------------------------------------------------
# case: deuce

all::	out/deuce_guarded.js out/deuce_decorated.ts

deuce.scxml:	deuce.spec
	$(RULES2SCXML) $< -o $@ --auto-exit
out/deuce_guarded.js:	deuce.js deuce.scxml
	@mkdir -p out
	$(SAFEGUARD) $< -o $@ --js-class=Deuce

test::	test_deuce
test_deuce::	deuce.test.js out/deuce_guarded.js
	$(JEST) $< --silent

# --------------------------------------------------------------------------------
# case: deuce w. decorators attached (experimental)

out/deuce_decorated.ts:	deuce.js deuce.scxml
	@mkdir -p out
	$(SAFEGUARD) $< -o $@ --js-class=Deuce --js-decorators=expects,ensures,update --js-keep-decorators

# decorator expansion
# ** note: typescript compiler needs to be properly set up.
out/%_decorated.js:	out/%_decorated.ts
	@test -L node_modules || ln -s ../src/ts/node_modules
	@test -x $(TSC) || exit 1
	$(TSC) -t ES2016 --experimentalDecorators -m amd --allowJS -outFile $@ $<

veryclean::
	test -L node_modules && rm -f node_modules || true

#test_deuce::	out/deuce_decorated.js
