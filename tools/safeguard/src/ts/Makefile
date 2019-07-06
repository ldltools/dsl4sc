#

TOP_DIR	= $(shell readlink -f ../../../..)
PREFIX	?= $(TOP_DIR)/_build/install

BUNDLED	= safeguard_ts.js
SRCS	= main.js transformer.js

all::	$(BUNDLED)

clean::;	rm -f *~
install::	all
	@mkdir -p $(PREFIX)/lib/dsl4sc/tools/safeguard_helpers
	@test -f $(BUNDLED) || exit 1
	rsync -avzop $(BUNDLED) $(PREFIX)/lib/dsl4sc/tools/safeguard_helpers
	rsync -avzop decorators.ts $(PREFIX)/lib/dsl4sc/tools/safeguard_helpers

$(BUNDLED):	$(SRCS)
	$(MAKE) setup
	@test -x $$(npm bin)/webpack || exit 1
	$$(npm bin)/webpack

test::
#	node -r esm main.js ../../tests/deuce.sg

#
setup::	check
	test -d node_modules/esm || $(MAKE) setup_node
	test -d node_modules/typescript -a -d node_modules/@types || $(MAKE) setup_typescript
	test -d node_modules/@babel || $(MAKE) setup_babel
	test -d node_modules/webpack || $(MAKE) setup_webpack

check::
	@test -f package.json || npm init -y
	@test -d node_modules || mkdir -p node_modules/.bin
	@test $$(npm bin) = "$$(readlink -f $$PWD/node_modules/.bin)" || exit 1

#
setup_node::
	npm i -D esm
setup_typescript::
	npm i -D @types/node
	npm i -D typescript
setup_babel::
	npm i -D @babel/core @babel/parser @babel/traverse @babel/generator
	npm i -D @babel/preset-typescript @babel/preset-env
	npm i -D @babel/plugin-proposal-class-properties @babel/plugin-proposal-object-rest-spread
	npm i -D @babel/plugin-proposal-decorators

setup_eslint::
	npm i -D eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin
setup_prettier::
	npm i -D prettier eslint-plugin-prettier eslint-config-prettier 

setup_webpack::
	npm i -D webpack webpack-cli babel-loader
