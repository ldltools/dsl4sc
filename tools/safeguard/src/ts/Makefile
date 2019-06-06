#

all::
clean::
insgall::

test::	check
	testdir=../../tests;\
	node -r esm main.js $$testdir/deuce.js $$testdir/deuce.guards

#
setup::	check setup_node setup_typescript setup_babel
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

check::
	@test -f package.json || exit 1
	@test $(npm bin) = $(readlink -f $PWD/node_modules/.bin) || exit 1
