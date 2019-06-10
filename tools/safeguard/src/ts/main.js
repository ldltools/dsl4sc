// -*-javascript-*-

import {parse} from '@babel/parser';
import * as t from '@babel/types';
import traverse from '@babel/traverse';
import generate from '@babel/generator';

import * as transformer from './transformer.js';
const fs = require ('fs');
const assert = require ('assert');

// usage: node main.js <conf_file>
assert (process.argv.length == 3);

// read conf
// conf = {spec: {location, guards, ..}, code: {location, handlers, ..}}
const conf = JSON.parse (fs.readFileSync (process.argv[2], 'utf8'));

const code = fs.readFileSync (conf.code.location, 'utf8');
var ast = parse (code, {sourceType: "module", plugins: ["typescript", "@babel/plugin-proposal-decorators"]});
//console.log (JSON.stringify (ast, null, "  "));

// transform
/*
const visitor = {
    FunctionDeclaration (path) {
	// path = {id, params, body}
	console.log (path.node.id.name);
	//path.node.id.name = "foo";
    }
}
traverse (ast, visitor);
*/
traverse (ast, transformer.visitor (conf));
//console.log (JSON.stringify (ast, null, "  "));

// codegen
const transpiled = generate (ast, {}, code);
console.log (transpiled.code);
