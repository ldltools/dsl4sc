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

// ----------
// parse
// ----------
var ast = parse (code, {sourceType: "module", plugins: ["typescript", "@babel/plugin-proposal-decorators"]});

const parse_only = (process.env["safeguard_parse_only"] == "true")
if (parse_only) {
    console.log (JSON.stringify (ast, null, "  "));
    process.exit (0);
}

// ----------
// transform
// ----------
//traverse (ast, () => { FunctionDeclaration (path) { console.log (path.node.id.name); }});
traverse (ast, transformer.visitor (conf));
if (process.env["safeguard_transform_print"] == "true") console.log (JSON.stringify (ast, null, "  "));

// ----------
// unparse
// ----------
const transpiled = generate (ast, {}, code);
console.log (transpiled.code);
