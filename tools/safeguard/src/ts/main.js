// -*-javascript-*-

import {parse} from '@babel/parser';
import traverse from '@babel/traverse';
import generate from '@babel/generator';
import transform from './transform.js';
const fs = require ('fs');

// read and parse source code
const code = fs.readFileSync (process.argv[2], 'utf8');
var ast = parse (code, {sourceType: "module", plugins: ["typescript", "@babel/plugin-proposal-decorators"]});
//console.log (JSON.stringify (ast, null, "  "));

// transform
const spec = JSON.parse (fs.readFileSync (process.argv[3], 'utf8'));
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
traverse (ast, transform (spec));
//console.log (JSON.stringify (ast, null, "  "));

// generate
const transpiled = generate (ast, {}, code);
console.log (transpiled.code);
