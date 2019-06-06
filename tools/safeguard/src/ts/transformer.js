//

import traverse from '@babel/traverse';
import * as t from '@babel/types';

// generate pre/post nodes
function gen_conditions (guards, name)
{
    const guard = guards.find ((elt) => elt.event == name);
    const transitions = guard.transitions.filter ((elt) => elt != null)
    //console.log (transitions);

    const pre =
	  transitions.slice (1).reduce (
	      (rslt, elt) => rslt + " || _state == \"" + elt.state + "\"",
	      "_state == \"" + transitions[0].state + "\"");
    const pre_exp =
	  transitions.slice (1).reduce (
	      (rslt, elt) =>
		  t.logicalExpression ("||", rslt,
				       t.binaryExpression ("==", t.identifier ("_state"),
							   t.stringLiteral (elt.state))),
	      t.binaryExpression ("==", t.identifier ("_state"),
				  t.stringLiteral (transitions[0].state)));
    const next =
	  transitions.reduceRight (
	      (rslt, elt) =>
		  "(_state == \"" + elt.state + "\") ? \"" + elt.target + "\" : (" + rslt + ")",
	      "assert (false)")
    const next_exp =
	  transitions.reduceRight (
	      (rslt, elt) =>
		  t.conditionalExpression (
		      t.binaryExpression ("==", t.identifier ("_state"),
					  t.stringLiteral (elt.state)),
		      t.stringLiteral (elt.target),
		      rslt),
	      t.callExpression (t.identifier ("assert"), [t.booleanLiteral (false)]));

    //console.log ("handler:", name);
    //console.log ("pre:", pre_str);
    //console.log ("next:", next_str);
    //console.log ();

    return {pre: {exp: pre_exp, str: pre},
	    next: {exp: next_exp, str: next}}
}

function add_conditions (path, conds)
{
    const deco_pre =
	  t.decorator (t.callExpression (t.identifier ("atstate"), [conds.pre.exp]));
    path.insertBefore (deco_pre);

    const deco_next =
	  t.decorator (t.callExpression (t.identifier ("nextstate"), [conds.next.exp]));
    path.insertBefore (deco_next);

    if (!path.node.leadingComments) path.node.leadingComments = [];
    path.node.leadingComments.push ({type: "CommentLine", value: " pre: " + conds.pre.str});
    path.node.leadingComments.push ({type: "CommentLine", value: " next: " + conds.next.str});
}

// spec = {initial, final, guards}
export function visitor (spec)
{
    //console.log ("transform called");
    const guards = spec.guards.filter ((elt) => elt != null);
    const events = guards.map ((elt) => elt.event);
    //console.log (events);

    var oop = false;

    return {
	ClassDeclaration : {
	    enter (path) { oop = true; },
	    exit (path) {}
	},
	ClassBody (path) {
	    //console.log (path.node.body);
	    path.node.body =
		[{type: "ClassProperty",
		  static: false,
		  key: {type: "Identifier", name: "_state"},
		  computed: false,
		  typeAnnotation: {type: "TSTypeAnnotation",
				   typeAnnotation: {type: "TSStringKeyword"}},
		  value: t.stringLiteral (spec.initial)}].
		concat (path.node.body);
	},
	ClassMethod (path) {
	    const name = path.node.key.name;
	    if (!events.includes (name)) return;

	    const conds = gen_conditions (guards, name);
	    add_conditions (path, conds);
	},

	FunctionDeclaration (path) {
	    // path.node = {id, params, body}
	    const name = path.node.id.name;
	    if (!events.includes (name)) return;

	    const conds = gen_conditions (guards, name);
	    add_conditions (path, conds);
	}
    }
}

// default
export default function () {
    console.log ("default function callled");
}
