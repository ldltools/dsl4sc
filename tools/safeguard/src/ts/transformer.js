//

import traverse from '@babel/traverse';
import * as t from '@babel/types';
const assert = require ('assert');

var _state = null;
var _state_pre = null;

// generate pre/post nodes
function gen_conditions (guards, name)
{
    const guard = guards.find ((elt) => elt.handler == name);
    const transitions = guard.transitions.filter ((elt) => elt != null)
    // transitions = [{state, target}, {state, target}, ..]
    // transitions_exp = [[state, target], [state, target], ..]
    /*
    const transitions_exp =
	  transitions.map (
	      (elt) => t.arrayExpression ([t.stringLiteral (elt.state), t.stringLiteral (elt.target)]));
    */
    const transitions_props =
	  transitions.map ((elt) => {
	      return t.objectProperty (t.stringLiteral (elt.state), t.stringLiteral (elt.target));
	  })
    const transitions_exp = t.objectExpression (transitions_props);

    // [pre]
    // _state == state1 || _state == state2 || ...
    const pre_str =
	  transitions.slice (1).reduce (
	      (rslt, elt) => rslt + " || " + _state + " == \"" + elt.state + "\"",
	      _state + " == \"" + transitions[0].state + "\"");
    const pre_exp =
	  transitions.slice (1).reduce (
	      (rslt, elt) =>
		  t.logicalExpression ("||", rslt,
				       t.binaryExpression ("==", t.identifier (_state),
							   t.stringLiteral (elt.state))),
	      t.binaryExpression ("==", t.identifier (_state),
				  t.stringLiteral (transitions[0].state)));

    // [next]
    // (_state == state1) ? target1 : ((_state == state2) ? target2 : ...)
    const next_str =
	  transitions.reduceRight (
	      (rslt, elt) =>
		  "(" + _state + " == \"" + elt.state + "\") ? \"" + elt.target + "\" : (" + rslt + ")",
	      "assert (false)")
    const next_exp =
	  transitions.reduceRight (
	      (rslt, elt) =>
		  t.conditionalExpression (
		      t.binaryExpression ("==", t.identifier (_state),
					  t.stringLiteral (elt.state)),
		      t.stringLiteral (elt.target),
		      rslt),
	      t.callExpression (t.identifier ("assert"), [t.booleanLiteral (false)]));

    // [post]
    // (pre(_state) == state1 && _state == target1) || (pre(_state) == state2 && _state = target2) || ..
    const post_str =
	  transitions.slice(1).reduce (
	      (rslt, elt) =>
		  rslt + " || " + "(" + _state_pre + " == \"" + elt.state + "\" && " + _state + " == \"" + elt.target + "\")",
	      "(" + _state_pre + " == \"" + transitions[0].state + "\" && " + _state + " == \"" + transitions[0].target + "\")")
    const guard_to_exp =
	  (elt) => {
	      const eq1 = t.binaryExpression ("==", t.identifier (_state_pre), t.stringLiteral (elt.state))
	      const eq2 = t.binaryExpression ("==", t.identifier (_state), t.stringLiteral (elt.target))
	      return t.logicalExpression ("&&", eq1, eq2)
	  }
    const post_exp =
	  transitions.slice(1).reduce (
	      (rslt, elt) => t.logicalExpression ("||", rslt, guard_to_exp (elt)),
	      guard_to_exp (transitions[0]))

    //console.log ("handler:", name);
    //console.log ("pre:", pre_str);
    //console.log ("next:", next_str);
    //console.log ("post:", post_str);
    //console.log ();

    return {pre: {exp: pre_exp, str: pre_str},
	    next: {exp: next_exp, str: next_str},
	    post: {exp: post_exp, str: post_str},
	    transitions: {exp: transitions_exp}};
}

function gen_decorators (conds)
{
    const pre_deco =
	  t.decorator (t.callExpression (t.identifier ("expects"), [conds.pre.exp]));

    const update_exp = t.assignmentExpression ("=", t.identifier ("_state"), conds.next.exp);
    const update_deco =
	  t.decorator (t.callExpression (t.identifier ("update"), [update_exp]));

    const post_deco =
	  t.decorator (t.callExpression (t.identifier ("ensures"), [conds.post.exp]));

    // transitions_exp = [[state, target], [state, target], ..]
    const transitions_deco =
	  t.decorator (t.callExpression (t.identifier ("transitions"), [conds.transitions.exp]));

    return {pre: {deco: pre_deco},
	    update: {deco: update_deco},
	    post: {deco: post_deco},
	    transitions: {deco: transitions_deco}};
}

// conf = {spec: {location, guards: {params, body}, ..},
//         code: {location, class, handlers, ..},
//         options: {decorators}
//        }
export function visitor (conf)
{
    // spec
    const event_guards_params = conf.spec.guards.params;
    const event_guards = conf.spec.guards.body.filter ((elt) => elt != null);
    //console.log (JSON.stringify (event_guards));
    const events = event_guards.map ((elt) => elt.event);
    //console.log (JSON.stringify (events));
    const initial = conf.spec.guards.params.initial;

    // code
    const class_name = conf.code.class;
    // handlers = [{name, event}, ..]
    var handlers = conf.code.handlers ? conf.code.handlers : [];
    handlers = handlers.filter ((elt) => elt != null);
    handlers = handlers.concat (events.map ((elt) => { return {name: elt, event: elt} }));
    if (class_name)
	handlers = handlers.map ((elt) => {
	    const name = (elt.name.includes (".")) ? elt.name : (class_name + "." + elt.name);
	    return {name: name, event: elt.event} });
    //console.log ("handlers:", JSON.stringify (handlers))
    const handler_names = handlers.map ((elt) => { return elt.name })

    var guards = event_guards.map ((elt) => {
	// elt = [{event, transitions}, ..]
	const event = elt.event;
	assert (events.includes (event));
	const transitions = elt.transitions.filter ((elt) => elt != null);
	const handler = handlers.find ((elt) => { return elt.event == event }).name;
	return {handler: handler, transitions: transitions};
    })
    //console.log ("guards:", JSON.stringify (guards));

    // options
    const options = conf.options;
    //console.log ("options:", options);

    // decorators
    var decorators = {initial: true, expects: true, update: true, ensures: true, transitions: true};
    if (options && options.decorators)
    {
	if (options.decorators.initial != null) decorators.initial = options.decorators.initial;
	if (options.decorators.expects != null) decorators.expects = options.decorators.expects;
	if (options.decorators.update != null) decorators.update = options.decorators.update;
	if (options.decorators.ensures != null) decorators.ensures = options.decorators.ensures;
	if (options.decorators.transitions != null) decorators.transitions = options.decorators.transitions;
    }
    //console.log ("decorators:", decorators);
    // tracker
    const tracker = {name: "_state", local: false, global: false}
    if (options && options.tracker)
    {
	if (options.tracker.name != null) tracker.name = options.tracker.name;
	if (options.tracker.local != null) tracker.local = options.tracker.local;
	if (options.tracker.global != null) tracker.global = options.tracker.global;
    }
    _state = tracker.name;
    _state_pre = _state_pre = _state + "_pre";

    var current_class_name = null;
    return {
	Program : {
	    exit (path) {
		if (!tracker.global) return;
		const var_prop =
		      {type: "VariableDeclaration",
		       declarations: [{
			   type: "VariableDeclarator",
			   id: {type: "Identifier", name: _state},
			   init: {type: "StringLiteral", value: initial}
		       }],
		       kind: "var",
		      }
		path.node.body.push (var_prop);
	    }
	},
	ClassDeclaration : {
	    enter (path) {
		current_class_name = path.node.id.name;
		if (class_name && current_class_name != class_name) return;
		const init_deco =
		      t.decorator (t.callExpression (t.identifier ("initial"), [t.stringLiteral (initial)]));
		var node = path.node;
		if (!node.decorators) node.decorators = [];
		if (decorators.initial) node.decorators.push (init_deco);
	    },
	    exit (path) {
		current_class_name = null;
	    }
	},
	ClassBody (path) {
	    if (!tracker.local) return;
	    if (class_name && current_class_name != class_name) return;
	    //console.log (path.node.body);
	    const init_prop =
		  {type: "ClassProperty",
		   static: false,
		   key: {type: "Identifier", name: _state},
		   computed: false,
		   typeAnnotation: {type: "TSTypeAnnotation",
				    typeAnnotation: {type: "TSStringKeyword"}},
		   value: t.stringLiteral (initial)}
	    path.node.body = [init_prop].concat (path.node.body);
	},
	ClassMethod (path) {
	    const name = class_name ? (current_class_name + "." + path.node.key.name) : path.node.key.name;
	    if (!handler_names.includes (name)) return;

	    const conds = gen_conditions (guards, name);
	    const decos = gen_decorators (conds);
	    //console.log (name, JSON.stringify (decos));

	    if (!path.node.decorators) path.node.decorators = [];
	    if (decorators.expects) path.node.decorators.push (decos.pre.deco);
	    if (decorators.update) path.node.decorators.push (decos.update.deco);
	    if (decorators.ensures) path.node.decorators.push (decos.post.deco);
	    if (decorators.transitions) path.node.decorators.push (decos.transitions.deco);

	    // comments
	    if (!path.node.leadingComments) path.node.leadingComments = [];
	    path.node.leadingComments.push ({type: "CommentLine", value: " expects: " + conds.pre.str});
	    path.node.leadingComments.push ({type: "CommentLine", value: " ensures: " + conds.post.str});
	},

	FunctionDeclaration (path) {
	    // path.node = {id, params, body}
	    const name = path.node.id.name;
	    if (!handler_names.includes (name)) return;

	    const conds = gen_conditions (guards, name);

	    // comments
	    if (!path.node.leadingComments) path.node.leadingComments = [];
	    path.node.leadingComments.push ({type: "CommentLine", value: " expects: " + conds.pre.str});
	    path.node.leadingComments.push ({type: "CommentLine", value: " ensures: " + conds.post.str});
	}
    }
}

// default
export default function ()
{
    console.log ("default function callled");
}
