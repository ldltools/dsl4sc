//

import traverse from '@babel/traverse';
import * as t from '@babel/types';
const assert = require ('assert');

// tracker names (default: "_state" and "_state_pre")
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
	      "null")
    const next_exp =
	  transitions.reduceRight (
	      (rslt, elt) =>
		  t.conditionalExpression (
		      t.binaryExpression ("==", t.identifier (_state),
					  t.stringLiteral (elt.state)),
		      t.stringLiteral (elt.target),
		      rslt),
	      //t.callExpression (t.identifier ("assert"), [t.booleanLiteral (false)])
	      t.nullLiteral ());

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

function add_conditions (path, conds, options)
{
    assert (path.node.type == "FunctionDeclaration" || path.node.type == "ClassMethod");
    //console.log ("[add_conditions]", path.node.id.name);

    assert (path.node.body.type == "BlockStatement");
    var body = path.node.body.body;

    if (options.conditions.includes ("pre"))
    {
	const pre = t.callExpression (t.identifier ("assert"), [conds.pre.exp]);
	body.unshift (t.expressionStatement (pre));
    }

    if (options.conditions.includes ("update"))
    {
	//const state_pre = t.variableDeclarator (t.identifier (_state_pre), t.identifier (_state));
	//body.push (t.variableDeclaration ("let", [state_pre]));
	const state_pre = t.assignmentExpression ("=", t.identifier (_state_pre), t.identifier (_state));
	body.push (t.expressionStatement (state_pre));
	const update = t.assignmentExpression ("=", t.identifier (_state), conds.next.exp);
	body.push (t.expressionStatement (update));
    }
    if (options.conditions.includes ("post"))
    {
	const post = t.callExpression (t.identifier ("assert"), [conds.post.exp]);
	body.push (t.expressionStatement (post));
    }

    path.traverse ({
	ReturnStatement (path) {
	    let stmts = [path.node];

	    if (options.conditions.includes ("post"))
	    {
		const post = t.callExpression (t.identifier ("assert"), [conds.post.exp]);
		stmts.unshift (t.expressionStatement (post));
	    }
	    if (options.conditions.includes ("update"))
	    {
		const state_pre = t.assignmentExpression ("=", t.identifier (_state_pre), t.identifier (_state));
		const update = t.assignmentExpression ("=", t.identifier (_state), conds.next.exp);
		stmts.unshift (t.expressionStatement (update));
		stmts.unshift (t.expressionStatement (state_pre));
	    }

	    path.replaceWith (t.blockStatement (stmts));
	    path.skip ();	// skip traversal of the generated block statement
	}
    });
}

function add_comments (path, conds)
{
    assert (path.node.type == "FunctionDeclaration" || path.node.type == "ClassMethod");

    let node = path.node;
    if (path.parent.type == "ExportNamedDeclaration") node = path.parent;
    //console.log (JSON.stringify (node));

    // comments
    if (!node.leadingComments) node.leadingComments = [];
    node.leadingComments.push ({type: "CommentLine", value: " expects: " + conds.pre.str});
    node.leadingComments.push ({type: "CommentLine", value: " ensures: " + conds.post.str});
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

function add_decorators (path, decos, options)
{
    assert (path.node.type == "ClassMethod");

    //console.log (name, JSON.stringify (decos));
    if (!path.node.decorators) path.node.decorators = [];

    if (options.js_decorators.includes ("expects")) path.node.decorators.push (decos.pre.deco);
    if (options.js_decorators.includes ("update")) path.node.decorators.push (decos.update.deco);
    if (options.js_decorators.includes ("ensures")) path.node.decorators.push (decos.post.deco);
    if (options.js_decorators.includes ("transitions")) path.node.decorators.push (decos.transitions.deco);
}

function add_trailer (path, spec, code, options, assert_declared = false)
{
    assert (code.js_class != null || options.tracker.global);

    const initial = spec.guards.params.initial;

    if (options.tracker.global)
    {
	// var _state = <initial>, _state_pre
	const tracker_decls =
	      [{
		  type: "VariableDeclarator",
		  id: {type: "Identifier", name: _state},
		  init: {type: "StringLiteral", value: initial}
	      },
	       {
		   type: "VariableDeclarator",
		   id: {type: "Identifier", name: _state_pre},
	       }
	      ]
	path.node.body.push (t.variableDeclaration ("var", tracker_decls));
    }

    if (code.js_class == null)
    {
	assert (options.tracker.global);

	// function _reset () { _state = <initial>; }
	const initializer = options.initializer.name;
	const init_stmt = 
	      t.expressionStatement (t.assignmentExpression ("=", t.identifier (_state),
							     t.stringLiteral (initial)));
	path.node.body.push (t.functionDeclaration (t.identifier (initializer), [],
						    t.blockStatement ([init_stmt])));
	// module.exports._reset = _reset
	const export_exp =
	      t.memberExpression (t.memberExpression (t.identifier ("module"), t.identifier ("exports")),
				  t.identifier (initializer));
	const export_stmt =
	      t.expressionStatement (t.assignmentExpression ("=", export_exp,
							     t.identifier (initializer)));
	path.node.body.push (export_stmt);
    }


    // assert
    if (!assert_declared)
    {
	// const assert = require ('assert')
	const assert_decl =
	      t.variableDeclarator (t.identifier ("assert"),
				    t.callExpression (t.identifier ("require"),
						      [t.stringLiteral ("assert")]));
	path.node.body.push (t.variableDeclaration ("const", [assert_decl]));
    }
}

// conf = {spec: {location, guards: {params, body}, ..},
//         code: {location, class, handlers, ..},
//         options: {guards, decorators, tracker}
//        }
export function visitor (conf)
{
    // ----------
    // spec
    // ----------
    const spec = conf.spec
    // location, guards
    const event_guards_params = spec.guards.params;
    const event_guards = spec.guards.body.filter ((elt) => elt != null);
    //console.log (JSON.stringify (event_guards));
    const events = event_guards.map ((elt) => elt.event);
    //console.log (JSON.stringify (events));
    const initial = spec.guards.params.initial;

    // ----------
    // code
    // ----------
    // location, class, handlers
    const code = conf.code;
    // handlers = [{name, event}, ..]
    var handlers = conf.code.handlers ? conf.code.handlers : [];
    handlers = handlers.filter ((elt) => elt != null);
    handlers = handlers.concat (events.map ((elt) => { return {name: elt, event: elt} }));
    if (code.js_class)
	handlers = handlers.map ((elt) => {
	    const name = (elt.name.includes (".")) ? elt.name : (code.js_class + "." + elt.name);
	    return {name: name, event: elt.event} });
    //console.log ("handlers:", JSON.stringify (handlers))
    const handler_names = handlers.map ((elt) => { return elt.name })

    // ----------
    // mappings
    // ----------
    var guards = event_guards.map ((elt) => {
	// elt = [{event, transitions}, ..]
	const event = elt.event;
	assert (events.includes (event));
	const transitions = elt.transitions.filter ((elt) => elt != null);
	const handler = handlers.find ((elt) => { return elt.event == event }).name;
	return {handler: handler, transitions: transitions};
    })
    //console.log ("guards:", JSON.stringify (guards));

    // ----------
    // options
    // ----------
    // guards, decorators, trakcer
    const options = conf.options;
    //console.log ("options:", options);
    // decorators
    //console.log ("decorators:", decorators);

    // tracker
    _state = options.tracker.name;
    _state_pre = _state_pre = _state + "_pre";
    if (!code.js_class) {
	assert (options.tracker.global != false);
	options.tracker.global = true;
    }
    if (code.js_class && options.tracker.global == null)
	options.tracker.global = true;

    var current_class_name = null;
    var assert_declared = false;

    return {
	Program : {
	    enter (path) {},
	    exit (path) {
		add_trailer (path, spec, code, options, assert_declared);
	    }
	},
	// NOTE: decorators CANNOT be attached to functions
	FunctionDeclaration (path) {
	    if (code.js_class) return;

	    // path.node = {id, params, body}
	    const name = path.node.id.name;
	    if (!handler_names.includes (name)) return;

	    const conds = gen_conditions (guards, name);

	    // conditions
	    add_conditions (path, conds, options)

	    // comments
	    add_comments (path, conds)
	},

	ClassDeclaration : {
	    enter (path) {
		current_class_name = path.node.id.name;
		if (!code.js_class || current_class_name != code.js_class) return;
		if (!options.js_decorators) return;
		// initial
		if (options.js_decorators.includes ("initial")) 
		{
		    const init_deco =
			  t.decorator (t.callExpression (t.identifier ("initial"), [t.stringLiteral (initial)]));
		    let node = path.node;
		    //if (path.parent.type == "ExportNamedDeclaration") node = path.parent;
		    // ** this causes an error
		    if (!node.decorators) node.decorators = [];
		    node.decorators.push (init_deco);
		}
	    },
	    exit (path) {
		if (current_class_name == code.js_class)
		{
		    let body = path.node.body;
		    assert (body.type == "ClassBody");
		    // method _reset () { _state = <initial>; }
		    const init_stmt = 
			  t.expressionStatement (t.assignmentExpression ("=", t.identifier (_state),
									 t.stringLiteral (initial)));
		    const initializer = options.initializer.name;
		    body.body.push (t.classMethod ("method", t.identifier (initializer), [],
						   t.blockStatement ([init_stmt])));
		}

		current_class_name = null;
	    }
	},
	ClassBody (path) {
	    if (!code.js_class || current_class_name != code.js_class) return;
	    if (options.tracker.global) return;

	    // insert tracker ("_state") declaration (local)
	    if (!options.tracker.global
		&& (!options.js_decorators || !options.js_decorators.includes ("initial")))
	    {
		//console.log (path.node.body);
		const init_prop1 =
		  {type: "ClassProperty",
		   static: false,
		   key: {type: "Identifier", name: _state},
		   computed: false,
		   //typeAnnotation: {type: "TSTypeAnnotation", typeAnnotation: {type: "TSStringKeyword"}},
		   value: t.stringLiteral (initial)
		  }
		const init_prop2 = t.classProperty (t.identifier (_state_pre));
		path.node.body.unshift (init_prop2);
		path.node.body.unshift (init_prop1);
	    }
	},
	ClassMethod (path) {
	    if (!code.js_class || current_class_name != code.js_class) return;

	    const name = code.js_class ? (current_class_name + "." + path.node.key.name) : path.node.key.name;
	    if (!handler_names.includes (name)) return;

	    // conditions
	    const conds = gen_conditions (guards, name);
	    add_conditions (path, conds, options)

	    // comments
	    add_comments (path, conds)

	    // decorators
	    if (!options.js_decorators) return;

	    const decos = gen_decorators (conds);
	    add_decorators (path, decos, options)
	}
    }
}

// default
export default function ()
{
    console.log ("default function callled");
}
