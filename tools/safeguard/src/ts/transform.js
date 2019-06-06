//

// generate pre/post pair
function gen_conditions (guards, name)
{
    const guard = guards.find ((elt) => elt.event == name);
    const transitions = guard.transitions.filter ((elt) => elt != null)
    //console.log (transitions);

    const pre =
	  transitions.slice (1).reduce (
	      (rslt, elt) => rslt + " || _state == \"" + elt.state + "\"",
	      "_state == \"" + transitions[0].state + "\"");
    const next =
	  transitions.reduce (
	      (rslt, elt) =>
		  rslt + "((_state == \"" + elt.state + "\") ? \"" + elt.target + "\" : ",
	      "")
	  + "assert (false)"
	  + transitions.reduce ((rslt, elt) => rslt + ")", "");

    console.log ("handler:", name);
    console.log ("pre:", pre);
    console.log ("next:", next);
    console.log ();

    return {pre: pre, next: next};
}

const transform = (spec) => {
    //console.log ("transform called");
    const guards = spec.guards.filter ((elt) => elt != null);
    const events = guards.map ((elt) => elt.event);
    //console.log (events);

    return {
	Identifier (path) {
	    //console.log (path.node.name);
	},

	FunctionDeclaration (path) {
	    const name = path.node.id.name;
	    if (!events.includes (name)) return;

	    const conds = gen_conditions (guards, name);

	    // path = {id, params, body}
	    //path.node.id.name = "foo";
	},

	ClassMethod (path) {
	    const name = path.node.key.name;
	    if (!events.includes (name)) return;

	    const conds = gen_conditions (guards, name);

	    // path = {id, params, body}
	    //path.node.id.name = "foo";
	}

    }
}

module.exports = transform;
