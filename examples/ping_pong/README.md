# ping & pong

Two dsl4sc models, [ping](ping.rules) and [pong](pong.rules), 
interact with each other by exchanging _ping_ and _pong_ events.  
Specifically,
starting from a _ping ({"count" : n})_ event arriving at [ping](ping.scxml),
the 2 processes exchange the following events.

- _ping ({"count" : n})_
- _pong ({"count" : n - 1})_
- ...
- _ping ({"count" : 0})_ or _pong ({"count" : 0})_ depending on the parity of _n_
- _quit_

![ping\_pong](ping_pong_sd.svg)


## (1) [*ping*](ping.rules)

The definition of the model includes 2 distinct parts,
namely _protocol_ and _rule_ definition parts.

<details>
  <summary>ping</summary>
  <div><img alt="statechart" src="ping.svg?sanitize=true"/></div>
</details>


### protocol

Each protocol defines a _regular pattern_ of acceptable event sequences.

- Operators such as sequence (;), choice (+), and loop (\*) can be used
  for composing regular patterns.
- Event parameters cannot be included.

```
protocol  
ping; ((pong; ping)* + pong; (ping; pong)*); quit ;;  
  // regular pattern of the incoming events  
  // [operator precedence] high: sequence (;), loop (*), choice (+) :low  
```

The above definition is interpreted as follows.

- Each _run_ (execution) of the model always starts upon a "_ping_" event  
  ("_ping; ..._")
- Subsequently, either of the following 2 cases occurs  
  1. 2 consecutive events, "_pong_" followed by "_ping_", repeat 0 or more times  
     ("_(pong; ping)\*_")
  1. "_pong_" followed by a loop of consecutive "_ping_" and "_pong_"  
     ("_pong; (ping; pong)\*_")
- Lastly, the run terminates when a "_quit_" event arrives  
  ("_...; quit;;_")


### rules (abstract)

Rules define how to respond to incoming events, and
they can carry implementation code.

- [_abstract rules_] rules without implementation code  
  these are for formal verification.
- [_implementation rules_] rules including implementation code  
  these are for generating an executable model.

For the _ping_ model, rules are primarily about how to process each incoming "_ping_" event.  
Upon receiving a "_ping_", it chooses one of the following actions

- raise a "_pong_" event for moving on to the next step
- raise a "_quit_" event for termination

```
rule  
on ping  
raise quit + pong;  
```


### rules (implementation)

Implementation rules carry (JavaScript) code that
look into event parameters
and take proper actions depending on their values.

```
rule  
// event: ping ({count})  
on ping  
raise quit + pong  
{  
    var n = _event.data.count;  
    if (n == 0)  
	SCXML.raise ({name: "quit", data: {die_alone: 0}});  
    else  
	SCXML.raise ({name: "pong", data: {count: (n - 1)}});  
        // note: this event is what the protocol means by "pong"  
}  
// event: ping ({count})  
on pong  
do { SCXML.send ({event: _event, topic: "pong"}); }  
// event: quit ({die_alone})  
on quit  
do { if (_event.data.die_alone == 0) SCXML.send ({event: {name: "quit", data: {die_alone: 1}}, topic: "pong"}); }
```

Note that
each `SCXML.raise` operation in the code has a corresponding dsl4sc `raise` operation, whereas
no `SCXML.send` operation has any such dsl4sc counterpart.
This is since dsl4sc models just take account of incoming events, that is,
events emitted by `SCXML.raise` are received internally and thus recognized
as a part of the model definition, though
those emitted by `SCXML.send` are outbound and not processed by the model itself.


## (2) [*pong*](pong.rules)

### protocol

```
protocol  
(pong; ping)*; pong?; quit ;;
```

<details>
  <summary>pong</summary>
  <div><img alt="statechart" src="pong.svg?sanitize=true"/></div>
</details>


## Formal verification

### claim 1: "_ping; pong; quit_" is an acceptable event sequence for the _ping_ model.

Can two consecutive events of "ping" and "pong" be processed/accepted by [*ping*](ping.rules)?

To verify this formally,
we first define this claim in dsl4sc as the following protocol

```
protocol ping; pong; quit;;
```

Then, by providing this and the model definition for our model checking tool, `rulesmc`,
we positively confirm the claim.

```
$ echo "protocol ping; pong; quit;;" | rulesmc ping.rules -- reachability  
reachable
```

### claim 2: it is always the case that either "_ping_" or "_pong_" repeats until "_quit_" arrives.

In the same way, we first define the claim as follows.

```
protocol (ping + pong)*; quit;;
```

Succeedingly, run the model checker as follows.

```
$ echo "protocol (ping + pong)*; quit;;" | rulesmc ping.rules  
claim holds  
$ echo "protocol (ping + pong)*; quit;;" | rulesmc pong.rules  
claim holds
```


## Statechart generation and its execution

For brevity,
we just show how to run one of the 2 models.
To run the both together, visit
[this page](https://github.com/ldltools/scxmlrun/examples/ping_pong/README.md).


## generation of SCXML statecharts

Each of [ping](ping.rules) and [pong](pong.rules) can be translated into SCXML
using our `rules2scxml` tool.

```
$ rules2scxml ping.rules -o ping.scxml --auto-exit  
$ rules2scxml pong.rules -o pong.scxml --auto-exit
```

Remark:  
the `--auto-exit` option indicates that the model definition includes
its own termination event ("_quit_") and
needs no extra event to be auto-generated.


## statechart execution

Events are JSON objects of the following form:

```
{"event": {"name": "ping", "data": {"count": 2}}}  
{"event": {"name": "pong", "data": {"count": 1}}}
```

To run either _ping_ or _pong_ statechart, we just need to supply input events.

```
$ cat <<EOF | scxmlrun ping.scxml | jq -c '.event.name, .event.data'  
{"event": {"name": "ping", "data": {"count": 2}}}  
{"event": {"name": "ping", "data": {"count": 0}}}  
EOF  
"pong"  
{"count":1}  
"quit"  
{"die_alone":1}
```

This corresponds with a sequence of:
_ping ({"count": 2}); pong ({"count": 1}); ping ({"count": 0}); quit_
