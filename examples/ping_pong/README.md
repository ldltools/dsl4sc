# ping pong

## (1) [*ping\_pong2.rules*](ping_pong2.rules) is defined in *dsl4sc* as follows.

&ensp; **protocol**  
&ensp;&ensp; (ping; pong)\*;;  
&ensp;&ensp;&ensp; // pairs of *ping* and *pong* events (repeated 0 or more times)  
&ensp; **rule**  
&ensp;&ensp; **on** ping  
&ensp;&ensp; **do** **raise** pong { console.log ("ping", \_event.data); \_\_raiseEvent ("pong", \_event.data); };  
&ensp;&ensp;&ensp; // Each incoming *ping* event is responded with _pong_  
&ensp;&ensp;&ensp; // Note: "**raise** pong" (spec) in *dsl4sc* needs to be _implemented_ within the "{...}" block  
&ensp;  
&ensp;&ensp; **on** pong  
&ensp;&ensp; **do** { console.log ("pong", \_event.data); };  

## (2) [*ping\_pong2.rules*](ping_pong2.rules) can be statically verified against different properties.

### verification 1: reachability of "_ping; pong_"

Can two consecutive events of "ping" and "pong" be processed/accepted by [*ping\_pong2.rules*](ping_pong2.rules)?

First, this property is defined as [ping\_pong.prop1](ping_pong.prop1) as follows:

**protocol** ping; pong;;

Then, run our dsl4sc model checker _rulesmc_ to verify if this holds or not,
and check out that the answer is positive.

```
$ rulesmc --model ping_pong2.rules ping_pong.prop1 -- reachability  
reachable
```

### verification 2: logical entailment

For any event sequence, does it include only "_ping_" or "_pong_"?  
This property is defined as [ping\_pong.prop2](ping_pong.prop2) as follows:

**protocol** (ping + pong)*;;

Then, run _rulesmc_ to verify if this holds or not.

```
$ rulesmc --model ping_pong2.rules ping_pong.prop2  
claim holds
```

## (3) To generate [*ping\_pong2.scxml*](ping_pong2.scxml) from [*ping\_pong2.rules*](ping_pong2.rules), run _rules2scxml_

run: `rules2scxml ping_pong2.rules -o ping_pong2.scxml`

![statechart](ping_pong2.svg)

## (4) [ping\_pong2.in](ping_pong2.in) is defined as an input scenario,
which includes the following input events

&ensp; {"event" : {"name" : "ping", "data" : 1}  
&ensp; {"event" : {"name" : "ping", "data" : 2}

##(5) To test *ping\_pong2.scxml* against *ping\_pong2.in*, run [_scxmlrun_](https://github.com/ldltools/scxmlrun), our SCXML interperter

run: `scxmlrun ping_pong2.scxml ping_pong2.in`

The following messages should appear on your terminal.

&ensp; ping 1  
&ensp; pong 1  
&ensp; ping 2  
&ensp; pong 2  

