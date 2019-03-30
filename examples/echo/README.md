# [echo](echo.rules)

## dsl4sc script: [*echo*](echo.rules)

The script is defined in *dsl4sc* as follows.

&ensp; **protocol**  
&ensp;&ensp; echo\*; quit;;  
&ensp;&ensp;&ensp; // sequence of *echo* events (repeated 0 or more times) terminated by *quit*  
&ensp; **rule**  
&ensp;&ensp; **on** echo **do** { console.log (_event.data); };  
&ensp;&ensp;&ensp; // upon each incoming *echo* event, print out its parameter string

The **protocol** part of the script defines a _regular pattern_ of acceptable events,
whereas the **rule** part defines how to process each incoming event.


## Formal verification

## claim 1: _echo_ can repeat 1 or more times 

This is about finding an acceptable event sequence that includes 1 or more _echo_ events,
and we obtain a positive result as follows.

```
$ echo 'protocol echo; echo*; quit;;' | rulesmc echo.rules --reachability  
reachable
```

## claim 2: _echo_ always repeats 1 or more times 

If we make a stricter claim -- it is always the case that _echo_ repeats 1 or more times,
then it turns out not to hold.

```
$echo 'protocol echo; echo*; quit;;' | rulesmc echo.rules  
claim does not hold
```

## claim 3: either _echo_ or _quit_ always repeats 1 or more times 

Then, if we relax the above and make a weaker claim
-- it's not _echo_ but either _echo_ or _quit_ that always repeats 1 or more times,
then it holds this time.

```
$ echo 'protocol (echo + quit)*;;' | rulesmc echo.rules  
claim holds
```


## Statechart generation and its execution

### Generation of statechart: [*echo.scxml*](echo.scxml)

From [*echo.rules*](echo.rules), our _rules2scxml_ tool included in this repository generates
an equivalent statechart that conforms to the [SCXML](https://www.w3.org/TR/scxml/) specification.

```
$ rules2scxml echo.rules -o echo.scxml --auto-exit
```

![statechart](echo.svg)


### its execution

We can run the statechart as an executable program
by using our companion tool, called
[scxmlrun](https://github.com/ldltools/scxmlrun).

To run the statechart with a particular input event sequence,
we just need to invoke [scxmlrun](https://github.com/ldltools/scxmlrun) in the following manner.

```
$ cat <<EOF | scxmlrun echo.scxml  
{"event" : {"name" : "echo", "data" : "hello"}}  
{"event" : {"name" : "echo", "data" : "world"}}  
{"event" : {"name" : "quit"}}  
EOF  
hello  
world  
```

