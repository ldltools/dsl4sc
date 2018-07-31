# [echo](echo.rules)

*echo.rules* is defined as follows.

```
protocol
 echo; echo*;;  // seq of "echo" events (1 or more times)
rule
 on echo do { console.log (_event.data[0]); }; // print out the param of the incoming "echo" event
```

To generate [*echo.scxml*](examples/echo/out/echo.scxml),
run `make -C examples/echo scxml`.

&ensp;&ensp;(*diagram to be filled in*)

To run this diagram as an executable code, try `make -C examples/echo run`, which transmits 2 *echo* events -- echo("hello") and echo ("world") -- and the code consumes them and prints out the following lines of text on your terminal.

&ensp; hello  
&ensp; world  
