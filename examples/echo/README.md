# [echo](echo.rules)

(1) [*echo.rules*](echo.rules) is defined in *dsl4sc* as follows.

&ensp; **protocol**  
&ensp;&ensp; echo; echo\*;;  
&ensp;&ensp;&ensp; // sequence of *echo* events (repeated 1 or more times)  
&ensp; **rule**  
&ensp;&ensp; **on** echo **do** { console.log (_event.data); };  
&ensp;&ensp;&ensp; // upon each incoming *echo* event, print out its parameter string

(2) [*echo.scxml*](echo.scxml) can be generated
from [*echo.rules*](echo.rules) as a semantically-equivalent
statechart in the [SCXML](https://www.w3.org/TR/scxml/) format.

run: `rules2scxml echo.rules -o echo.scxml`

![statechart](echo.svg)

(3) [echo.in](echo.in) is defined as an input scenario,
which includes the following input events

&ensp; {"event" : {"name" : "echo", "data" : "hello"}}  
&ensp; {"event" : {"name" : "echo", "data" : "world"}}

(4) To test *echo.scxml* against *echo.in*
using [scxmlrun](https://github.com/ldltools/scxmlrun), our SCXML interperter,

run: `scxmlrun echo.scxml echo.in`

The following messages should appear on your terminal.

&ensp; hello  
&ensp; world  
