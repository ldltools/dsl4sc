# Summary
[*dsl4sc*](https://github.com/ldltools/dsl4sc) is a domain-specific language,
based on [LDL<sub>f</sub>](https://www.cs.rice.edu/~vardi/),
for defining, verifying, and running state transition models for event processing.

Each model defined in dsl4sc has the following unique characteristics:

- Its building blocks -- _protocols_, _properties_, and
  [_ECA rules_](https://en.wikipedia.org/wiki/Event_condition_action) --
  are independent of each other.  
  You can regard each protocol, property, or rule as a separate model,
  while the whole model as a conjunctive composition of these building-block models.
- It has a clear semantics in LDL<sub>f</sub>.
- it can be verified statically and formally against arbitrary requirements
  that are also defined in dsl4sc
- It can derive an executable statechart in [SCXML](https://www.w3.org/TR/scxml/)

# Example: [ping\_pong](examples/ping\_pong/README.md) -- You say "ping" and I say "pong"

Let us consider 2 sorts of events, _ping_ and _pong_, which are emitted in an
alternating manner.  
We can define the following dsl4sc models that capture this behavior and
perform their verification and translation to executable forms.

(1) _model1_ includes a single _protocol_ definition in dsl4sc as follows.

&ensp; **protocol**
&ensp; (ping; pong); (ping; pong)*;; // ping followed by pong (1 or more times)

(2) _model2_ introduces _waiting_ to keep track of whether a _pong_ is awaited

&ensp; **property**  
&ensp; &ensp; !waiting; // initially waiting is set to false (pong is not awaited)  
&ensp; **rule**  
&ensp; &ensp; **on** ping **when** !waiting **do** { console.log ("You say ping"); } **ensure** waiting;  
&ensp; &ensp; &ensp; // ping turns waiting from false into true  
&ensp; &ensp; **on** pong **when** waiting **do** { console.log ("I say pong"); } **ensure** !waiting;  
&ensp; &ensp; &ensp; // pong turns waiting from true into false

Note that, since no protocol is defined,
_model2_ assumes that `ping` and `pong` events may arrive in any order.

(3) _model3_ by merging (1) and (2) into one.

(4) Requirements for the models.

1. **protocol** (ping + pong)*; pong;;  
   either ping or pong may repeat any times, though the last event is always pong
1. **property** [{true}\*][{waiting}] !waiting;  
   at any point, waiting always changes from true to false

The first requirement is met by _model1_ and _model3_, while
the second one is met by _model2_ and _model3_.

(5) SCXML generation and execution

Models can be translated into SCXML.
When `ping` and `pong` are emitted to the SCXML statechart generated from _model3_,
the following messages appear on the console.

```
You say ping  
I say pong
```

For more detail,
please take a look at [these examples](examples/README.md).

# Installation on Docker

- run `docker build --target builder -t ldltools/ldlsat-dev .` in the [ldlsat](https://github.com/ldltools/ldlsat) directory
- run `docker build -t ldltools/dsl4sc .` in this directory

# Installation on Debian/Ubuntu
## Prerequisites
- [ocaml](https://ocaml.org) (v4.05 or higher. tested with 4.07.0)  
  run: `apt-get install ocaml`  
  Alternatively, you can install a particular version of the compiler using opam  
  run: `opam switch 4.07.0` for example
- [opam](https://opam.ocaml.org) (ocaml package manager)  
  run: `apt-get install opam`
- ocaml packages: ocamlfind, sedlex, menhir, yojson, ppx\_deriving, ppx\_deriving\_yojson, xml-light  
  for each of these packages,  
  run: `opam install <package>`
- [ldlsat](https://github.com/ldltools/ldlsat)  
  run: `git clone https://github.com/ldltools/ldlsat`  
  build & install the tool by running `make && make install` in the top directory.  
  By default, its library modules will be installed to `/usr/local/lib/ldlsat`.
- [mona](http://www.brics.dk/mona/) (v1.4)  
  run: `wget http://www.brics.dk/mona/download/mona-1.4-17.tar.gz`  
  expand the archive, and build/install the tool as is instructed.
- [xqilla](http://xqilla.sourceforge.net/) and [xmllint](http://xmlsoft.org/)  
  run: `apt-get install xqilla libxml2-utils`
- [scxmlrun](https://github.com/ldltools/scxmlrun) (optional, for running/testing generated SCXML files)  
  run: `git clone https://github.com/ldltools/scxmlrun`  
  build & install the tool by running `make && make install` in the top directory.  
  By default, the binaries will be installed into `/usr/local/bin`.
- [graphviz](http://www.graphviz.org/) (optional)  
  run: `apt-get install graphviz`

## Build
- run `make && make install` in the top directory  
  Tools will be created and installed into `/usr/local/bin`.  
  To change the installation directory,
  run `make PREFIX=<prefix> install` instead (default: `PREFIX=/usr/local`).

# Installation on macOS (Darwin)
In addition to the tools listed above, you also need the following GNU tools:

- GNU common utilities  
  run: `brew install coreutils debianutils`
- GNU sed/awk  
  run: `brew install gnu-sed gawk`
- GNU make (v4.1 or higher)  
  run: `brew install remake`  
  and build with `MAKE=remake remake` instead of `make`

# Testing
- run: `make -C tests test`  
  run test cases using `rulessat`, `rulesmc`, and `rules2scxml`.
- run: `make -C tests scxml`  
  SCXML files will be generated from rules definitions and stored into `tests/out`
