# Summary
[*dsl4sc*](https://github.com/ldltools/dsl4sc) is a domain-specific language,
based on [LDL<sub>f</sub>](https://www.cs.rice.edu/~vardi/),
primarily targeted at defining and verifying state transition models for event processing.

Each model in dsl4sc has the following unique characteristics:

- It can include the following 3 different sort of declarations:
  - event **protocol**:
    _regular_ pattern of acceptable event sequences
  - logical **property**:
    temporal LDL<sub>f</sub> formula the model should meet.
  - ECA **rule**:
    triple of _event_, _condition_, and _action_
    that defines how to respond to the specified incoming event.
- It has a clear semantics in terms of the LDL<sub>f</sub> formalism.
- it can be verified statically and formally against arbitrary requirements
  that are also defined in dsl4sc
- It can derive an executable statechart in [SCXML](https://www.w3.org/TR/scxml/)

# Example: [deuce](examples/deuce/README.md) -- Sharapova vs. Williams

Consider 2 professional tennis players, fictitiously called Sharapova and Williams,
are fighting for winning a game.
They are currently at deuce and either needs to win by 2 points ahead of her opponent.

To model what can happen through the game,
let us start with the following protocol definition

```
protocol  
(sharapova + williams)*; game ;;
```

This declares that, until the "_game_" event is emitted when the game is won,
either of the "_sharapova_" and "_williams_" events, indicating which player wins a point,
repeats 0 or more times.  

Succeedingly, we add the following _property_ that defines
the players are initially at deuce and either will win the game in the end.

```
variable  
state : nat(4); // nat(4) = {0,1,2,3}  
  // 0: deuce, 1: advantage with sharapova, 2: advantage with williams, 3: game won  
property  
state = 0 & [](last -> state = 3); // initial and final conditions
```

Lastly, we add the following rules that define how each event is processed.

```
rule  
on sharapova when state = 0 ensure state = 1;  
on sharapova when state = 1 raise game ensure state = 3;  
on sharapova when state = 2 ensure state = 0;  
...
```

Here, the first rule defines that
if a "_sharapova_" event is emitted when the game is at deuce (state = 0),
then it turns out that the advantage is with the player called Sharapova (state = 1).

By combining all of these, we derive a state-transition model illustrated as follows.

![statechart](examples/deuce/deuce.svg)

Once a model is defined, we can formally verify the model in various ways.
Take a look at [this](examples/deuce/README.md) for the detail.

You can also check out [more examples](examples/README.md) if you are interested.  


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
- ocaml packages: ocamlfind, sedlex, menhir, yojson, ppx\_deriving, ppx\_deriving\_yojson, xml-light, z3  
  for each of these packages,  
  run: `opam install <package>`
- [ldlsat](https://github.com/ldltools/ldlsat) (v1.0.4 or higher)  
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
