# Summary
[*dsl4sc*](https://github.com/ldltools/dsl4sc) is a domain-specific language,
based on [LDL<sub>f</sub>](https://www.cs.rice.edu/~vardi/),
for defining and verifying state transition models for event processing.

Each model in dsl4sc has the following unique characteristics:

- It can be defined in terms of event _protocol_, _properties_, and ECA _rules_
  - event protocol: _regular_ pattern of acceptable event sequences
  - propery: LDL<sub>f</sub> formula that globally holds as an invariant condition.
  - [_ECA rule_](https://en.wikipedia.org/wiki/Event_condition_action):
    triple of event, condition, and action
- It has a clear semantics in terms of LDL<sub>f</sub>.
- it can be verified statically and formally against arbitrary requirements
  that are also defined in dsl4sc
- It can derive an executable statechart in [SCXML](https://www.w3.org/TR/scxml/)

# Example: [ping\_pong](examples/ping\_pong/README.md) -- You say "ping" and I say "pong"

Consider 2 sorts of events, _ping_ and _pong_:
The following two dsl4sc models both track ping and pong events
emitted in an alternating manner.  

(1) ping\_pong1.rules -- _protocol-only_ model

```
protocol (ping; pong)* ;; // ping followed by pong (repeats 0 or more times)
```

(2) ping\_pong2.rules -- model that includes _protocol_, _property_, and _rule_ parts

```
protocol  
  (ping + pong)*;; // ping or pong (repeats 0 or more times)  
property  
  !pinged; // not "pinged", initially  
rule  
  on ping when !pinged ensure pinged; // pinged after processing ping  
  on pong when pinged ensure !pinged;  
```

(3) model checking

ping\_pong1 accepts only ping or pong -- (ping; pong)* |= (ping + pong)*

```
$ echo 'protocol (ping + pong)*;;' | rulesmc -m ping_pong1.rules /dev/stdin  
claim holds
```

ping\_pong2 includes ping\_pong1

```
$ rulesmc -m ping_pong2.rules ping_pong1.rules --reachability  
reachable
```

ping\_pong3, defined as the _conjunction_ of ping\_pong1 and ping\_pong2,
entails that _!pinged_ and _pinged_ hold in an alternating manner.
-- ping\_pong3 |= <({!pinged}; {pinged})*> !pinged

```
$ cat ping_pong1.rules ping_pong2.rules > ping_pong3.rules  
$ echo 'property <({!pinged}; {pinged})*> !pinged;' | rulesmc -m ping_pong3.rules /dev/stdin  
claim holds
```

Check out [more examples](examples/README.md) if you are interested.  

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
