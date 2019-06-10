# v0.12.0 (2019-??-??)

- **new** [safeguard] attach to existing EcmaScript code a set of pre/post conditions.
  (33c54bc)
- [modelgen] switch the internal SAT solver from toysat to [z3](https://github.com/Z3Prover/z3).
  (015057d)

# v0.11.2 (2019-03-30)

- [dsl4sc] pack all ocaml modules into "Dsl4sc"
- [rules2scxml] SCXML event names can be overridden  
  when a rule is of the form "on e1 { e2 } ...", it is "e2", instead of "e1", that appears in the generated SCXML.  
  refer to tests/test22.
  (2dd2d3e)

## fixes
- [rulespp] recognize term variables in modal paths correctly.
  (b098d28)
- [rulespp] recognize term variables in "preserve" rules correctly.
  (39bcf14)
- [rules2ldl] p18n of _nat (n)_ where _n_ is not a power of 2.
  (7b85a4c)

# v0.11.1 (2018-12-30)

- [rules2ldl] natural numbers  
  support all arithmetic (add/sub/mul/div) and comparison (eq/ne/gt/lt/ge/le) operations.

# v0.11.0 (2018-12-03)

- **new** [rules2ldl] support natural numbers  
  natural numbers (literals and variables) are supported.
  `variable x : nat (n)` declares a variable `x` that ranges over 0, ..., n - 1.
  expressions such as `x = 1`, `x = y`, and `x + 1 = y + 2` are recognized as propositions.
- [rulespp] epsilon events  
  epsilon events (`_epsilon`) can appear in protocol definitions.  
  `p?`, where `p` denotes a protocol, can also appear as a shorthand for `p + _epsilon`.

# v0.10.0 (2018-11-08)

- [rules2scxml] monitor generation  
  `rules2scxml --monitor` generates a statechart that works as a monitor.
  the primary difference that this option makes is:
  each incoming event at a state that no transition can process causes an error,
  instead of being ignored and discarded
  (which is the default behavior defined in the SCXML specification).
- [rules2scml] _implementation_ section  
  dsl4sc scripts can now include "implementation { .. }" sections
  for carrying implementation-specific code.
- [rules2scxml] dsl4sc namespace (https://github.com/ldltools/dsl4sc)  
  elements of intermediate xml files generate by `rules2scxml` belong to this namespace.
- [rules2scxml] _\_initial_ event is no longer generated.


# v0.9.0 (2018-10-01)

initial public release.
