# v0.12.0rc

- [dsl4sc] pack all modules into "Dsl4sc"

- [rules2scxml] SCXML event names can be overridden  
  when a rule is of the form "on e { e' } ...", "e'" turns out to be the event name.  
  refer to tests/test22.
  (2dd2d3e)

## fixes
- [rules2ldl] p18n of _nat (n)_ where _n_ is not a power of 2.
  (7b85a4c)

# v0.11.1 (2018-12-30)

- [rules2ldl] natural numbers  
  support all arithmetic (add/sub/mul/div) and comparison (eq/ne/gt/lt/ge/le) operations.

# v0.11.0 (2018-12-03)

- [rules2ldl] natural numbers  
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
