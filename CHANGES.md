# v0.12.0rc

- [rulespp] natural numbers  
  support all arithmetic operations (add/sub/mul/div) and comparison operators (eq/ne/gt/lt/ge/le).

# v0.11.0 (2018-12-03)

- [rulespp] natural numbers  
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
