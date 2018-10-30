# Features

- rulespp  
  - preprocess  
    decl list -(preprocess)-> decl list
  - xml/map generation  
    decl list -(preprocess)-> decl list  
    -(decls\_to\_rules)-> Rules.t -(Rules.print\_rules\_in\_xml)-> xml

- rules2ldl: preprocess & spec generation  
  Rules.decl list -(Rulespp.preprocess)-> Rules.decl list  
  -(Rulespp.decls\_to_rules)-> Rules.t  
  -(Spec.rules\_to\_spec)-> Spec.t  
  -(Spec.translate)-> Ldl.formula list * (string * Ldl.formula) list

# Steps
1. [rulespp] expand rules (rules.t -> rules.t)
  - expand proposition/event array decls (during the parsing stage)
  - add missing proposition/event/label decls
  - instantiate property/protocol/rule macros

- [rulespp] generate spec
  - strip off (JavaScript) code fragments

- [rulepp] convert rules into xml (for scxml generation)
