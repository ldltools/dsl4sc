Each DSL4SC program is translated to LDL<sub>f</sub>
through the following representations:

1. [dsl4sc](../docs/grammar/grammar.html) (= core dsl4sc with syntactic sugars)
   - `Rulespp.preprocess : Rules.decl list -> Rules.decl list`
   - `Rules.decls_to_rules : Rules.decl list -> Rules.t`
1. spec (= [core dsl4sc](../docs/grammar/core_grammar.html) without code fragments)
   - `Spec.rules_to_spec : Rules.t -> Spec.t`
1. LDL<sub>f</sub>
   - `Spec2ldl.translate : Spec.t -> Ldl.formula list`
