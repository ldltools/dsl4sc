# toolset

## tools for the _RULES_ language
- rules2scxml  
  usage: rules2scxml _infile_

- specgen  
  usage: specgen [-p] [-t _fmt_] _infile_
  - _infile_ needs to be formatted in _rules_
  - -p: parse-only
  - -t _fmt_: specifies the output format.
    _fmt_ can be either of _json_ (default) and _spec_

## tools for the _SPEC_ language
- spec2ldl  
  usage: spec2ldl [-p] [-t _fmt_] _infile_   
  - _infile_ can be formatted in _spec_ or _json_ (auto-recognized)
  - -p: parse-only
  - -t _fmt_: specifies the output format.
    _fmt_ can be either of _json_ (default), _caml_, and _ldl_

## tools for the _LDL_ language
- ldl2dfa  
  usage: ldl2dfa _infile_

- ldl2mso  
  usage: ldl2mso _infile_

- ldl2re  
  usage: ldl2re [-p] [-t _fmt_] _infile_
  - _infile_ can be formatted in _ldl_ or _json_ (auto-recognized)
  - -t _fmt_: specifies the output format.
    _fmt_ can be either of _json_ (default) and _caml_, and _re_

- ldl2afw  
  usage: ldl2afw [-p] [-t _fmt_] _infile_   
  - _infile_ can be formatted in _ldl_ or _json_ (auto-recognized)
  - -t _fmt_: specifies the output format.
    _fmt_ can be either of _json_ (default) and _caml_

## miscellaneous tools
- re2mso
- mso2dfa
- dfa2scxml
- dfa2dot


# source/intermediate languages
## RULES
- its grammar is defined in __src/rules/rules\_p.mly__
- its abstract syntax is defined in __src/rules/rules.mli__
- __specgen__ generates _spec_ from  _rules_

## SPEC
- [syntax](docs/html/spec_syntax.html)
- refer to "_docs/dsl4sc.pdf_" for the __spec__ syntax/semantics
- its grammar is defined in __src/spec/spec\_p.mly__
- its abstract syntax is defined in __src/spec/spec.mli__
- __spec2ldl__ tranforms _spec_ to _ldl_

## LDL
- [syntax](docs/html/ldl2_syntax.html)
- refer to "_docs/dsl4sc.pdf_" for the __ldl__ syntax/semantics
- its grammar is defined in __src/ldl/ldl\_p.mly__
- its abstract syntax is defined in __src/ldl/ldl.mli__
- __ldl2re__ tranforms _ldl_ to _re_

## RE
- its abstract syntax is defined in __src/ldlsat/re.mli__
- __re2mso__ tranforms _re_ to _mso_

## MSO
- its abstract grammar is defined in __src/ldlsat/mso/mso.mli__
- __mso2dfa__ tranforms _mso_ to _dfa_ (by internally using __mona__)
