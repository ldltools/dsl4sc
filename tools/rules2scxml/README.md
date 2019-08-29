Each process definition (`P.rules`) is first divided by `rulespp`, a preprocessor,
into the following two parts:

- `P.spec`: process definition w/o code in _core dsl4sc_, which we call _specification_
- `P.xml`: code fragments, along with event names, in xml

Subsequently, these are processed separately and then combined into a single xml file (`P_dfa.xml`),
from which the resulting scxml file (`P.scxml`) is generated.  
To obtain intermediate files, instead of the final scxml file,
run `rules2scxml` with `--until <stage>` option where `<stage>` can be either of
`spec`, `ldl`, `mso`, .., `dfa0`, .., `dfa3`.
See the man page (`man rules2scxml`) for the detail.

![rules2scxml](docs/images/flow.svg?sanitize=true)
