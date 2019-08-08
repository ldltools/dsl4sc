Each input spec (`S.rules`) is first divided by `rulespp`, a preprocessor,
into the following two parts:

- `S.spec`: specification (w/o code) in _core dsl4sc_
- `S.xml`: event names and code fragments in xml

Subsequently, these are processed separately and then combined into a single xml file `S_dfa.xml`,
from which the resulting scxml file is generated.  
To obtain intermediate files, instead of the final scxml file,
run `rules2scxml` with `--until <stage>` option where `<stage>` can be either of
`spec`, `ldl`, `mso`, .., `dfa0`, .., `dfa3`.
See the man page (`man rules2scxml`) for the detail.

![rules2scxml](docs/images/flow.svg?sanitize=true)
