Each input spec (`S.rules`) is first divided by `rulespp`, a preprocessor,
into the following two parts:

- rule-set w/o code (pure spec in _core dsl4sc_)
- event names and code fragments in xml

Subsequently, these are processed separately and then combined into a single xml file,
from which the resulting scxml file is generated.

![rules2scxml](docs/images/flow.svg?sanitize=true)
