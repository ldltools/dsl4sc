Each input contract is first divided by `rulespp`, a preprocessor, into the following two parts:

- contract w/o code (_spec_)
- event names and code fragments in xml

Subsequently, these are processed separately and then merged into a single xml file,
from which the resulting scxml file is generated.

![rules2scxml](docs/images/flow.svg?sanitize=true)
