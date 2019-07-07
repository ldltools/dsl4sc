## rules2scxml

Script for translation to [SCXML](https://www.w3.org/TR/scxml/).
For the detail, refer to [this](rules2scxml/README.md).

### rules2dfa

Script for translation from DSL4SC to DFA.

### modelgen

This utility program plays a primary role to turn a DFA into a state machine with labels (i.e., event names).
The name "dfa2dfa" is a bit misleading in that sense, though its output (in xml) is still defined as a DfA.
[rules2scxml]() internally uses this.
For the detail, refer to [this](modelgen/README.md).

## safeguard

In contrast to rules2scxml which  generates executable code from DSL4SC definition,
safeguard deals with _existing_ executable code (in EcmaScript):
given an executable code, safeguard enforces requirements, defined in DSL4SC, on the code.
