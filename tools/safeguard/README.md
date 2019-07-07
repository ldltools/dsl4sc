## summary

Given a set of event handler functions in TypeScript
and their formal specification in dsl4sc,
_safeguard_ generates pre/post conditions for the handlers and
attach each pair of the conditions to the corresponding handler.

## usage

```
safeguard <code_file> [--spec <spec_file>]
```

## input to _safeguard_

- `<code_file>`  
  `<code_file>` includes a set of event handler functions in TypeScript

- `<spec_file>`
  formal specification of the handlers in dsl4sc.

  [Note]
  - `<spec_file>` can be formatted in either dsl4sc or scxml.
  - In case `<spec_file>` is not explicitly passed, _safeguard_ tries auto-detection.


## testing

See [this](tests/README.md).
