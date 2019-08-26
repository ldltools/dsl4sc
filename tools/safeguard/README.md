## summary

Given (1) a JavaScript program that includes a set of event handler functions
and (2) their formal specification in dsl4sc,
_safeguard_ generates pre/post-conditions for the handlers in the program, and
then outputs a new JavaScript program with these conditions attached with.

## example [ping_pong](../../examples/ping_pong)





## usage

```
safeguard <prog_file> [--spec <spec_file>]
```

## input to _safeguard_

- `<prog_file>`  
  `<prog_file>` includes a set of event handler functions in JavaScript

- `<spec_file>`
  specification of the events handlers in dsl4sc.

  [Note]
  - `<spec_file>` can be formatted in either dsl4sc or scxml.
  - In case `<spec_file>` is not explicitly passed, _safeguard_ tries auto-detection.


## testing

See [this](tests/README.md).
