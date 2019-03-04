`dfa2dfa` performs the following operations.

- for each DFA state, associate it with a formula (disjunction of possible worlds).
- for each pair (q1, q2) where q1 and q2 are DFA states,
  - find applicable rules by comparing their conditions and actions with the formulas accompanied by q1 and q2.
  - turn them into transitions from q2 to q2, and associate them with q1.
