// -*-text-*-

// model /\ spec

#include "../tx_composite-model.dsl"
  // tx1_state, tx2_state : nat(4);  // = {0, 1, 2, 3}
  // 0:not done, 1:done, 2:undone(rollbacked), 3:failed

#include "../tx_composite-spec.dsl"
  // tx1_done, tx2_done : prop;

// ------------------------------------------------------------
// extra constraints

property
[]((tx1_state = 1 -> tx1_done) & (tx1_done -> tx1_state = 1));
[]((tx1_state != 1 -> !tx1_done) & (!tx1_done -> tx1_state != 1));
[]((tx2_state = 1 -> tx2_done) & (tx2_done -> tx2_state = 1));
[]((tx2_state != 1 -> !tx2_done) & (!tx2_done -> tx2_state != 1));
