// -*-text-*-

// model /\ spec

#include "../tx_composite-model.dsl"
#include "../tx_composite-spec.dsl"

// ------------------------------------------------------------
// extra constraints

property
[]((tx1_state = 1 -> tx1_done) & (tx1_done -> tx1_state = 1));
[]((tx1_state != 1 -> !tx1_done) & (!tx1_done -> tx1_state != 1));
[]((tx2_state = 1 -> tx2_done) & (tx2_done -> tx2_state = 1));
[]((tx2_state != 1 -> !tx2_done) & (!tx2_done -> tx2_state != 1));
