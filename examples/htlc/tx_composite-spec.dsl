// -*-text-*-
// composite transaction

variable
tx1_done, tx2_done;

property
!tx1_done & !tx2_done;  // initial
[]<>(tx1_done & tx2_done | !tx1_done & !tx2_done);  // liveness (atomicity)
