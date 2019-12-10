// -*-text-*-
// composite transaction

variable
tx1_done, tx2_done, error;

property
!tx1_done & !tx2_done & !error;  // initial
[]<>(tx1_done & tx2_done | !tx1_done & !tx2_done | error);  // liveness (atomicity)
[]!(tx1_done & tx2_done & error);  // safety
[](error -> []error);
