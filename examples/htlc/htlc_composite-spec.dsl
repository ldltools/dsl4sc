// -*-text-*-
// composite transaction

variable
Sec_done, Cash_done, error;

property
!Sec_done & !Cash_done & !error;  // initial
[]<>(Sec_done & Cash_done | !Sec_done & !Cash_done | error);  // liveness (atomicity)
[]!(Sec_done & Cash_done & error);  // safety
//[](error -> []error);
