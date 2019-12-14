// -*-text-*-

protocol
  Sec.newContract; Cash.newContract;
  Cash.withdraw;
   (Cash.withdraw_end; (Sec.withdraw; (Sec.withdraw_end + Sec.withdraw_err_expired))
   + Cash.withdraw_err_expired; (Cash.refund; Cash.refund_end; Sec.refund; (Sec.refund_err_premature; Sec.refund)*; Sec.refund_end))
;;

variable
  // for compaction
  state : nat(9);

/*
  mappings: state -> (Sec_state, Sec_lock, Cash_state, Cash_lock)

  Sec_state, Sec_lock : nat(3);
  Cash_state, Cash_lock : nat(3);
    // [state] 0:not done, 1:done(withdrawn), 2:undone(refunded)
    // [lock]  0:not locked, 1:locked(asset_locked), 2:timedout

00: 0,0,0,0
01: 0,1,0,0
02: 0,1,0,1
03: 0,2,0,1
04: 1,0,0,1
05: 1,0,0,2
06: 1,0,1,0
07: 2,0,0,1
08: 2,0,2,0
*/

property
state = 0;

rule
on Sec.newContract ensure state = 1; // 0,1,0,0
on Cash.newContract ensure state = 2; // 0,1,0,1

on Cash.withdraw ensure state = 2; // 0,1,0,1
on Cash.withdraw_end ensure state = 4; // 1,0,0,1
on Cash.withdraw_err_expired ensure state = 3; // 0,2,0,1

on Cash.refund ensure state = 3; // 0,2,0,1
on Cash.refund_end ensure state = 7; // 2,0,0,1

on Sec.withdraw ensure state = 4; // 1,0,0,1
on Sec.withdraw_end ensure state = 6; // 1,0,1,0
on Sec.withdraw_err_expired ensure state = 5; // 1,0,0,2

on Sec.refund ensure state = 7; // 2,0,0,1
on Sec.refund_end ensure state = 8; // 2,0,2,0
on Sec.refund_err_premature ensure state = 7; // 2,0,0,1

// --------------------------------------------------------------------------------

/*
variable
  Sec_state, Sec_lock : nat(3);
  Cash_state, Cash_lock : nat(3);
    // [state] 0:not done, 1:done(withdrawn), 2:undone(refunded)
    // [lock]  0:not locked, 1:locked(asset_locked), 2:timedout

property
Sec_state = 0 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 0;  // initial

rule
on Sec.newContract ensure Sec_state = 0 & Sec_lock = 1 & Cash_state = 0 & Cash_lock = 0;
on Cash.newContract ensure Sec_state = 0 & Sec_lock = 1 & Cash_state = 0 & Cash_lock = 1;

on Cash.withdraw ensure Sec_state = 0 & Sec_lock = 1 & Cash_state = 0 & Cash_lock = 1;
on Cash.withdraw_end ensure Sec_state = 1 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1;
on Cash.withdraw_err_expired ensure Sec_state = 0 & Sec_lock = 2 & Cash_state = 0 & Cash_lock = 1;

on Cash.refund ensure Sec_state = 0 & Sec_lock = 2 & Cash_state = 0 & Cash_lock = 1;
on Cash.refund_end ensure Sec_state = 2 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1;

on Sec.withdraw ensure Sec_state = 1 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1;
on Sec.withdraw_end ensure Sec_state = 1 & Sec_lock = 0 & Cash_state = 1 & Cash_lock = 0;
on Sec.withdraw_err_expired ensure Sec_state = 1 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 2;

on Sec.refund ensure Sec_state = 2 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1;
on Sec.refund_end ensure Sec_state = 2 & Sec_lock = 0 & Cash_state = 2 & Cash_lock = 0;
on Sec.refund_err_premature ensure Sec_state = 2 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1;

//
on Sec.newContract when !(Sec_state = 0 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 0) ensure false;
on Cash.newContract when !(Sec_state = 0 & Sec_lock = 1 & Cash_state = 0 & Cash_lock = 0) ensure false;
on Cash.withdraw when !(Sec_state = 0 & Sec_lock = 1 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Cash.withdraw_end when !(Sec_state = 0 & Sec_lock = 1 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Cash.withdraw_err_expired when !(Sec_state = 0 & Sec_lock = 1 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Cash.refund when !(Sec_state = 0 & Sec_lock = 2 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Cash.refund_end when !(Sec_state = 0 & Sec_lock = 2 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Sec.withdraw when !(Sec_state = 1 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Sec.withdraw_end when !(Sec_state = 1 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Sec.withdraw_err_expired when !(Sec_state = 1 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Sec.refund when !(Sec_state = 2 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Sec.refund_end when !(Sec_state = 2 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1) ensure false;
on Sec.refund_err_premature when !(Sec_state = 2 & Sec_lock = 0 & Cash_state = 0 & Cash_lock = 1) ensure false;
*/
