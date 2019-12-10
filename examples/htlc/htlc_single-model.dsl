// -*-text-*-

protocol
newContract;
  (refund; refund_err_premature)*;
  (withdraw;
   (withdraw_end + withdraw_err_expired; refund; refund_end)
   +
   refund; refund_end)
;;

variable
//  state : nat(5);
//  // 0:ready, 1:locked, 2:timed_out, 3:withdrawn, 4:refunded

// state : nat(3); // 0(not done), 1:done(withdrawn), 2:undone(refunded)
// lock_state : nat(3); // 0(init), 1(locked), 2(timedout)

property
  <{state=0};({state=1} + {state=2})*; ({state=3} + {state=4})>last;

rule
on newContract ensure state = 1;

on withdraw ensure state = 1;
on withdraw_end ensure state = 3;
on withdraw_err_expired ensure state = 2;

on refund when state = 1 ensure state = 1;
on refund when state = 2 ensure state = 2;
on refund_end ensure state = 4;
on refund_err_premature ensure state = 1;

