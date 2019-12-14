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
state : nat(3);
  // 0:not done, 1:done(withdrawn), 2:undone(refunded)
lock : nat(3);
  // 0:not locked, 1:locked(asset_locked), 2:timedout

property
state = 0 & lock = 0;
//<{state=0};({state=1} + {state=2})*; ({state=3} + {state=4})>last;

rule
on newContract ensure state = 0 & lock = 1;

on withdraw ensure state = 0 & lock = 1;
on withdraw_end ensure state = 1 & lock = 0;
  // lock updated
on withdraw_err_expired ensure state = 0 & lock = 2;
  // lock updated

on refund when state = 0 & lock = 1 ensure state = 0 & lock = 1;
on refund when state = 0 & lock = 2 ensure state = 0 & lock = 2;
on refund_end ensure state = 2 & lock = 0;
on refund_err_premature ensure state = 0 & lock = 1;
