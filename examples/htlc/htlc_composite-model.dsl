// -*-text-*-

protocol
  Sec.newContract; Cash.newContract;
  Cash.withdraw;
   (Cash.withdraw_end; (Sec.withdraw; (Sec.withdraw_end + Sec.withdraw_err_expired))
   + Cash.withdraw_err_expired; (Cash.refund; Cash.refund_end; Sec.refund; (Sec.refund_err_premature; Sec.refund)*; Sec.refund_end))
;;

variable
  Sec.state, Cash.state : nat(5);
  // 0:ready, 1:locked, 2:timed_out, 3:withdrawn, 4:refunded


//variable
//tx1_state, tx2_state : nat(4);
//  //  0:ready, 1:locked, 2:done, 3:timeouted

property
Sec_state = 0 & Cash_state = 0;

rule
on Sec.newContract ensure Sec_state = 1 & Cash_state = 0;
on Cash.newContract ensure Sec_state = 1 & Cash_state = 1;
