// -*-text-*-

protocol
tx1; (tx1_err; rollback + tx1_end; tx2; (tx2_end + tx2_err; rollback));;

variable
tx1_state, tx2_state : nat(4);  // = {0, 1, 2, 3}
  // 0:not_done, 1:done, 2:failed
  // state : nat(3); // 0(not done), 1:done, 2:undone


property
tx1_state = 0 & tx2_state = 0;

rule
on tx1 ensure tx1_state = 0 & tx2_state = 0;
on tx2 ensure tx1_state = 1 & tx2_state = 0;

on tx1_end when tx1_state = 0 ensure tx1_state = 1;
on tx2_end when tx1_state = 1 & tx2_state = 0 ensure tx2_state = 1;

on tx1_err ensure tx1_state = 2 & tx2_state = 0;
on tx2_err ensure tx2_state = 2 & tx1_state = 1;

on rollback when tx1_state = 2 ensure tx1_state = 3 & tx2_state = 0;
on rollback when tx2_state = 2 ensure tx1_state = 3 & tx2_state = 3;

// exclude all unspecified transitions
on tx1 when tx1_state != 0 | tx2_state != 0 ensure false;
on tx2 when tx1_state != 1 | tx2_state != 0 ensure false;
on tx1_end when tx1_state != 0 | tx2_state != 0 ensure false;
on tx2_end when tx1_state != 1 | tx2_state != 0 ensure false;
on tx1_err when tx1_state != 0 ensure false;
on tx2_err when tx2_state != 0 ensure false;
on rollback when tx1_state != 2 & tx2_state != 2 ensure false;


//except on tx1_end, tx1_err preserve tx1_state = 0, tx1_state = 1, tx1_state = 2, tx1_state = 3;
