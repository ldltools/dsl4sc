// atomicity of a composite transaction (tx1; tx2)
// only one of the following 2 cases can occur in the end
// - both tx1 and tx2 succeed
// - neither succeeds (rollbacked)
protocol
tx1; (tx1_err; rollback + tx2; (tx2_err; rollback)?);;

property
!tx1_done & !tx2_done;  // initial condition

rule
on tx1 ensure tx1_done;
on tx2 ensure tx2_done;
  // tx{1,2} validates tx{1,2}_done, respectively
on rollback ensure !tx1_done & !tx2_done;
  // rollback invalidates the both tx1 and tx2

except on tx1, rollback preserve tx1_done;
except on tx2, rollback preserve tx2_done;
  // tx{1,2}_done changes its value only on tx{1,2} and rollback

// --------------------------------------------------------------------------------
// verification (atomicity)
// property []<>(tx1_done & tx2_done | !tx1_done & !tx2_done);
