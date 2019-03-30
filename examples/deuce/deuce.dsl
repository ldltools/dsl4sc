protocol
(sharapova + williams)*; game ;;

variable
state : nat(4); // nat(4) = {0, 1, 2, 3}
  // 0: deuce
  // 1: advantage with sharapova
  // 2: advantage with williams
  // 3: game

property
state = 0; // initial condition
[](last -> state = 3); // final condition

rule

on sharapova when state = 0 ensure state = 1;
on sharapova when state = 1 raise game ensure state = 3;
on sharapova when state = 2 ensure state = 0;
on sharapova when state = 3 ensure false;

on williams when state = 0 ensure state = 2;
on williams when state = 1 ensure state = 0;
on williams when state = 2 raise game ensure state = 3;
on williams when state = 3 ensure false;

on game when state != 3 ensure false;

except on sharapova, williams
preserve state = 0, state = 1, state = 2, state = 3;
