protocol
(sharapova; williams + williams; sharapova)*;
(sharapova; sharapova + williams; williams);
game ;;

variable
state : nat(3); // nat(3) = {0, 1, 2}
  // 0: deuce
  // 1: advantage with either sharapova or williams
  // 2: 2 points ahead

property
<{state != 2}*; {state = 2}; {state = 2}> last;
  // state = 2  only in the last 2 steps
state = 0 && [](last -> state = 2);
  // initial and final conditions
[]!(<{state = 0}> state = 0 || <{state = 1}> state = 1);
  // neither state = 0 or 1 repeats
