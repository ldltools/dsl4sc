protocol (ping; pong)*;;
  // event ping, followed by pong, repeats 0 or more times
property <{!pinged}; ({pinged}; {!pinged})*> last;
  // boolean variable pinged is falsein the initial state
  // subsequently, pinged and not pinged alternate
  // repeatedly 0 or more times
