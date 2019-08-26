protocol (ping; pong)*; ping?; quit;;
  // event "ping", followed by "pong", which repeats 0 or more times
  // and may be further followed by an extra "ping".
property <{!pinged}><({pinged}; {!pinged})*>(<{pinged}>last | last);
  // boolean variable pinged is set to false in the initial state
  // subsequently, pinged and not pinged alternate
  // repeatedly 0 or more times
