// -*-javascript-*-

protocol
ping; ((pong; ping)* + pong; (ping; pong)*); quit ;;
  // regular pattern of the incoming events

rule

// ping ({count})
on ping
raise quit + pong
{
    var n = _event.data.count;
    if (n == 0)
	SCXML.raise ({name: "quit", data: {die_alone: 0}});
    else
	SCXML.raise ({name: "pong", data: {count: (n - 1)}});
        // note: this event is what the protocol means by "pong"

    console.log ("[ping]", _event.data.count);
}

// pong ({count})
on pong
do
{
    SCXML.send ({event: _event, topic: "pong"});
    // [note] this outbound "pong" event is not included in the protocol
}

// quit ({die_alone})
on quit
do
{
    if (_event.data.die_alone == 0)
	SCXML.send ({event: {name: "quit", data: {die_alone: 1}}, topic: "pong"});
}
