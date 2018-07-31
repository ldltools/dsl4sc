(: $Id: $ :)

declare function local:attach_transitions_rec ($tr_seq as element (transition)*, $nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:attach_transitions_rec ($tr_seq, $n/node ())

    case element (state)
      return
        element state {
	  $n/@*, $n/node (),

          let $id := data ($n/@id) cast as xs:string
          for $tr in $tr_seq
          where data ($tr/@from) = $id
          return $tr
        }

    case element (transitions)
      return ()

    case element (rules)
      return ()

    case element (*)
      return
        element {name ($n)} {
          $n/@*, local:attach_transitions_rec ($tr_seq, $n/node ())
        }

    default
      return $n
};

declare function local:attach_transitions ($doc)
{
  local:attach_transitions_rec ($doc//transitions/transition, $doc)
};
