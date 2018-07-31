(: $Id: $ :)

declare function local:transform ($nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:transform ($n/node ())

    case element (transition)
      return
        element transition {
	  if (empty ($n/rule))
	  then $n/@*
	  else ($n/@from, $n/@to, attribute label { string-join (data ($n/rule/event/@name), "\n") }),

	  $n/node ()
        }

    case element (rules)
      return element rules { $n/@*, $n/node () }

    case element (*)
      return
        element {name ($n)} { $n/@*, local:transform ($n/node ()) }

    default
      return $n
};

local:transform (.)
