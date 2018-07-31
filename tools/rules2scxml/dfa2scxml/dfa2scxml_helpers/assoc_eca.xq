(: $Id: dfa2scxml_helper.xq,v 1.4 2017/10/25 05:45:52 sato Exp sato $ :)

(: for $transition, assciate rules in $rules using their events as keys :)
declare function local:assoc_eca ($rules, $transition)
{
  if (empty ($transition/@event))
  then $transition
  else
    for $e in fn:tokenize (data ($transition/@event), "\s+")
    return
      element transition {
        for $attr in $transition/@*
        where not (node-name ($attr) = xs:QName ("event"))
        return $attr,
        attribute event { $e },

        $transition/node (),

        for $r in $rules
        where data ($r/event/@name) = $e
        return $r
      }
};

declare function local:transform ($rules, $nodes as node()*) as node()*
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:transform ($rules, $n/node ())

    case element (transition)
      return local:assoc_eca ($rules, $n)
    case element (rules)
      return ()
    case element (*)
      return element {name ($n)} { $n/@*, local:transform ($rules, $n/node ()) }

    default
      return $n
};

let $rules := .//rules/rule
return
local:transform ($rules, .)
