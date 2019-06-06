(: :)

declare default element namespace "http://www.w3.org/2005/07/scxml";
declare namespace dsl4sc = "https://github.com/ldltools/dsl4sc";

declare function local:collect_rec ($nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:collect_rec ($n/node ())

    case element (scxml)
      return
        let $initial := element dsl4sc:initial { attribute id { data ($n/@initial) } }
        return ($initial, local:collect_rec ($n/node ()))
    case element (final)
      return
        element dsl4sc:final { $n/@id }

    case element (state)
      return
      for $tr in $n/transition
      return
        element dsl4sc:transition {
          $tr/@event,
          attribute state { data ($n/@id) },
          $tr/@target
        }

    case element (*)
      return local:collect_rec ($n/node ())

    default
      return ()
};

declare function local:scxml2guards ($doc as node())
{
  let $collected := local:collect_rec ($doc)
  let $transitions :=
    (:$collected[name (.) = transition]:)
    for $n in $collected where data (local-name ($n)) = "transition" return $n
  let $events := distinct-values (data ($transitions/@event))
  let $guards :=
    for $e in $events
    return
      element dsl4sc:guard {
	attribute event { $e },
	for $tr in $transitions
	where data ($tr/@event) = $e
	return
	  element dsl4sc:transition { $tr/@state, $tr/@target }
      }

  return
    element dsl4sc:guards {
      $collected[data (local-name (.)) = "initial"],
      $collected[data (local-name (.)) = "final"],
      $guards
    }
};

declare function local:to_json ($guards as node())
{
  "{",
  concat ("&quot;initial&quot; : &quot;", data ($guards//dsl4sc:initial/@id), "&quot;,"),
  "&quot;guards&quot; : [",
   (for $e in $guards//dsl4sc:guard
    return
      (concat ("{&quot;event&quot; : &quot;", data ($e/@event), "&quot;,"),
       "&quot;transitions&quot; : [",
       (for $tr in $e/dsl4sc:transition
        return
          concat ("{&quot;state&quot; : ", "&quot;", data ($tr/@state), "&quot;, ",
                  "&quot;target&quot; : ", "&quot;", data ($tr/@target), "&quot;},")
       ),
       "null]},")
   ),
   "null]}"
};

(: main :)
(:
let $guards := local:scxml2guards (.)
return
(:$guards:)
local:to_json ($guards)
:)
