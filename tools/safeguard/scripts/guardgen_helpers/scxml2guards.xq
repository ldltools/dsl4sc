(: $Id: $ :)

(:
declare namespace scxml_ns = "http://www.w3.org/2005/07/scxml";
declare namespace dsl4sc_ns = "https://github.com/ldltools/dsl4sc";
declare default element namespace "http://www.w3.org/2005/07/scxml";
declare variable $scxml := doc ($scxmlfile);
:)

(:
declare variable $handler_prefix :=
  if (exists ($safeguard//code/@class))
  then concat (data ($safeguard//code/@class), ".")
  else "";

(:declare variable $safeguard := <dsl4sc_ns:safeguard/>;:)
declare function local:map_event ($event)
{
  let $matched := $safeguard//dsl4sc_ns:event[@name = $event]
  return
    if (exists ($matched))
    then concat ($handler_prefix, data ($matched[1]/@handler))
    else $event
};
:)

(: $scxml -> (initial, final, transition, ...) :)
declare function local:collect_rec ($nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:collect_rec ($n/node ())

    case element (scxml_ns:scxml)
      return
        let $initial := element dsl4sc_ns:initial { attribute id { data ($n/@initial) } }
        return ($initial, local:collect_rec ($n/node ()))
    case element (scxml_ns:final)
      return
        element dsl4sc_ns:final { $n/@id }

    case element (scxml_ns:state)
      return
      for $tr in $n/scxml_ns:transition
      return
        element dsl4sc_ns:transition {
          $tr/@event,
          attribute state { data ($n/@id) },
          $tr/@target
        }

    case element (*)
      return local:collect_rec ($n/node ())

    default
      return ()
};

declare function local:scxml2guards_xml ($root as node())
{
  let $collected := local:collect_rec ($root)
  let $transitions :=
    (:$collected[name (.) = transition]:)
    for $n in $collected where data (local-name ($n)) = "transition" return $n
  let $events := distinct-values (data ($transitions/@event))
  let $guards :=
    for $e in $events
    return
      element guard {
        attribute event { $e },
	for $tr in $transitions
	where data ($tr/@event) = $e
	return
	  element transition { $tr/@state, $tr/@target }
      }

  return
    element guards {
      attribute initial { data ($collected[data (local-name (.)) = "initial"]/@id) },
      attribute final { data ($collected[data (local-name (.)) = "final"]/@id) },
      $guards
    }
};

declare function local:to_json ($guards as node())
{
  "{",
  "&quot;params&quot; : {",
    concat ("&quot;initial&quot; : &quot;", data ($guards/@initial), "&quot;,"),
    concat ("&quot;final&quot; : &quot;", data ($guards/@final), "&quot;"),
  "},",
  "&quot;body&quot; : [",
   (for $e in $guards//dsl4sc_ns:guard
    return
      (concat ("{&quot;event&quot; : &quot;", data ($e/@event), "&quot;,"),
       "&quot;transitions&quot; : [",
       (for $tr in $e/dsl4sc_ns:transition
        return
          concat ("{&quot;state&quot; : ", "&quot;", data ($tr/@state), "&quot;, ",
                  "&quot;target&quot; : ", "&quot;", data ($tr/@target), "&quot;},")
       ),
       "null]},")
   ),
   "null]}"
};

declare function local:scxml2guards ($root as node(), $outtype as xs:string)
{
  if ($outtype = "xml")
  then local:scxml2guards_xml ($root)
  else local:to_json (local:scxml2guards_xml ($root))
};

(: main :)
(:
let $guards := local:scxml2guards (.)
return
(:$guards:)
local:to_json ($guards)
:)
