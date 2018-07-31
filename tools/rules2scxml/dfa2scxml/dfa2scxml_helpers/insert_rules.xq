(: $Id: $ :)

declare function local:insert_rules_rec ($rules as element (rule)*, $nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:insert_rules_rec ($rules, $n/node ())

    case element (states)
      return $n

    case element (transition)
      return
	element transition {
	  $n/@*, $n/node (),

          let $id := data ($n/@id)
          for $r in $rules
          let $applicable :=
            for $tr in $r/applicable/tr
            where data ($tr/@name) = $id and not (data ($tr/@certainty) = "0")
            return $tr
          where exists ($applicable)
          return
            element rule {
              attribute rid { data ($r/@id) },
              attribute tid { data ($applicable/@name) },
              attribute event { data ($r/event/@name) },
              attribute certainty { data ($applicable/@certainty) },
              $r/event, $r/condition, $r/action
            }
	}

    case element (rules)
      return ()

    case element (*)
      return element {name ($n)} {$n/@*, local:insert_rules_rec ($rules, $n/node ())}

    default
      return $n
};

declare function local:insert_rules ($doc)
{
  local:insert_rules_rec ($doc//rules/rule, $doc)
};
