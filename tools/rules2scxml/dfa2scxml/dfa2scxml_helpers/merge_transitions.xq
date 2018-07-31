(: $Id: $ :)

(: --------------------------------------------------------------------------------
   merge transitions with the same source/target states
   --------------------------------------------------------------------------------
 :)

declare function local:merge_rec ($grouped, $from, $to, $rest, $rslt)
{
  if (empty ($rest)) then
    ($rslt,
    element transition {
      attribute from { $from },
      attribute to { $to },
      attribute label0 { string-join (data ($grouped/@label0), "\n") },
      attribute label { string-join (data ($grouped/@label), "\n") }
    })
  else
    let $fst := $rest[1]
    return
      if (data ($fst/@from) = $from and data ($fst/@to) = $to)
      then local:merge_rec (($grouped, $fst), $from, $to, subsequence ($rest, 2), $rslt)
      else
        let $rslt2 :=
	  ($rslt,
	  element transition {
	    attribute from { $from },
	    attribute to { $to },
	    attribute label0 { string-join (data ($grouped/@label0), "\n") },
	    attribute label { string-join (data ($grouped/@label), "\n") }
	  })
	return 
          local:merge_rec ($fst, data ($fst/@from), data ($fst/@to), subsequence ($rest, 2), $rslt2)
};

(: transitions -> (uniq) transitions :)
declare function local:merge ($transitions as node()*)
{
  if (empty ($transitions))
  then ()
  else 
    let $transitions2 := 
      for $tr in $transitions order by $tr/@from, $tr/@to return $tr
      (: group-by is not suppered by xquery 1.0 :)
    let $fst := $transitions2[1]
    return
      $transitions2
(:
      local:merge_rec ($fst, data ($fst/@from), data ($fst/@to), subsequence ($transitions2, 2), ())
:)

};

(: inefficient :)
declare function local:merge_obsolete ($transitions as node()*)
{
  let $max := max (for $tr in $transitions return data ($tr/@to)) cast as xs:integer
  for $i in ("init", 0 to $max)
  for $j in 0 to $max
  let $seq :=
    for $tr in $transitions
    where data ($tr/@from) = string ($i) and data ($tr/@to) = string ($j)
    return $tr
  return
    if (empty ($seq))
    then ()
    else
      element transition {
        attribute from { $i },
        attribute to { $j },
        attribute label0 { string-join (data ($seq/@label0), "\n") },
        attribute label { string-join (data ($seq/@label), "\n") }
    }
};

(: --------------------------------------------------------------------------------
   main
   --------------------------------------------------------------------------------
 :)

declare function local:merge_transitions_rec ($nodes as node()*, $transitions)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:merge_transitions_rec ($n/node (), $transitions)

    case element (transitions)
      return
        element transitions { $n/@*, $transitions }

    case element (rules)
      return $n

    case element (*)
      return
        element {name ($n)} {
	  $n/@*, local:merge_transitions_rec ($n/node (), $transitions)
        }
    default
      return $n
};

(: main :)
declare function local:merge_transitions ($doc)
{
  let $transitions := local:merge ($doc//transitions/transition)
  return
  element dfa { $transitions }
  (:local:merge_transitions_rec ($doc, $transitions):)

};
