(: $Id: adjust_labels.xq,v 1.1 2018/01/23 02:59:23 sato Exp sato $ :)

declare variable $propositions := root()//proposition/@name;
declare variable $nproposition := count ($propositions);

(: replaces $label (label="X01..." -> label="l1 l2 ...") :)
declare function local:construct_proposition ($label, $i, $result)
{
  if ($i > $nproposition)
  then $result
  else
    let $ch := substring ($label, $i, 1)
    let $p := $propositions[$i]
    let $sep := if ($result = "") then "" else " "
    return
      if ($ch = "1") then
        local:construct_proposition ($label, $i + 1, concat ($result, $sep, $i - 1))
      else if ($ch = "0") then
        local:construct_proposition ($label, $i + 1, concat ($result, $sep, "!", $i - 1))
      else
        local:construct_proposition ($label, $i + 1, $result)
};

(: label="01X.." => label0="01X.." label="!0 1 .." :)
declare function local:transform_transition ($n as element (transition))
{
  let $label := $n/@label
  return
    if (empty ($label))
    then $n
    else
      element transition {
	attribute from { data ($n/@from) },
	attribute to { data ($n/@to) },
	attribute label0 { data ($n/@label) },
	if (contains ($label, "0") or contains ($label, "1"))
	then attribute label { local:construct_proposition ($label, 1, "") }
	else attribute label { "T" }
      }
};

declare function local:transform ($nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:transform ($n/node ())

    case element (states)
      return
	let $acc := $n/accepting/id/text()
	let $rej := $n/rejecting/id/text()
	return
	  element states {
	    for $q in $n/state
	    let $id := data ($q/@id)
	    return
	      element state {
		$q/@*,
		if (exists (index-of ($acc, $id))) then
		  attribute accepting { "true" }
		else if (exists (index-of ($rej, $id))) then
		  attribute rejecting { "true" }
		else
		  ()
	      }
	  }

    case element (transitions)
      return
	element transitions {
	  $n/@*,
	  for $child in $n/element (*) return local:transform_transition ($child)
	}

    case element (*)
      return element {name ($n)} {$n/@*, local:transform ($n/node ())}
    default
      return $n
};

local:transform (.)
