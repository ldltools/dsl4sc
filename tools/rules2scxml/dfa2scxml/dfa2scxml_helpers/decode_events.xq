(: $Id: decode_events.xq,v 1.1 2018/01/23 02:59:01 sato Exp sato $ :)

(:
declare variable $alist := doc("${infile_map}")//assoc;
 :)

(: ================================================================================
   helpers
   ================================================================================
 :)

declare function local:lookup_prop ($propositions, $i)
{
  for $p in $propositions
  where data ($p/@number) = $i
  return data ($p/@name)
};

declare function local:label2props ($propositions, $l) as xs:string*
{
  if ($l = "T") then () else
  for $str in fn:tokenize ($l, "\s+")
  return
    if (substring ($str, 1, 1) = "!")
    then string-join (("!", local:lookup_prop ($propositions, substring ($str, 2))))
    else local:lookup_prop ($propositions, $str)
};

(: l0 = "01X.." :)
declare function local:label0_order ($l0 as xs:string, $n, $i) as xs:integer
{
  if ($i > $n)
  then 1
  else (if (substring ($l0, $i, 1) = "X") then 2 else 1) * local:label0_order ($l0, $n, $i + 1)
};

(: $props (label) ∋ $prop (event elt) where $prop ::= p | !p :)
declare function local:includes1 ($props, $prop) as xs:boolean
{
  if (exists (index-of ($props, $prop)))
  then true ()
  else
    let $negative := substring ($prop, 1, 1) = "!"
    let $negation := if ($negative) then substring ($prop, 2) else string-join (("!", $prop))
    return
      empty (index-of ($props, $negation))
};

(: $props1 (label) ⊃ $props2 (event)
   ** note: returns true if neither event or !event is included in label
            thus, always returns true if label = T ($props1 = ())
 :)
declare function local:includes ($props1 as xs:string*, $props2 as xs:string*)
{
  if (empty ($props2))
  then true ()
  else
    if (not (local:includes1 ($props1, $props2[1])))
    then false ()
    else local:includes ($props1, subsequence ($props2, 2))
};

(: ================================================================================
   find_events ($propositions, $l0, $l)
   returns a list of event names (as string) that are included in $l
   ================================================================================
 :)

declare function local:find_events ($propositions, $l0, $l as xs:string?) as xs:string*
{
  if (empty ($l)) then () else

  let $props1 as xs:string* := local:label2props ($propositions, $l)
        (: props indicated by $l. special case: () when $l = "T" :)
  let $events :=
    for $bits in $alist[@type = "event"]
    let $bit_seq := $bits//bit/text ()
    where local:includes ($props1, $bit_seq)
        (: $props1 (=$l) includes the event indicated by $bits (=$bits) :)
    return
      data ($bits/@name)
  let $count := local:label0_order ($l0, string-length ($l0), 1)
	(: $count = num of combinations of propositions implied by $l0
	   e.g., "01" -> 1, "0X" -> 2, "XX" -> 4
	 :)
  return
    (: ($events, if (count ($events) = $count) then () else "_skip") :)
    $events
};

(: ================================================================================
   decode_events_rec ($propositions, $node)
   returns a new node set

   - case $node = <transition label="01X.." label="!p1 p2 ..">:
     <transition label0="01X.." label="!p1 p2 .." event="e">
   - o.w.: no change
   ================================================================================
 :)

declare function local:decode_events_rec ($propositions, $nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:decode_events_rec ($propositions, $n/node ())

    case element (transition)
(:
      return
        let $label0 := data ($n/@label0)
        let $label1 := data ($n/@label)
        let $label := string-join (local:label2props ($propositions, $label1), " ")
        let $event := local:find_events ($propositions, $label0, $label1)
        return
        element transition { $n/@*,
          if (exists ($event)) then attribute event { $event } else (),
          $n/node () }
:)

      return
        let $from := data ($n/@from)
        return if ($from = "init" or $from = "0") then $n else

        let $label0 := data ($n/@label0)
        let $label1 := data ($n/@label)
        let $label :=
	  if ($label1 = "T")
          then "true"
          else  string-join (local:label2props ($propositions, $label1), " ")
        let $event := local:find_events ($propositions, $label0, $label1)
        return
        element transition {
	  $n/@id, $n/@from, $n/@to, $n/@label0,
	  attribute label1 { $label1 },
	  attribute label { $label },
          if (exists ($event)) then attribute event { $event } else (),
          $n/node () }

    case element (*)
      return
        element {name ($n)} {
	  $n/@*, local:decode_events_rec ($propositions, $n/node ())
        }

    default
      return $n
};

declare function local:decode_events ($doc)
{
  let $propositions := $doc//variables/variable[data (@type) = "prop"]
  return local:decode_events_rec ($propositions, $doc)
};
