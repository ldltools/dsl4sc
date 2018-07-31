(: $Id: $ :)

declare function local:lookup_prop ($propositions, $i)
{
  for $p in $propositions
  where data ($p/@name) = $i
  return data ($p/@variable)
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

(: l0 = "01X..":)
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

(: $props1 (label) ⊃ $props2 (event) :)
declare function local:includes ($props1 as xs:string*, $props2 as xs:string*)
{
  if (empty ($props2))
  then true ()
  else
    if (not (local:includes1 ($props1, $props2[1])))
    then false ()
    else local:includes ($props1, subsequence ($props2, 2))
};
