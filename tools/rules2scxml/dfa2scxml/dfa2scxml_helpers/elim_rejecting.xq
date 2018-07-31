(: $Id: elim_rejecting.xq,v 1.1 2018/01/23 02:59:23 sato Exp sato $ :)

declare function local:elim_rejecting_rec ($final_rej, $nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:elim_rejecting_rec ($final_rej, $n/node ())

    case element (state)
      return
        if (empty (index-of ($final_rej, data ($n/@id)))) then $n else ()

    case element (transition)
      return
        if (empty (index-of ($final_rej, data ($n/@from))) and empty (index-of ($final_rej, data ($n/@to))))
        then $n
        else ()

    case element (*)
      return
        element {name ($n)} {$n/@*, local:elim_rejecting_rec ($final_rej, $n/node ())}

    default
      return $n
};

declare function local:elim_rejecting ($doc)
{
  let $final_rej :=
    for $q in $doc//states/state
    where exists ($q/@final) and exists ($q/@rejecting)
    return data ($q/@id)
  return
    local:elim_rejecting_rec ($final_rej, $doc)
};
