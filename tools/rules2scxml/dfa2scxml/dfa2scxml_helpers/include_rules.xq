(: $Id: add_rules.xq,v 1.1 2018/01/23 02:59:23 sato Exp sato $ :)

(:
declare variable $rules := doc("${infile_rules}")//rules/rule;
declare variable $vars := doc("${infile_rules}")//variables/variable;
 :)

declare function local:include_rules_rec ($nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:include_rules_rec ($n/node ())

    case element (dfa)
      return
        element dfa { $n/@*, $n/node (),
          (:<variables>{$vars}</variables>,:)
          <rules>{$rules}</rules>,
          $impl
        }

    case element (*)
      return
        element {name ($n)} {
	  $n/@*, local:include_rules_rec ($n/node ())
        }

    default
      return $n
};

declare function local:include_rules ($doc)
{
  local:include_rules_rec ($doc)
};