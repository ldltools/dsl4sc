(: $Id: $ :)

declare function local:merge_formulas ($f1, $f2) as node()?
{
  if (empty ($f1)) then $f2 else if (empty ($f2)) then $f1 else
  element formula { data ($f1), "&amp;&amp;", data ($f2) }
};

declare function local:merge_rules1 ($rule1, $rule2) as node()
{
  element rule {
    attribute rid { data ($rule1/@rid), "+", data ($rule2/@rid) },
    $rule1/@tid, $rule1/@event,
    attribute certainty {
      if (data ($rule1/@certainty) = "2" and data ($rule2/@certainty) = "2")
      then "2" else "1"
    },

    $rule1/event,

    element condition {
      local:merge_formulas ($rule1/condition/formula, $rule2/condition/formula),
      $rule1/condition/script,
      $rule2/condition/script,
      ()
    },

    element action {
      local:merge_formulas ($rule1/action/formula, $rule2/action/formula),
      $rule1/action/raise,
      $rule2/action/raise,
      $rule1/action/script,
      $rule2/action/script,
      ()
    }
  }
};

declare function local:merge_rules_fold ($rslt, $rules as node()*) as node()
{
  if (empty ($rules))
  then $rslt
  else local:merge_rules_fold (local:merge_rules1 ($rslt, $rules[1]), subsequence ($rules, 2))
};

declare function local:merge_rules_rec ($nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:merge_rules_rec ($n/node ())

    case element (transition)
      return
        let $rules := $n/rule
        return
	  if (count ($rules) <= 1)
	  then $n
	  else
	    element transition { $n/@*,
	      local:merge_rules_fold ($rules[1], subsequence ($rules, 2))
	    }

    case element (states)
      return $n
    case element (rules)
      return $n

    case element (*)
      return
        element {name ($n)} {
	  $n/@*, local:merge_rules_rec ($n/node ())
        }
    default
      return $n
};

(: main :)
declare function local:merge_rules ($doc)
{
  local:merge_rules_rec ($doc)
};
