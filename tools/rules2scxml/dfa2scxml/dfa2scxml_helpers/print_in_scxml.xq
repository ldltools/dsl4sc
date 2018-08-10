(: $Id: print_in_scxml.xq,v 1.1 2018/01/23 02:59:23 sato Exp sato $ :)
(:
 * (C) Copyright IBM Corp. 2018.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 :)

declare function local:list_states ($states, $transitions, $propositions)
{
  for $q in $states
  return
    element state {
      $q/@id,

      let $tr_seq := $q/transition
      for $tr in $tr_seq

      let $ev := 
        if (empty ($tr/@event)) then () else
        attribute event {
          if (exists ($tr/@alt_event)) then $tr/@alt_event else $tr/@event
        }
      (: check if $tr is the only transition w. $ev :)
      let $ev_appears_many as xs:boolean :=
        exists ($ev) and count ($tr_seq[@event = $tr/@event]) > 1

      return
	(: transition with no rule :)
        if (empty ($tr/rule)) then
          element transition { attribute target { $tr/@to }, $ev,
            comment { "tid", data ($tr/@id) }
          }

	(: transition with 1 or more rules :)
        else
          let $target := $states[@id = $tr/@to]

          for $r in $tr/rule

          let $certainty := data ($r/@certainty) cast as xs:integer
	  let $c := $r/condition
          let $c_omittable :=
            $certainty = 3 or $certainty = 7 or $certainty = 15
	  let $cond :=
	    if ($c_omittable and empty ($c/script)) then
              ()
            else if ($c_omittable) then
	      $c/script/text ()
            else if (empty ($c/script)) then
              ("_trace_matches (&quot;", $c/formula/text (), "&quot;)")
            else
              ("_trace_matches (&quot;", $c/formula/text (), "&quot;)",
              "&amp;&amp;", $c/script/text ())
(:
	  let $cond :=
            if (not ($ev_appears_many))
	    then $cond
	    else (data ($states[@id = $tr/@from]/formula), $cond)
 :)
	  let $a := $r/action
	  let $action :=
            ("_trace_append (&quot;", data ($tr/formula), "&quot;);",
             if (empty ($a/script)) then () else (data ($a/script), ";"))

          return
          element transition { attribute target { $tr/@to }, $ev,
            if (empty ($cond)) then () else attribute cond { $cond },

            comment { "tid", data ($tr/@id) },

            (: condition :)
            if (empty ($cond)) then () else comment { "condition:", $cond },

            (: action :)
            comment { "transition:",
                      "curr_world:", data ($q/formula),
                      "label:", data ($tr/formula),
                      "next_world:", data ($target/formula) },
            (:$a/raise,:)
            element script { $action },

            (: rule :)
            comment { "rule:", data ($r/@rid),
                      "event:", data ($r/@event),
                      "certainty:", $certainty },
            comment { "rule.when:", data ($c/formula) },
            comment { "rule.ensure:", data ($a/formula) },
	    if (empty ($a/raise)) then () else
            comment { "rule.raise:", data ($a/raise/@event) },
	    if (empty ($a/choice)) then () else
            comment { "rule.raise_sum:", $a/choice/raise/@event },

            ()
          }
    }
};

declare function local:list_propositions ($propositions)
{
  for $p in $propositions
  return comment { "proposition", data ($p/@name), data ($p/@variable) }
};

(: --------------------------------------------------------------------------------
   main
   --------------------------------------------------------------------------------
 :)

declare function local:print_in_scxml ($doc)
{
  let $propositions := $doc//propositions/proposition
  let $states := $doc//states/state
  let $transitions := $doc//transitions/transition
  return

  <scxml version="1.0"
	 datamodel="ecmascript"
	 initial="1">
  <datamodel>
    <!-- _trace for each 'run' is a sequence of 'possible worlds' generated/tracked at run-time -->
    <data id="_trace"/>
    <!-- _trace_matches (guard) examines whether _trace ⊨ [true*]guard holds or not -->
    <data id="_trace_matches"
	  expr="function (guard) {{ return true; }}"/>
    <!-- _trace_append (next_world) appends next_world to _trace -->
    <data id="_trace_append"
	  expr="function (next_world) {{ }}"/>
  </datamodel>
  {local:list_propositions ($propositions),
   local:list_states ($states, $transitions, $propositions)
  }
  </scxml>
};
