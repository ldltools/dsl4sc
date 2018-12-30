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

declare default element namespace "http://www.w3.org/2005/07/scxml";
declare namespace dsl4sc = "https://github.com/ldltools/dsl4sc";

declare function local:list_states ($states, $transitions)
{
  for $q in $states
  let $tr_seq := $q/dsl4sc:transition

  return
    if (exists ($q/@final))
    then
      element final {
        $q/@id, $q/node (),
        if (data ($q/@id) = "_rejected")
        then
          <onentry>
            <!--raise event="error.execution"/-->
            <script>
              SCXML.send ({{"event" : {{"name" : "error.execution"}}}});
              console.log ("_rejected");
            </script>
          </onentry>
        else
          ()
      }
    else
    element state {
      $q/@id,

      for $tr in $tr_seq

      let $ev := 
        if (empty ($tr/@event)) then () else
        if (empty ($tr/@alt_event))
        then $tr/@event
        else attribute event { data ($tr/@alt_event) }

      (: check if $tr is the only transition w. $ev :)
      let $ev_appears_many as xs:boolean :=
        exists ($ev) and count ($tr_seq[@event = $tr/@event]) > 1

      return
	(: transition with no rule :)
        if (empty ($tr/dsl4sc:rule)) then
          element transition { attribute target { $tr/@to }, $ev,
            comment { "tid", data ($tr/@tid) },

            (: special case: transition to "_rejected" :)
            if ($ev = "*" and data ($tr/@to) = "_rejected")
            then ()
            else ()
          }

	(: transition with 1 or more rules :)
        else
          let $target := $states[@id = $tr/@to]

          for $r in $tr/dsl4sc:rule

	  (: cond :)
          let $certainty := data ($r/@certainty) cast as xs:integer
	  let $c := $r/dsl4sc:condition
	    (: The LDL formula part of c has led to deriving this dfa in the first place,
		so (in most cases) the formula needs no runtime evaluation. :)
          let $c_omittable :=
            $certainty = 3 or $certainty = 7 or $certainty = 15
	    (: "omittable" indicates c always holds and needs no checking at runtime
	       refer to "Ldlrule.applicable" for the detail.
	     :)
	  let $cond :=
	    if ($c_omittable) then
	      if (empty ($c/dsl4sc:script)) then
                ()
              else
	        $c/dsl4sc:script/text ()
	    else (: c is not omittable :)
	      if (empty ($c/dsl4sc:script)) then
                (: ("_trace_matches (&quot;", $c/dsl4sc:formula/text (), "&quot;)") :)
                ()
              else
                (: ("_trace_matches (&quot;", $c/dsl4sc:formula/text (), "&quot;)",
                    "&amp;&amp;", $c/dsl4sc:script/text ()) :)
	        $c/dsl4sc:script/text ()
(:
	  let $cond :=
            if (not ($ev_appears_many))
	    then $cond
	    else (data ($states[@id = $tr/@from]/formula), $cond)
 :)

	  (: action :)
	  let $a := $r/dsl4sc:action
	  let $script :=
	    (:
            ("_trace_append (&quot;", data ($tr/dsl4sc:formula), "&quot;);",
             if (empty ($a/dsl4sc:script)) then () else (data ($a/dsl4sc:script), ";"))
	     :)
            if (empty ($a/dsl4sc:script)) then () else (data ($a/dsl4sc:script), ";")

          return
          element transition { attribute target { $tr/@to }, $ev,
            if (empty ($cond)) then () else attribute cond { $cond },

            comment { "tid", data ($tr/@tid) },

            (: condition :)
            if (empty ($cond)) then () else comment { "condition:", $cond },

            (: action :)
            comment { "transition:",
                      "curr_world:", data ($q/dsl4sc:formula),
                      "label:", data ($tr/dsl4sc:formula),
                      "next_world:", data ($target/dsl4sc:formula) },
            (:$a/raise,:)
            if (exists ($script)) then element script { $script } else (),

            (: rule :)
            comment { "rule:", data ($r/@rid),
                      "event:", data ($r/@event),
                      "certainty:", $certainty },
            comment { "rule.when:", data ($c/dsl4sc:formula) },
            comment { "rule.ensure:", data ($a/dsl4sc:formula) },
	    if (empty ($a/dsl4sc:raise)) then () else
            comment { "rule.raise:", data ($a/dsl4sc:raise/@event) },
	    if (empty ($a/dsl4sc:choice)) then () else
            comment { "rule.raise_sum:", $a/dsl4sc:choice/dsl4sc:raise/@event },

            ()
          }
    }
};

declare function local:list_propositions ($propositions)
{
  for $p in $propositions
  return comment { "proposition", data ($p/@name), data ($p/@variable) }
};

declare variable $extra_data :=
  <datamodel>
    <!-- _trace for each 'run' is a sequence of 'possible worlds' generated/tracked at run-time -->
    <data id="_trace"/>
    <!-- _trace_matches (guard) examines whether _trace ⊨ [true*]guard holds or not -->
    <data id="_trace_matches"
	  expr="function (guard) {{ return true; }}"/>
    <!-- _trace_append (next_world) appends next_world to _trace -->
    <data id="_trace_append"
	  expr="function (next_world) {{ }}"/>
  </datamodel>;

(: --------------------------------------------------------------------------------
   main
   --------------------------------------------------------------------------------
 :)

declare function local:print_in_scxml ($doc)
{
  (:let $propositions := $doc//dsl4sc:propositions/dsl4sc:proposition:)

  let $states0 := $doc//dsl4sc:states/dsl4sc:state
  (: we will discard the initial state that derives from the dfa generated by mona :)
  let $states :=
    for $state in $states0 where empty ($state/@initial) return $state
  let $initial :=
    for $state in $states0 where exists ($state/@initial)
    for $tr in $state/dsl4sc:transition where data ($tr/@alt_event) = "_init"
    return data ($tr/@to)

  let $transitions := $doc//dsl4sc:transitions/transition

  (:let $variables := $doc//dsl4sc:variables/dsl4sc:variable:)

  let $data := $doc//dsl4sc:scripts/dsl4sc:script/dsl4sc:datamodel/dsl4sc:data

  return

  <scxml version="1.0"
	 datamodel="ecmascript">
  { attribute initial { $initial } }

  <datamodel>
    {for $d in $data
     return element data { attribute id {$d/@id}, attribute expr {$d/@expr} }}
    {(:$extra_data:)()}
  </datamodel>

  {local:list_states ($states, $transitions)}
  </scxml>
};
