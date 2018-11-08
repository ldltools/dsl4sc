(: $Id: $ :)
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

(:
declare variable $accept_transition := "_accept";
 :)

declare function local:rename_acc_transitions_rec ($nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:rename_acc_transitions_rec ($n/node ())

    case element (propositions)
      return $n

    case element (states)
      return $n

    case element (transition)
      return
        let $alt_event := $n/@alt_event
        let $alt_event :=
          if (exists ($alt_event))
          then
            if (data ($alt_event) = "_accept")
            then
              attribute alt_event { $accept_transition }
            else $alt_event
          else ()
        return
          element transition {
            $n/@*[name (.) != "alt_event"], $alt_event,
	    $n/node ()
          }

    case element (variables)
      return $n

    case element (rules)
      return $n

    case element (*)
      return
        element {name ($n)} {
          $n/@*, local:rename_acc_transitions_rec ($n/node ())
        }

    default
      return $n
};

declare function local:rename_acc_transitions ($doc)
{
  if ($accept_transition = "_accept")
  then $doc
  else local:rename_acc_transitions_rec ($doc)
};

