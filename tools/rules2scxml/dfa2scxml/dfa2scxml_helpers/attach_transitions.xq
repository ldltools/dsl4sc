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

declare function local:attach_transitions_rec ($tr_seq as element (transition)*, $nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:attach_transitions_rec ($tr_seq, $n/node ())

    case element (state)
      return
        element state {
	  $n/@*, $n/node (),

          let $id := data ($n/@id) cast as xs:string
          for $tr in $tr_seq
          where data ($tr/@from) = $id
          return
	    element transition {
	      attribute tid { data ($tr/@id) },
	      $tr/@*[name (.) != "id"],
	      $tr/node ()
	    }
        }

    case element (transitions)
      return ()

    case element (rules)
      return ()

    case element (*)
      return
        element {name ($n)} {
          $n/@*, local:attach_transitions_rec ($tr_seq, $n/node ())
        }

    default
      return $n
};

declare function local:attach_transitions ($doc)
{
  local:attach_transitions_rec ($doc//transitions/transition, $doc)
};
