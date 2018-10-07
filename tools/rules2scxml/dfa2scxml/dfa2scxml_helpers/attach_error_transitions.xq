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

declare function local:attach_error_transitions_rec ($nodes as node()*)
{
  for $n in $nodes
  return
    typeswitch ($n)
    case document-node ()
      return local:attach_error_transitions_rec ($n/node ())

    case element (dfa)
      return
        element {name ($n)} {
          $n/@*,
          local:attach_error_transitions_rec ($n/node ())
        }

    case element (states)
      return
        element {name ($n)} {
          $n/@*,
          local:attach_error_transitions_rec ($n/node ()),

          <state id="_rejected" final="true">
            <onentry><raise event="error.execution"/></onentry>
          </state>
        }

    case element (state)
      return
        element state {
	  $n/@*, $n/node (),

          if (data ($n/@final) = "true")
          then
            ()
          else
            <transition event="*" to="_rejected"/>
        }

    case element (*)
      return
        element {name ($n)} {
          $n/@*, local:attach_error_transitions_rec ($n/node ())
        }

    default
      return $n
};

declare function local:attach_error_transitions ($doc)
{
  local:attach_error_transitions_rec ($doc)
};
