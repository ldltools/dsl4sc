(* $Id: $ *)
(*
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
 *)

open Protocol
open Property
open Rule
open Rules
open Spec

open Printf

(* ================================================================================
   preprocessing (moved from Rulespp.ml)
   [property]
   - align propositions
   - add_event_contraints
   [rule]
   - none (see Rulespp.preprocess)
   ================================================================================
 *)

let replace_properties s (props : Property.t list) =
  { events = s.events;
    protocols = s.protocols;
    variables = s.variables;
    properties = props;
    rules = s.rules
  }

(** property *)

(** align_propositions

   for each proposition p, add the following property:
     [true*] (<p>!p | <!p>p -> <true>!_idle)

   this indicates that, when a variable p changes its value by a transition,
   it must have been caused by processing an event.
   (!_idle indicates one or more events have just been processed)

   notes
   - no proposition value changes in any final transition (to the last states)
   - the introduced modal property assumes one event bit (_b0) for representing _idle
   - in case no event is defined, this function does nothing but returns,
     so each property-only definition is translated into a single-state model.
 *)
let rec align_propositions (s : Spec.t) =
  if s.events = [] then s else

  let props : Property.t list =
    List.fold_left
      (fun rslt -> function
	| p, VT_prop ->
	    let prop : Property.property =
	      (* [true*] (<p>!p & <!p>p -> <true>!_idle) *)
	      let tt = Prop_atomic "true" in
	      let p1 : Property.property =
		let p11 = Prop_modal (Mod_ex, (Path_prop (Prop_atomic p), None), (Prop_neg (Prop_atomic p), None))
		and p12 = Prop_modal (Mod_ex, (Path_prop (Prop_neg (Prop_atomic p)), None), (Prop_atomic p, None))
		in Prop_disj [p11; p12]
	      and p2 : Property.property =
		Prop_modal (Mod_ex, (Path_prop tt, None),
			    (Prop_neg (Prop_atomic "_idle"), None))
	      in
	      Prop_modal (Mod_all, (Path_star (Path_prop tt, None), None),
			  (Prop_disj [Prop_neg p1; p2], None))
	    in rslt @ [prop]
	| p, VT_term _ -> rslt)
      [] s.variables
  in
  replace_properties s (s.properties @ props)

(** add special properties [_idle1; _idle2; _idle3]

    - idle1 indicates that no event is observed in the initial state.

    - idle2 indicates _idle holds in the last state,
      hence the last state can only be reached by _skip

    - idle3 indicates, except the first (initial) state,
      _idle holds only in the last state.

 *)
let add_event_contraints ?(protocol_relax = false) (s : Spec.t) =
  if s.events = [] then s else

  let props_on_events : Property.property list =
    let idle1 = Prop_atomic "_idle"

	(* idle1 = _idle
	   idle1 indicates that no event is observed in the initial state.

	   in general, _idle indicates no event is observed in the current state
	   - internally, _idle is defined by setting all event-bits to false.
	     (event-bits are a set of log2(num_of_events) propositions
	      for representating events)
	   - when a state transition q -(e)-> q' occurs,
	     q' is marked with the "event bit" for e set to true.
	   - for q-(_skip)-> q', all event bits are set to false in q'.
	     strictly, if _idle holds at q',
	     it means that in q' there exist no incoming transition with an event

	   note that, if _idle appears in a formula,
	   the _skip event is also introduced.
	   this means the length of the event-bits is at least 1.
	 *)

    and idle2 =
      Prop_modal (Mod_all, (Path_star (Path_prop (Prop_atomic "true"), None), None),
		  (Prop_disj [Prop_neg (Prop_atomic "last"); Prop_atomic "_idle"], None))
	(* idle2 = [true*] (last -> _idle)
	   idle2 indicates _idle holds in the last state,
	   hence the last state can only be reached by _skip
	 *)

    and idle3 =
      Prop_modal (Mod_ex, (Path_prop (Prop_atomic "true"), None),
		  (Prop_modal (Mod_all, (Path_star (Path_prop (Prop_atomic "true"), None), None),
			       (Prop_disj [Prop_neg (Prop_atomic "_idle"); Prop_atomic "last"], None)), None))

      (* idle3 = <true>[true*] (_idle -> last)
	 idle3 indicates, except the first (initial) state,
	 _idle holds only in the last state.

	 note: this prohibits intermediate _idle states (and thus _skip events).
	 _idle appears only at the beginning/end of the trace.
	 included only when relax_protocol is not set
       *)

    in [idle1; idle2] @ if protocol_relax (*|| skip_allow*) then [] else [idle3]
  in
  replace_properties s (s.properties @ props_on_events)

(** preprocess : Spec.t -> Spec.t *)

let preprocess (s : Spec.t) =
  s |> align_propositions |> add_event_contraints
