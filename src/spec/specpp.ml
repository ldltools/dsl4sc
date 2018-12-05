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

open Ldl
open Ldlsimp

open Printf

(* ================================================================================
   preprocessing (moved from Rulespp.ml)
   [property]
   - align propositions
   - add special properties
   [rule]
   - move "preserve" rules to the last part
   - add a special "_skip" rule to mark the end of "normal" rules,
     which suggests the subsequent rules can be ignored in statechart generation
   - expand "preserve" rules to normal ones
   ================================================================================
 *)

let replace_properties s (props : Property.t list) =
  { events = s.events;
    protocols = s.protocols;
    variables = s.variables;
    properties = props;
    rules = s.rules
  }

(* align_propositions
   for each proposition p, add [true*] (<p>!p & <!p>p -> <true>!_idle) as a property.
 *)
let rec align_propositions s =
  let props : Property.t list =
    List.map
      (function
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
	    in prop
	| _ -> failwith "[align_propositions]")
      s.variables
  in
  replace_properties s (s.properties @ props)

(* add special properties [_idle1; _idle2; _idle3] *)
let add_special_properties ?(protocol_relax = false) s =
  let props_on_events : Property.property list =
    let idle1 = Prop_atomic "_idle"
	(* _idle *)
    and idle2 =
      Prop_modal (Mod_all, (Path_star (Path_prop (Prop_atomic "true"), None), None),
		  (Prop_disj [Prop_neg (Prop_atomic "last"); Prop_atomic "_idle"], None))
	(* [true*] (last -> _idle) *)
    and idle3 =
      (* this prohibits intermediate _idle states (and thus _skip events).
	 _idle appears only at the beginning/end of the trace.
	 included only when relax_protocol is not set *)
      Prop_modal (Mod_ex, (Path_prop (Prop_atomic "true"), None),
		  (Prop_modal (Mod_all, (Path_star (Path_prop (Prop_atomic "true"), None), None),
			       (Prop_disj [Prop_neg (Prop_atomic "_idle"); Prop_atomic "last"], None)), None))
	(* <true>[true*] (_idle -> last) *)
    in [idle1; idle2] @ if protocol_relax (*|| skip_allow*) then [] else [idle3]
  in
  replace_properties s (s.properties @ props_on_events)

let preprocess s =
  s |> align_propositions |> add_special_properties
