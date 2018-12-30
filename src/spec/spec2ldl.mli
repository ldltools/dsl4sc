(* $Id: spec2ldl.mli,v 1.1 2017/08/25 20:07:53 sato Exp $ *)
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

(** spec to ldl translation *)

type event_map = (string * Property.t) list
      (** event name -> conjunction of atomic propositions *)

val translate :
    ?propositionalize: bool ->
    ?keep_terms: bool ->
    Spec.t -> Property.t list * event_map

val formula_of_property : Property.t -> Ldl.formula

(** helpers *)

val events_to_map : string list -> event_map
    (** event name -> atomic propositions *)

val property_of_protocol : event_map -> Protocol.t -> Property.t

val property_of_rule : event_map -> Rule.t -> Property.t
