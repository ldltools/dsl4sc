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

type t
type rule = Ldlrule.rule

type label = string * Ldl.formula list * string list
      (* (id, next_world, event_name list) *)

val read_in : in_channel -> Xml.xml * t * rule list
    (* (props, lts, rules) *)

val update : t -> rule list -> t * (string * string list) list
    (** returns (m, alist)
	where alist is of the form [(rid, [tid; ...]); ..]
     *)

val collect_transitions : t -> (string * label * string) list

val rule_id : rule -> string

val verbose : int ref

(* printing *)

val print_states_in_xml : (string -> unit) -> t -> unit
val print_transitions_in_xml : (string -> unit) -> t -> unit
val print_rules_in_xml : (string -> unit) -> t -> (string * string list) list -> rule list -> unit
    (** print_rules_in_xml out m alist rs
	where alist is of the form [(rid, [tid; ...]); ..]
     *)

val debug_print : t -> unit
val debug_print_rule : rule -> unit
