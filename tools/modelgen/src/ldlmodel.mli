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

type model =
    (state, label) Fsa.t

and state =
    string * bool * Ldl.formula
      (* (qid, accepting, possible_world) *)

and label =
    string * Ldl.formula list * string list
      (* (tid, next_world, event_name list) *)

type t = model

(** reader *)

val read_in : in_channel -> (string * Xml.xml) list * t * Ldlrule.t list
    (* (xml elements, model, rules) *)

(** accessors *)

val state_name : t -> int -> string

val detect_final : t -> int list

val collect_transitions : t -> (string * label * string) list

(** xml printer *)

val print_states_in_xml : (string -> unit) -> t -> unit
val print_transitions_in_xml : (string -> unit) -> t -> unit

val print_rules_in_xml : (string -> unit) -> t -> (string * string list) list -> Ldlrule.t list -> unit
    (** print_rules_in_xml out m alist rs
	where alist is of the form [(rid, [tid; ...]); ..]
     *)

(** for debugging *)

val verbosity_set : int -> unit
val verbosity_get : unit -> int

val debug_print : t -> unit
