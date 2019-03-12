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

(** rule *)

open Ldlsat

module Rule : sig

  type rule =
      string * event * condition * action * condition
	(* (rid, tid_seq, e, c, a, post) *)
  and event =
    string * string option
  and condition =
      Ldl.formula * string option
	(* (c, script) *)
  and action =
      action_unit list * string option
	(* (a, script) *)
  and action_unit =
  | Act_ensure of Ldl.formula
	(* post-condition *)
  | Act_raise of string
	(* raise e *)
  | Act_raise_sum of string list
	(* raise e1 + e2 + .. (choice) *)

  type t = rule

  val id_get : t -> string

  val simp : t -> t

  val applicable : rule -> Ldl.formula * Ldl.formula -> bool * int option

  (** printing *)

  val print_rule_in_xml : (string -> unit) -> (string list) -> (string * (Ldl.formula * Ldl.formula)) list -> t -> unit

end

(** executable model *)

type model =
    { fsa : (state, label) Fsa.t;
      mutable rules : Rule.t list;
      mutable rules_map : (string * string list) list;
      elements : (string * Xml.xml) list;
    }

and state =
    string * bool * Ldl.formula
      (* (qid, accepting, possible_world) *)

and label =
    string * Ldl.formula list * string list
      (* (tid, next_world, event_name list) *)

type t = model

(* accessors *)

val state_name : t -> int -> string

val detect_final : t -> int list

(* input / output *)

val from_channel : in_channel -> t

val to_channel : out_channel -> t -> unit

(* for debugging *)

val verbosity_set : int -> unit
val verbosity_get : unit -> int

val debug_print : t -> unit
