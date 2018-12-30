(* $Id: rule.mli,v 1.3 2018/01/09 07:54:44 sato Exp $ *)
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
type rule =
    { event : event * string option;
        (** event (with optional expression) *)
      condition : Property.labelled_property * string option;
        (** trigger condition (with optional expression) *)
      action : action;
        (** action that includes a set of unit actions *)
    }

(** event *)
and event =
  | Ev_name of string
	(** case "on e".
	    rules2ldl only takes this into account.
	    the following cases are substitued in the preprocessing stage *)

  | Ev_name_seq of string list
	(** case "on e1, e2, .." *)
  | Ev_name_seq_compl of string list
	(** case "except on e1, e2, .." *)

(** action *)
and action =
    (action_unit * string option) list
        (** a set of unit actions each may accompany code fragment *)

and action_unit =
  | Act_ensure of Property.t
	(** post-condition *)
  | Act_raise of string list
	(** [raise [e1; e2; ..]] selects/raises one of the events non-deterministically *)
  | Act_do
        (** [do { code }] in dsl4sc corresponds to (Acto_do, Some code) *)
  | Act_preserve of Property.t list
	(** [on e preserve [p; p';..]] indicates p, p', .. should be preserved
	    thru processing e.
	    eliminated by preprocessor
	 *)

type t = rule

(** accessors *)

val event_name : event -> string

(** pretty-printing *)

val print_rule : (string -> unit) -> ?fancy:bool -> rule -> unit
val print_action : (string -> unit) -> action -> unit

val string_of_action : action -> string

(** pretty-printing -- ppx-generated *)

val pp_rule : Format.formatter -> rule -> unit

val show_event : event -> string
val show_action : action -> string
val show_rule : rule -> string

val rule_of_yojson : Yojson.Safe.json -> (rule, string) Result.result
val rule_to_yojson : rule ->  Yojson.Safe.json
