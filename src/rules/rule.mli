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

(** term *)
type _ term_t =
  | Ty_int : int term_t
  | Ty_Arrow : 'a term_t * 'b term_t -> ('a -> 'b) term_t

type _ term =
  | Tm_val : 'a -> 'a term
  | Tm_var : string * 'a term_t -> 'a term
	(** var name, var type *)
  | Tm_abs : string * 'a term_t * 'b term -> ('a -> 'b) term
	(** var name, var type, tm type *)
  | Tm_app : ('a -> 'b) term * 'a term -> 'b term

(** protocol *)
(*
type protocol =
  | Proto_prop of protocol_prop
  | Proto_seq of protocol list
  | Proto_sum of protocol list
  | Proto_test of protocol
  | Proto_star of protocol

and protocol_prop =
  | PProp_event of string
  | PProp_event_elt of string * int term list
  | PProp_neg of protocol_prop
 *)

(** property *)
type property =
  | Prop_atomic of string
  | Prop_atomic_elt of string * int term list	(* indexed *)
  | Prop_neg of property
  | Prop_conj of property list
  | Prop_disj of property list
  | Prop_modal of modality * labelled_path * labelled_property
  | Prop_label of string			(* reference *)

and modality =
  | Mod_all | Mod_ex

and labelled_property =
    property * label option

(** path *)
and path =
  | Path_prop of property
	(** property (propositional) *)
  | Path_seq of labelled_path list
  | Path_sum of labelled_path list
  | Path_test of property
  | Path_star of labelled_path
  | Path_label of string		(* reference *)

and labelled_path =
    path * label option

and label =
    string

(** rule *)
type rule =
    { event : event * string option;
        (** event, code *)
      condition : labelled_property * string option;
        (** property, code *)
      action : action * string option;
        (** action, code *)
      path : labelled_path option;		(* deprecated *)
    }

(** event *)
and event =
  | Ev_name of string
	(** rules2ldl only takes this into account *)

  | Ev_name_seq of string list
  | Ev_name_seq_compl of string list
	(** eliminated by preprocessor *)

(** action *)
and action =
    path option * action_unit list
      (** path part is no longer used *)

and action_unit =
  | Act_ensure of property
  | Act_raise of string list
	(** [raise [e1; e2; ..]] selects/raises one of the events non-deterministically *)
  | Act_preserve of string list
	(** [on e preserve [p; p';..]] indicates p, p', .. should be preserved
	    thru processing e.
	    eliminated by preprocessor
	 *)

type t = rule

val propositional : property -> bool

val event_name : event -> string

(** equality -- ppx-generated *)

val equal_property : property -> property -> bool

(** pretty-printing *)

val print_labelled_property : (string -> unit) -> labelled_property -> unit
val print_property : (string -> unit) -> ?fancy:bool -> property -> unit
val print_labelled_path : (string -> unit) -> labelled_path -> unit
(*val print_protocol : (string -> unit) -> ?fancy:bool -> protocol -> unit*)
val print_rule : (string -> unit) -> ?fancy:bool -> rule -> unit
val print_action : (string -> unit) -> action -> unit

val string_of_labelled_property : labelled_property -> string
val string_of_labelled_path : labelled_path -> string
(*val string_of_protocol : protocol -> string*)
val string_of_action : action -> string

(** pretty-printing -- ppx-generated *)

val pp_property : Format.formatter -> property -> unit
val pp_path : Format.formatter -> path -> unit
val pp_labelled_property : Format.formatter -> labelled_property -> unit
val pp_labelled_path : Format.formatter -> labelled_path -> unit
(*val pp_protocol : Format.formatter -> protocol -> unit*)
val pp_term : Format.formatter -> 'a term -> unit
val pp_rule : Format.formatter -> rule -> unit

(*val show_protocol : protocol -> string *)
val show_event : event -> string
val show_action : action -> string
val show_rule : rule -> string

val labelled_property_of_yojson : Yojson.Safe.json -> (labelled_property, string) Result.result
val labelled_property_to_yojson : labelled_property ->  Yojson.Safe.json
val labelled_path_of_yojson : Yojson.Safe.json -> (labelled_path, string) Result.result
val labelled_path_to_yojson : labelled_path ->  Yojson.Safe.json
(*
val protocol_of_yojson : Yojson.Safe.json -> (protocol, string) Result.result
val protocol_to_yojson : protocol ->  Yojson.Safe.json
 *)
val term_of_yojson : Yojson.Safe.json -> ('a term, string) Result.result
val term_to_yojson : 'a term ->  Yojson.Safe.json
val rule_of_yojson : Yojson.Safe.json -> (rule, string) Result.result
val rule_to_yojson : rule ->  Yojson.Safe.json
