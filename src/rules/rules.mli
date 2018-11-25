(* $Id: rules.mli,v 1.3 2018/01/11 01:06:37 sato Exp sato $ *)
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

(** rules *)
type rules =
    { event_decls : event_spec list;
      proto_decls : protocol_spec list;

      var_decls : variable_spec list;
      prop_decls : property_spec list;

      rule_decls : rule_spec list;
      impl_decls : string list;

      (* deprecated *)
      pvar_decls : proposition_spec list;	(* propositional variable *)
      path_decls : path_spec list;	
      label_decls : string list;
    }

(** event *)
and event_spec =
    (* name, code *)
    string * string option

(** protocol *)
and protocol_spec =
    (string * string list) option * Protocol.t
    (* (name, args), protocol *)

(** variable *)
and variable_spec =
    (* (name, type), code *)
    (string * variable_type) * string option

and variable_type =
  | VT_prop
  | VT_range of int * int
  | VT_impl of string option
    (** for implementation only. no corresponding proposition *)

(** property *)
and property_spec =
    (string * string list) option * Rule.labelled_property
    (* (name, args), property *)

(** rule *)
and rule_spec =
    (string * string list) option * Rule.rule
    (* (name, args), rule *)

(** proposition -- deprecated *)
and proposition_spec =
    (* name, code *)
    string * string option

(** path -- deprecated *)
and path_spec =
    string option * Rule.labelled_path

type t = rules

(** for parsing/preprocessing *)

type decl =
  (* protocol *)
  | Decl_event of event_spec
  | Decl_protocol of protocol_spec

  (* property *)
  | Decl_variable of variable_spec
  | Decl_property of property_spec

  (* eca rule *)
  | Decl_rule of rule_spec

  (* extras *)
  | Decl_impl of string

  | Decl_proposition of proposition_spec	(* deprecated *)
  | Decl_path of path_spec			(* deprecated *)
  | Decl_label of string			(* deprecated *)

val decls_to_rules : ?event_sort:bool -> decl list -> t

(** pretty-printing *)

val print_rules : (string -> unit) -> t -> unit
val string_of_rules : t -> string

val print_rules_in_xml : (string -> unit) -> t -> unit

(** ppx-generated *)

val pp_rules : Format.formatter -> t -> unit
val show_rules : t -> string

val rules_of_yojson : Yojson.Safe.json -> (t, string) Result.result
val rules_to_yojson : t ->  Yojson.Safe.json
