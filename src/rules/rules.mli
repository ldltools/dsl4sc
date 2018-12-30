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
    { events : event_spec list;
      protocols : Protocol.t list;

      variables : variable_spec list;
      properties : Property.t list;

      rules : rule_spec list;
      extras : string list;
    }

and event_spec =
    string * string option
      (** event_name with optional expression *)

and variable_spec =
    (string * variable_type) * string option
      (** (var_name, var_type) with optional expression *)

and variable_type =
  | VT_prop			(* proposition type *)
  | VT_term of Property.base_t	(* term type *)

and rule_spec =
    Rule.t * string option
      (** rule with optional annotation *)

type t = rules

(** for parsing/preprocessing *)

type decl =
  (* protocol *)
  | Decl_event of event_spec
  | Decl_protocol of Protocol.t

  (* property *)
  | Decl_variable of variable_spec
  | Decl_property of Property.t

  (* eca rule *)
  | Decl_rule of rule_spec

  (* extra *)
  | Decl_extra of string

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
