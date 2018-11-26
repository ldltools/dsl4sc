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

(** type-carrying term *)
type _ term =
  | Tm_val : 'a * 'a term_t -> 'a term
  | Tm_var : string * 'a term_t -> 'a term
	(** var_name, var_type *)
  | Tm_app : ('a -> 'b) term * 'a term -> 'b term
	(** application Tm_app (f, e) *)
  | Tm_abs : ('a term -> 'b term) -> ('a -> 'b) term
	(** abstraction Tm_abs (fun e -> e') *)
  | Tm_bop : string -> ('a -> 'a -> 'a) term
	(** binary op *)
  | Tm_eq : 'a term * 'a term -> bool term
	(** equality *)

and _ term_t =
  | Ty_prop : bool term_t
  | Ty_nat : int -> int term_t
      (** [Ty_nat n] denotes {0, 1, ..., n} *)

  | Ty_fun : 'a term_t * 'b term_t -> ('a -> 'b) term_t

(** property *)
type property =
  | Prop_atomic of string
  | Prop_equal of int term * int term

  | Prop_neg of property
  | Prop_conj of property list
  | Prop_disj of property list
  | Prop_modal of modality * labelled_path * labelled_property

  (* deprecated *)
  | Prop_atomic_elt of string * int term list	(* indexed *)
  | Prop_label of string

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

  (* deprecated *)
  | Path_label of string

and labelled_path =
    path * label option

and label =
    string

type t = property

(** term ops *)

val eval_term : (string -> int term) -> int term -> int term

val term_to_propositions : int term -> string list

(** property ops *)

val modal_p : t -> bool
    (** includes modality or not *)

val propositionalize : t -> t
    (** expand prop w. terms to prop w/o terms *)

(** equality -- ppx-generated *)

val equal_property : property -> property -> bool
val equal_labelled_property : labelled_property -> labelled_property -> bool
val equal_path : path -> path -> bool
val equal_labelled_path : labelled_path -> labelled_path -> bool

(** pretty-printing *)

val print_property : (string -> unit) -> ?fancy:bool -> property -> unit
val print_labelled_property : (string -> unit) -> labelled_property -> unit
val print_labelled_path : (string -> unit) -> labelled_path -> unit

val string_of_labelled_property : labelled_property -> string
val string_of_labelled_path : labelled_path -> string

(** pretty-printing -- ppx-generated *)

val pp_term : (Format.formatter -> int -> unit) -> Format.formatter -> int term -> unit

val pp_property : Format.formatter -> property -> unit
val pp_path : Format.formatter -> path -> unit
val pp_labelled_property : Format.formatter -> labelled_property -> unit
val pp_labelled_path : Format.formatter -> labelled_path -> unit

val property_of_yojson : Yojson.Safe.json -> (property, string) Result.result
val property_to_yojson : property ->  Yojson.Safe.json
val labelled_property_of_yojson : Yojson.Safe.json -> (labelled_property, string) Result.result
val labelled_property_to_yojson : labelled_property ->  Yojson.Safe.json
val path_of_yojson : Yojson.Safe.json -> (path, string) Result.result
val path_to_yojson : path ->  Yojson.Safe.json
val labelled_path_of_yojson : Yojson.Safe.json -> (labelled_path, string) Result.result
val labelled_path_to_yojson : labelled_path ->  Yojson.Safe.json
val term_of_yojson : Yojson.Safe.json -> ('a term, string) Result.result
val term_to_yojson : 'a term ->  Yojson.Safe.json
