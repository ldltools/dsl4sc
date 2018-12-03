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

(** term *)
type _ term =
  | Tm_const : 'a * base_t -> 'a term
  | Tm_var : string * base_t -> 'a term
	(** var_name, var_type *)
  | Tm_app : ('a -> 'b) term * 'a term -> 'b term
	(** application Tm_app (f, e) *)
  | Tm_abs : ('a term -> 'b term) -> ('a -> 'b) term
	(** abstraction Tm_abs (fun e -> e') *)
  | Tm_op : string * 'a term list -> 'a term
	(** 'a operation *)
  | Tm_eq : 'a term * 'a term -> bool term
	(** equality *)

and base_t =
  | Ty_prop
  | Ty_nat of int
      (** [Ty_nat n] denotes {0, 1, ..., n - 1} *)

(** property *)
type property =
  | Prop_atomic of string
  | Prop_equal of int term * int term

  | Prop_neg of property
  | Prop_conj of property list
  | Prop_disj of property list
  | Prop_modal of modality * labelled_path * labelled_property

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

and labelled_path =
    path * label option

and label =
    string

type t = property

(** term ops *)

val term_to_propositions : int term -> string list
    (** variable of nat (n) -> m propositions where m = log2 (n) *)

(** property ops *)

val modal_p : t -> bool
    (** includes modality or not *)

val simp : t -> t
    (** property simplifier similar to Ldlsimp.simp, albeit more limited *)

val split : t -> ((string * (base_t * int)) list * t) list
    (** split a property p that includes term variables x1, .., xn into a set of pairs,
	each of which is of the form:
	([x1, (t1, v1); x2, (t2, v2); ..], q)
	where
	- v1, v2, ... : values for the variables
	- q : p[v1/x1, v2/x2, ...]
     *)

val propositionalize : t -> t
    (** expand prop w. terms to prop w/o terms *)

val find_term_variables : t -> (string * base_t) list

val include_term_variable_p : t -> bool

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
