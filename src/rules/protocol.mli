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

type protocol =
  | Proto_prop of protocol_prop
  | Proto_seq of protocol list
  | Proto_sum of protocol list
  | Proto_test of protocol
  | Proto_star of protocol

and protocol_prop =
  | PProp_event of string
(*
  | PProp_event_elt of string * int term list
  | PProp_neg of protocol_prop
 *)

type t = protocol

(** epsilon elimination *)

val include_epsilon_p : protocol -> bool
val eliminate_epsilon : protocol -> protocol

(** pretty-printing *)

val print_protocol : (string -> unit) -> ?fancy:bool -> protocol -> unit
val string_of_protocol : protocol -> string

(** pretty-printing -- ppx-generated *)

val pp_protocol : Format.formatter -> protocol -> unit
val show_protocol : protocol -> string

(** json parser/serializer -- ppx-generated *)

val protocol_of_yojson : Yojson.Safe.json -> (protocol, string) Result.result
val protocol_to_yojson : protocol ->  Yojson.Safe.json
