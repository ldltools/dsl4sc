(* $Id: protocol.mli,v 1.1 2018/11/23 18:05:46 sato Exp sato $ *)
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

(** protocol: regular language for specifying event patterns.

    identity events
    - Proto_seq []: epsilon event (1 in Kleene algebra).
    - Proto_sum []: empty protocol (0 in Kleene algebra).

    built-in events/protocols (macros)
    - _epsilon (1)
    - _empty (0): empty.
    - _any: synonymous to "e1 + e2 + ..." where ei ranges over all user-defined events

    special built-in event
    - _skip: label-less transition that is retained as it is.
    - _end: termination event

  *)
type protocol =
  | Proto_event of string
	(* named event *)
  | Proto_seq of protocol list
	(* p1; p2; .. *)
  | Proto_sum of protocol list
	(* p1 + p2 + .. *)
  | Proto_star of protocol
	(* p* *)

type t = protocol

(** minimization *)

val minimize : t -> t
    (** by means of dfa minimization *)

val mem_event : string -> t -> bool

(** helpers *)

val simp : t -> t

val flatten : t -> t

(** pretty-printing *)

val print_protocol : (string -> unit) -> ?fancy: bool -> t -> unit
val string_of_protocol : t -> string

(** pretty-printing -- ppx-generated *)

val pp_protocol : Format.formatter -> t -> unit
val show_protocol : t -> string

(** json parser/serializer -- ppx-generated *)

val protocol_of_yojson : Yojson.Safe.json -> (t, string) Result.result
val protocol_to_yojson : t ->  Yojson.Safe.json
