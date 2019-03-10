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

type rule =
    string * event * condition * action * condition
      (* (rid, e, c, a, post) *)

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

    (** appliable (r, pre, post) (w1, w2) examines whether r is applicable
	to a transition from w1 to w2.

	- it returns false, when there is absolutely no chance, that is,
	  when either (pre(r) ∩ w1) or (post(r) ∩ w2) turns out to be empty.

	- otherwise, it returns true, since there is a chance.
	  in this case, it also returns some "certainty" value computed as follows.

	[certainty]
	it's a 4-bit value (b0, b1, b2, b3)
	- b1 = (w1 -> pre(r) always holds)
	- b3 = (post(r) -> w2 always holds)
	and returns a 4-bit value of (1, b1, 1, b3) as certainty.
	
	when b1 = b2 = true (1), r is applicable unconditionally.

	Given r = (id, e, c, a), this function returns 0, 1, or 2.
	when it returns 2, it is guaranteed that no runtime checking of c or a
	is needed, since it is statically guaranteed by the following rule,
	which is almost identical with the consequence rule of the Hoare logic.

        w1 -> pre, {pre}r{post}, post -> w2
        ----------------------------------------- (applicable unconditionally)
                      {w1}r{w2}
     *)


(** printing *)

val print_rule_in_xml : (string -> unit) -> (string list) -> (string * (Ldl.formula * Ldl.formula)) list -> t -> unit

(** for debugging *)

val verbosity_set : int -> unit
val verbosity_get : unit -> int

val debug_print_rule : t -> unit
