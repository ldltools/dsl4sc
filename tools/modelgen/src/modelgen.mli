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

val restore_possible_worlds : Model.t -> Model.t
    (** associate possible worlds to the states

	each state
	  <state id=."q"/>
	is extended to
	  <state id="q">
	    <formula>propositional_formula</formula>
	  </state>
     *)

val split_transitions : Model.t -> Model.t
    (** split each transition that can be triggered by <n> different events into <n> transitions
	so that each corresponds with a single event

	each transition
	  <transition id="t" event="e1 e2 .."/>
	is split to
	  <transition id="t_1" event="e1"/>
	  <transition id="t_2" event="e2"/>
	  ...
     *)

val chart_rules :  Model.t -> Model.t
    (** for each rule, find its corresponding transitions,
	and attach their references to the rule.

	each rule
	  <rule id="r"/>
	is extended to
	  <rule id="r">
	    <applicable><tr name=../><tr name=../>...</applicable>
	  </rule>
     *)

(** for debugging *)

val verbosity_set : int -> unit
val verbosity_get : unit -> int
