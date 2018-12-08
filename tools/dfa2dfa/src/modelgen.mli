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

val merge : Ldlmodel.t -> Ldlrule.t list -> Ldlmodel.t * (string * string list) list

    (** generate a new model m' and alist
	by merging a model m and a set of rules
	where
        - m' : its states carry a set of possible worlds, and
	       each of its transitions is associated with a single event
        - alist is of the form [(rid, [tid; ...]); ..].
	    
	for each rule r, the pair (rid(r), [tid1; tid2; ..]) indicates
        that r can be accomapnied with the transitions indexed by tid1, tid2, ...
     *)

(** helpers *)

val update_states : Ldlmodel.t -> Ldlmodel.t
    (** associate possible worlds with each state *)

val split_transitions : Ldlmodel.t -> Ldlmodel.t
    (** split a transition for <n> multiple events into <n> transitions
	so each corresponds with a single event
     *)

val find_applicable_rules : Ldlmodel.t -> Ldlrule.t list -> (string * string) list
    (** model -> rules -> [rid, tid; ...] *)
