(* $Id: rulesexpand.mli,v 1.1 2017/10/23 17:24:28 sato Exp $ *)
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

val preprocess :

    (* variables *)
    ?allow_undeclared: bool ->

    (* protocol *)
    ?expand_any: bool ->
    ?minimize_protocols: int ->
    ?relax_protocols: bool ->

    (* rule *)
    ?expand_preserve: bool ->
    ?discard_codes: bool ->

    Rules.decl list -> Rules.decl list 
	(** preprocess rules
	    - expand any: any -> e1 + e2 + ...
	    - minimize protocols by means of dfa minimization
	    - relax protocols: e -> _skip*; e; _skip*
	    - expand preserve
	    - discard code fragments in rules
	 *)

(** event / variable *)

val pp_add_undeclared : ?allow_undeclared: bool -> Rules.decl list -> Rules.decl list
    (* add missing proposition/event/label declarations -- mandatory *)

(** protocol *)

val pp_expand_any : Rules.decl list -> Rules.decl list

val pp_relax_protocols : Rules.decl list -> Rules.decl list

val pp_minimize_protocols : ?always: bool -> Rules.decl list -> Rules.decl list

(** rule *)

val pp_expand_trigger : Rules.decl list -> Rules.decl list
    (** expand "on e1, e2, .." rules to "on e1; on e2; .."  rules
     *)

val pp_expand_preserve : Rules.decl list -> Rules.decl list
    (** expand "preserve" rules to their equivalent "ensure" rules
     *)

val pp_discard_codes  : Rules.decl list -> Rules.decl list
    (** code_discard
	strip off code fragments (in JavaScript, etc) from rules
     *)
