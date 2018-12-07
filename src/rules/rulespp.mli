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

    (* protocol *)
    ?expand_any: bool ->
    ?relax_protocols: bool ->
    ?minimize_protocols_always: bool ->

    (* rule *)
    ?discard_codes: bool ->
    ?expand_preserve: bool ->

    Rules.decl list -> Rules.decl list 
	(** preprocess rules
	    - expand any: any -> e1 + e2 + ...
	    - relax protocols: e -> _skip*; e; _skip*
	    - minimize protocols by means of dfa minimization
	    - discard code fragments in rules
	    - expand preserve
	 *)

(** event / variable *)

val pp_add_undeclared : Rules.decl list -> Rules.decl list
    (* add missing proposition/event/label declarations -- mandatory *)

(** protocol *)

val pp_expand_any : Rules.decl list -> Rules.decl list

val pp_relax_protocols : Rules.decl list -> Rules.decl list

val pp_minimize_protocols : ?always: bool -> Rules.decl list -> Rules.decl list

(** rule *)

val pp_expand_preserve : string list -> Rules.decl list -> Rules.decl list
    (** preserve_expand *)

val pp_discard_codes  : Rules.decl list -> Rules.decl list
    (** code_discard
	strip off code fragments (in JavaScript, etc) from rules
     *)
