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
    ?any_expand: bool ->
    ?protocol_relax: bool ->

    (* property *)
    ?case_split: bool ->
    ?propositionalize: bool ->
    ?extra_properties: bool ->

    (* rule *)
    ?code_discard: bool ->
    ?preserve_expand: bool ->

    Rules.decl list -> Rules.decl list 
	(** preprocess rules
	    - relax protocols
	    - align propositions w. events -- add extra properties
	    - discard code fragments in rules
	 *)

(** event / variable *)

val add_undeclared : Rules.decl list -> Rules.decl list
    (* add missing proposition/event/label declarations *)

(** protocol *)

val relax_protocols : Rules.decl list -> Rules.decl list
val relax_protocol : Protocol.t -> Protocol.t

(** property *)

val align_propositions : Rules.decl list -> Rules.decl list
    (** proposition_align
	for each proposition p, add [true*] (<p>!p & <!p>p -> <true>!_idle) as a property.
     *)

(** rule *)

val expand_preserve : string list -> Rules.decl list -> Rules.decl list
    (** preserve_expand *)

val discard_codes  : Rules.decl list -> Rules.decl list
    (** code_discard
	strip off code fragments (in JavaScript, etc) from rules
     *)
