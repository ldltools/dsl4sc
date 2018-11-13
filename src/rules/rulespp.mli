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
    ?macro_expand:bool ->
    ?array_expand:bool ->
    ?undeclared_add:bool ->
    ?any_expand:bool ->
    ?interleaving_apply:bool ->
    ?protocol_relax:bool ->
    ?proposition_align:bool ->
    ?code_discard:bool ->
    ?preserve_expand:bool ->
    (*?skip_allow:bool ->*)
    Rules.decl list -> Rules.decl list 
  (** preprocess rules
       - expand macros/arrays
       - add missing proposition/event/label decls
       - apply interleaving
       - relax protocols
       - align propositions w. events -- add extra properties
       - discard code fragments in rules
    *)

(** macro_expand *)

val expand_macros : Rules.decl list -> Rules.decl list

(** array_expand *)

val expand_arrays : Rules.decl list -> Rules.decl list

(** undeclared_add *)

val add_undeclared : Rules.decl list -> Rules.decl list
    (* add missing proposition/event/label declarations *)

val find_declared : Rules.decl list -> string list * string list * string list
    (* decls -> (propositions, events, labels) *)
val find_undeclared : Rules.decl list -> string list * string list * string list
    (* decls -> (propositions, events, labels) *)

(** interleaving_apply *)

val apply_interleaving : Rules.decl list -> Rules.decl list

(** protocol_relax *)

val relax_protocols : Rules.decl list -> Rules.decl list
val relax_protocol : Protocol.t -> Protocol.t

(** proposition_align
    for each proposition p, add [true*] (<p>!p & <!p>p -> <true>!_idle) as a property.
 *)

val align_propositions : Rules.decl list -> Rules.decl list

(** preserve_expand
 *)

val expand_preserve : string list -> Rules.decl list -> Rules.decl list

(** code_discard
    strip off code fragments (in JavaScript, etc) from rules
 *)

val discard_codes  : Rules.decl list -> Rules.decl list
