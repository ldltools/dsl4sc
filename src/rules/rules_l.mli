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

type lexbuf

exception Error of lexbuf * string
exception ParseError of string

val create_lexbuf : ?file:string -> Sedlexing.lexbuf -> lexbuf

(** lexer *)
val token : lexbuf -> Rules_p.token

(** parser (adapter) *)

val parse : (Rules_p.token, 'ast) MenhirLib.Convert.traditional -> lexbuf -> 'ast
