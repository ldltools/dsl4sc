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

type t =
    { events : string list;
      protocols : Protocol.t list;

      variables : (string * Rules.variable_type) list;
      properties : Property.t list;

      rules : Rule.t list;
    }

(** ctor *)

val spec_of_rules : Rules.t -> t

(** pretty-printing *)

val print_spec : (string -> unit) -> t -> unit
val string_of_spec : t -> string
