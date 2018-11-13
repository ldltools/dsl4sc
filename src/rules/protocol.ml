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

type protocol =
  | Proto_prop of protocol_prop
  | Proto_seq of protocol list
  | Proto_sum of protocol list
  | Proto_test of protocol
  | Proto_star of protocol

and protocol_prop =
  | PProp_event of string
(*
  | PProp_event_elt of string * int term list
  | PProp_neg of protocol_prop
 *)

[@@deriving show, yojson, eq]

type t = protocol

(** pretty-printing *)

let rec print_protocol out ?(fancy=false) (p : protocol) =
  print_protocol_rec out ~fancy p

and print_protocol_rec out ?(fancy=false) (p : protocol) =
  match p with
  | Proto_prop f -> print_protocol_prop out ~fancy f

  (* seq *)
  | Proto_seq [p'] when proto_prec p' <= proto_prec p ->
      print_protocol out p'
  | Proto_seq [p'] ->
      out "("; print_protocol out p'; out ")"
  | Proto_seq (p' :: ps) when proto_prec p' <= proto_prec p ->
      print_protocol out p';
      out "; ";
      print_protocol out (Proto_seq ps)
  | Proto_seq (p' :: ps) ->
      out "("; print_protocol out p'; out ")";
      out "; ";
      print_protocol out (Proto_seq ps)

  (* sum *)
  | Proto_sum [p'] when proto_prec p' <= proto_prec p ->
      print_protocol out p'
  | Proto_sum [p'] ->
      out "("; print_protocol out p'; out ")"
  | Proto_sum (p' :: ps) when proto_prec p' <= proto_prec p ->
      print_protocol out p';
      out " + ";
      print_protocol out (Proto_sum ps)
  | Proto_sum (p' :: ps) ->
      out "("; print_protocol out p'; out ")";
      out " + ";
      print_protocol out (Proto_sum ps)

  (* test *)
  | Proto_test p' when proto_prec p' <= proto_prec p ->
      print_protocol out p'; out "?";
  | Proto_test p' ->
      out "("; print_protocol out p'; out ")?";

  (* star *)
  | Proto_star p' when proto_prec p' <= proto_prec p ->
      print_protocol out ~fancy p'; out "*"
  | Proto_star p' ->
      out "("; print_protocol out ~fancy p'; out ")*"

  | _ ->
      failwith "print_protocol_rec"

and print_protocol_prop out ?(fancy=false) (f : protocol_prop) =
  match f with
  | PProp_event ev -> out ev
(*
  | PProp_neg f' -> out "!"; print_protocol_prop out ~fancy f'
 *)
  | _ ->
      failwith "print_protocol_prop"

(* precedence: grouping (()) < *, ? < concat (;) < choice (+) *)
and proto_prec = function
  | Proto_prop _ -> 0
  | Proto_seq _  -> 100
  | Proto_sum _  -> 200
  | Proto_test _ -> 50
  | Proto_star _ -> 30


let to_string (prn : (string -> unit) -> 'a -> unit) (x : 'a) =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  prn concat x;
  !str

let string_of_protocol =
  to_string (print_protocol ~fancy:false)

(** epsilon elimination *)

let rec include_epsilon_p = function
  | Proto_prop _ -> false
  | Proto_seq ps | Proto_sum ps ->
      (match List.find_opt include_epsilon_p ps with None -> false | Some _ -> true)
  | Proto_test p -> true
  | Proto_star p -> include_epsilon_p p

type nfa = (unit, string) Nfa.nfa

let eliminate_epsilon p =
  p
