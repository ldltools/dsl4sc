(* $Id: rule.ml,v 1.3 2018/01/09 07:54:44 sato Exp $ *)
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

open Protocol
open Property

(** rule *)
type rule =
    { event : event * string option;
      condition : Property.labelled_property * string option;
      action : action * string option;

      (* deprecated *)
      path : Property.labelled_path option;
    }

(** event *)
and event =
  | Ev_name of string
  | Ev_name_seq of string list
  | Ev_name_seq_compl of string list

(** action *)
and action =
    path option * action_unit list

and action_unit =
  | Act_ensure of Property.property
  | Act_raise of string list (* nondeterministic choice *)
  | Act_preserve of string list

[@@deriving show, yojson, eq]

type t = rule

(** accessors *)

let event_name ev =
  match ev with
  | Ev_name e -> e
  | Ev_name_seq [e] -> e
  | _ -> failwith ("event_name: " ^ show_event ev)

(** pretty-printing *)

let rec print_rule out ?(fancy=false) (r : rule) =
  out "on ";
  let ev : event = fst r.event in
  let _ =
    match ev with
    | Ev_name e -> out e;
    | Ev_name_seq (e :: rest) ->
	out e; List.iter (fun e -> out ", "; out e) rest
    | Ev_name_seq_compl (e :: rest) ->
	out "^(";
	out e; List.iter (fun e -> out ", "; out e) rest;
	out ")"
    | _ -> failwith "print_rule"
  in ();
  out " ";

  out "when ";
  let p, p_opt = r.condition in
  print_labelled_property out p;
  let _ =
    match p_opt with Some str -> out " {"; out str; out "}" | _ -> ()
  in
  out " ";

  out "do ";
  let a, a_opt = r.action in
  print_action out a;
  let _ =
    match a_opt with Some str -> out " {"; out str; out "}" | _ -> ()
  in

  ()

and print_action out = function
  | None, [] ->
      print_action1 out (Act_ensure (Prop_atomic "true"))
  | None, [act] ->
      print_action1 out act
  | None, acts when List.length acts > 1 ->
      out "(";
      print_action1 out (List.hd acts);
      List.iter	(fun act -> out ", "; print_action1 out act) (List.tl acts);
      out ")"
  | Some _, acts ->
      failwith "not yet implemented";
      print_action out (None, acts)
  | _ ->
      failwith "print_action"

and print_action1 out = function
  | Act_ensure p ->
      out "ensure "; print_property out p
  | Act_raise (e :: es) ->
      out "raise "; out e;
      List.iter (fun e' -> out @@ " + " ^ e') es
  | Act_preserve ps ->
      out "preserve ";
      assert (List.length ps > 0);
      out (List.hd ps);
      List.iter (fun p -> out (", " ^ p)) (List.tl ps)
  | _ ->
      failwith "print_action1"

(** pretty-printing (string conversion) *)

let to_string (prn : (string -> unit) -> 'a -> unit) (x : 'a) =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  prn concat x;
  !str

let string_of_action =
  to_string print_action
