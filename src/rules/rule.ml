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
      action : action;
    }

(** event *)
and event =
  | Ev_name of string
  | Ev_name_seq of string list
  | Ev_name_seq_compl of string list

(** action *)
and action =
    (action_unit * string option) list

and action_unit =
  | Act_ensure of Property.property
  | Act_raise of string list (* nondeterministic choice *)
  | Act_do
  | Act_preserve of Property.property list

[@@deriving show, yojson, eq]

type t = rule

(** accessors *)

let event_name ev =
  match ev with
  | Ev_name e -> e
  | Ev_name_seq [e] -> e
  | _ -> failwith ("[event_name] " ^ show_event ev)

(** propositionalize *)

let propositionalize r =
  let (p, p_opt), c_opt = r.condition
  in let p' = Property.propositionalize p
  in let acts = r.action
  in let acts' =
    List.map
      (fun (act, a_opt) ->
	(match act with
	| Act_ensure p -> Act_ensure (Property.propositionalize p)
	| _ -> act),
	a_opt)
      acts
  in
  { event = r.event; condition = (p', p_opt), c_opt; action = acts'; }

let rec include_term_variable_p r =
  let (p, _), _ = r.condition
  in
  if Property.include_term_variable_p p then true else
  let acts = r.action 
  in let found_opt =
    List.find_opt
      (fun (act, _) ->
	match act with
	| Act_ensure p -> Property.include_term_variable_p p
	| Act_preserve ps ->
	    (match List.find_opt Property.include_term_variable_p ps with
	     Some _ -> true | None -> false)
	| _ -> false)
      acts
  in
  match found_opt with Some _ -> true | None -> false

(** pretty-printing *)

let rec print_rule out ?(fancy=false) (r : rule) =
  (* event *)
  let ev : event = fst r.event in
  let _ =
    match ev with
    | Ev_name e -> out "on "; out e;
    | Ev_name_seq (e :: rest) ->
	out "on ";
	out e; List.iter (fun e -> out ", "; out e) rest
    | Ev_name_seq_compl (e :: rest) ->
	out "except on ";
	out e; List.iter (fun e -> out ", "; out e) rest;
    | _ -> failwith "[print_rule]"
  in ();
  out " ";

  (* condition *)
  out "when ";
  let p, p_opt = r.condition in
  print_labelled_property out p;
  let _ =
    match p_opt with Some str -> out " {"; out str; out "}" | _ -> ()
  in
  out " ";

  (* action *)
  print_action out r.action;

  ()

and print_action out acts =
  let _ =
    List.fold_left
      (fun n (act, code_opt) ->
	print_action1 out act;
	let _ =
	  match code_opt with
	  | None -> ()
	  | Some code -> out " {"; out code; out "}"
	in
	if n > 1 then out " ";
	n - 1)
      (List.length acts) acts
  in ()

and print_action1 out = function
  | Act_ensure p ->
      out "ensure "; print_property out p
  | Act_raise (e :: es) ->
      out "raise "; out e;
      List.iter (fun e' -> out @@ " + " ^ e') es
  | Act_do ->
      out "do"
  | Act_preserve (p :: ps) ->
      out "preserve ";
      print_property out p;
      List.iter (fun p -> out ", "; print_property out p) ps
  | _ ->
      failwith "[print_action1]"

(** pretty-printing (string conversion) *)

let to_string (prn : (string -> unit) -> 'a -> unit) (x : 'a) =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  prn concat x;
  !str

let string_of_action =
  to_string print_action
