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

open Ldl
open Ldlsimp
open Printf

type model =
    (state, label) Fsa.t

and state =
    string * bool * Ldl.formula
      (* (qid, accepting, possible_world) *)

and label =
    string * formula list * string list
      (* (tid, next_world, event_name list) *)

type t = model
type rule = Ldlrule.t

let verbose = ref 0

let verbosity_set n =
  verbose := n
      
let verbosity_get () =
  !verbose

let rule_id (rid, _, _, _) = rid

(* string -> string list *)
let rec tokenize str =
  tokenize_rec str 0 (String.length str) []

and tokenize_rec str i len rslt =
  try 
    let j = String.index_from str i ' ' in
    tokenize_rec str (j + 1) len (rslt @ [String.sub str i (j - i)])
  with Not_found ->
    rslt @ [String.sub str i (len - i)]

let formula_of_string str =
  Ldl_p.formula Ldl_l.token (Lexing.from_string str)

let state_name (m : t) (i : int) =
  let name, _, _ = Fsa.state_get m i in name

let state_index_by_name (states : (int * state) list) (name : string) =
  try
    let i, _ = List.find (function (_, (name', _, _)) -> name' = name) states
    in i
  with Not_found -> failwith ("state_index_by_name: " ^ name)

(* id *)
let _id = ref 0
let gen_id (prefix : string) =
  incr _id; prefix ^ (string_of_int !_id)

(** reader *)

(* in_channel -> t * rule list *)
let read_in (ic : in_channel) =
  let xml = Xml.parse_in ic in
  let alist : (string * Xml.xml) list =
    (* elts = [propositions; states; transitions; variables; rules] *)
    let rec trav (rslt : (string * Xml.xml) list) = function
      | Xml.Element ("dfa", _, children) ->
	  List.map
	    (fun child -> match child with Xml.Element (name, _, _) -> name, child)
	    children
      | Xml.Element (_, _, children) ->
	  List.fold_left (Xml.fold trav) rslt children
      | _ -> rslt
    in trav [] xml in
  assert (List.length alist >= 3);

  (* xml -> t *)
  let m : t = Fsa.make () in

  (* states *)
  let nodes = List.assoc "states" alist in
  if !verbose > 0 then eprintf "** states: %d\n" (List.length (Xml.children nodes));
  List.iter
    (function Xml.Element ("state", attrs, _) ->
      assert (List.mem_assoc "id" attrs);
      let qid : string = List.assoc "id" attrs in
      let i = Fsa.state_add m (qid, List.mem_assoc "accepting" attrs, Ldl_atomic "false") in
      if !verbose > 1 then eprintf "  state: %d, %s\n" i qid;
      ())
    (Xml.children nodes);
  if !verbose > 0 then
    (eprintf "** states:";
     List.iter (fun (i, (qid, _, _)) -> eprintf " (%d,%s)" i qid) (Fsa.alist_of_states m);
     eprintf "\n");

  (* transitions *)
  let edges = List.assoc "transitions" alist in
  if !verbose > 1 then eprintf "** transitions: %d\n" (List.length (Xml.children edges));
  List.iter
    (function Xml.Element ("transition", attrs, _) ->
      assert (List.mem_assoc "id" attrs);
      let tid = List.assoc "id" attrs in
      let q1, q2 = List.assoc "from" attrs, List.assoc "to" attrs in
      let n1, n2 =
	let states = Fsa.alist_of_states m
	in state_index_by_name states q1, state_index_by_name states q2
      and lab = List.assoc "label" attrs
      and events =
	if not (List.mem_assoc "event" attrs) then [] else
	tokenize (List.assoc "event" attrs) in
      assert (List.length (List.sort_uniq compare events) = List.length events);
      let props : formula list =
	List.fold_left
	  (fun rslt str ->
	    assert (String.length str > 0);
	    let i = if str.[0] = '!' then 1 else 0 in
	    if String.length str >= i + 2 && String.sub str i 2 = "_b"
	    then rslt (* skip events *)
	    else rslt @ [formula_of_string str])
	  [] (tokenize lab)
      in
      Fsa.transition_add m n1 (Some (Fsa.sigma_add m (tid, props, events)), n2);
      if !verbose > 1 then
	begin
	  eprintf "  transition: %s (%s->%s) " tid q1 q2;
	  eprintf "events:"; List.iter (eprintf " %s") events;
	  eprintf "\n"
	end;
      ())
    (Xml.children edges);
  if !verbose > 0 then
    begin
      eprintf "** transitions:";
      let n =
	List.fold_left
	  (fun n (i, edges) ->
	    List.iter
	      (fun (l_opt, j) ->
		let q1, _, _= Fsa.state_get m i and q2, _, _ = Fsa.state_get m j in
		let tid, _, _ =
		  match l_opt with
		  | None -> failwith "Ldllts.read_in"
		  | Some l -> Fsa.sigma_get m l
		in
		eprintf " (%s: %s->%s)" tid q1 q2)
	      edges;
	    n + List.length edges)
	  0 (Fsa.alist_of_delta m)
      in
      eprintf " (%d)\n" n
    end;

  (* xml -> rule list *)
  let rules = List.assoc "rules" alist in
  if !verbose > 0 then eprintf "** rules: %d\n" (List.length (Xml.children rules));
  let rs : rule list =
    List.map
      (function Xml.Element ("rule", attrs, [e_elt; c_elt; a_elt]) ->
	assert (List.mem_assoc "id" attrs);
	let e : string =
	  match e_elt with
	  | Xml.Element ("event", attrs, _) -> List.assoc "name" attrs
	and c : Ldlrule.condition =
	  match c_elt with
	  | Xml.Element ("condition", _, elts) ->
	      List.fold_left
		(fun (rslt : Ldlrule.condition) -> function
		  (* rslt : condition = (c, script) *)
		  | Xml.Element ("formula", _, [Xml.PCData f]) ->
		      (*eprintf "** formula %S\n" f;*)
		      formula_of_string f, snd rslt
		  | Xml.Element ("script", _, [Xml.PCData s]) ->
		      fst rslt, Some s
		  | Xml.Element ("script", _, []) ->
		      fst rslt, None
		  | Xml.Element (tag, _, _) ->
		      failwith (sprintf "read_in: (condition) %s" tag)
		  | _ -> failwith "read_in")
		(Ldl_atomic "false", None) elts
	and a : Ldlrule.action =
	  match a_elt with
	  | Xml.Element ("action", _, elts) ->
	      List.fold_left
		(fun (rslt : Ldlrule.action) -> function
		  | Xml.Element ("ensure", _, [Xml.PCData f]) ->
		      (*eprintf "** ensure %S\n" f;*)
		      (fst rslt @ [Act_ensure (formula_of_string f)], snd rslt)
		  | Xml.Element ("raise", attrs, _) ->
		      (fst rslt @ [Act_raise (List.assoc "event" attrs)], snd rslt)
		  | Xml.Element ("choice", _, elts') ->
		      let names =
			List.map
			  (function
			      Xml.Element ("raise", attrs, _) -> List.assoc "event" attrs
			    | _ -> failwith "** illegal choice")
			  elts' in
		      (fst rslt @ [Act_raise_sum names], snd rslt)
		  | Xml.Element ("script", _, [Xml.PCData s]) ->
		      (fst rslt, Some s)
		  | Xml.Element ("script", _, []) ->
		      rslt
		  | Xml.Element (tag, _, _) ->
		      failwith (sprintf "read_in: (action) %s" tag)
		  | _ -> failwith "read_in")
		([], None) elts
	in
	gen_id "r", e, c, a)
      (Xml.children (List.assoc "rules" alist))
  in      
  alist, m, rs

(** helpers *)

(* collect_transitions m returns [(qid1, l, qid2); ..] *)

let collect_transitions (m : t) =
  let edges : (int * (int option * int) list) list = Fsa.alist_of_delta m in
  List.fold_left
    (fun (rslt : (string * label * string) list) (i, nexts) ->
      List.fold_left
	(fun rslt (lab_opt, j) ->
	  let qid1, qid2 = state_name m i, state_name m j in
	  match lab_opt with
	  | None -> failwith "collect_transitions"
	  | Some k -> rslt @ [qid1, Fsa.sigma_get m k, qid2])
	rslt nexts)
    [] edges

(* detect_final *)

let rec detect_final (m : t) =
  let _, final = detect_final_rec m ([], []) 0 in final

and detect_final_rec m (visited, final) i =
  if List.mem i visited then (visited, final) else
  if sink_p m i then
    visited @ [i], final @ [i]
  else
    let _, acc, _ = Fsa.state_get m i
    and nexts = Fsa.delta_get m i in
    if acc && last_p m i then
      visited @ [i], final @ [i]
    else
      let next_indices = List.map (fun (_, j) -> j) nexts in
      List.fold_left (detect_final_rec m) (visited @ [i], final) next_indices

and sink_p m i =
  let nexts = Fsa.delta_get m i in
  try
    let _ = List.find (fun (_, j) -> j <> i) nexts in false
  with Not_found ->
    true

and last_p m i =
  let _, acc, _ = Fsa.state_get m i in
  if not acc then false else
  let nexts = Fsa.delta_get m i in
  try
    let _ = List.find (fun (_, j) -> j <> i && not (sink_p m j)) nexts in false
  with Not_found ->
    true

(** printers *)

(* <,>,& -> &lt;,&gt;,&amp; *)
let rec escape out (str : string) =
  let str_seq : string list = escape_rec str 0 0 (String.length str) []
  in List.iter out str_seq

and escape_rec str prev curr len (rslt : string list) =
  let tab = ['"', "&quot;"; '&', "&amp;"; '\'', "&apos;"; '<', "&lt;"; '>', "&gt;"] in
  if curr >= len then rslt @ [String.sub str prev (curr - prev)] else
  if List.mem_assoc str.[curr] tab
  then
    let rslt' = rslt @ [String.sub str prev (curr - prev); List.assoc str.[curr] tab] in
    escape_rec str (curr + 1) (curr + 1) len rslt'
  else
    escape_rec str prev (curr + 1) len rslt

let print_states_in_xml out (m : t) =
  out "<states>\n";
  let final : int list = detect_final m in
  List.iter
    (fun (i, (id, acc, w)) ->
      out (sprintf "<state id=%S %s=\"true\"%s%s>"
	     id (if acc then "accepting" else "rejecting")
	     (if i = 0 then " initial=\"true\"" else "")
	     (if List.mem i final then " final=\"true\"" else ""));
      out "<formula>";
      escape out (string_of_formula (simp w));
      out "</formula>";
      out "</state>\n")
    (Fsa.alist_of_states m);
  out "</states>\n"

let rec print_transitions_in_xml out (m : t) =
  out "<transitions>\n";
  let final = detect_final m in
  List.iter
    (fun (i, delta) ->
      List.iter
	(fun (l_opt, j) ->
	  let id, props, es =
	    match l_opt with
	    | Some l ->
		let id, props, es = Fsa.sigma_get m l in id, props, es
	    | None -> failwith "print_transitions_in_xml" in
	  assert (List.length es = 1);
	  let e = List.hd es in
	  let qid1, qid2 = state_name m i, state_name m j in
	  out (sprintf "<transition id=%S from=\"%s\" to=\"%s\" event=%S%s>"
		 id qid1 qid2 e
		 (* _skip -> _init/_accept/_reject *)
		 (match subst_event_name m 0 final (i, j) e with
		 | None -> ""
		 | Some e' -> sprintf " alt_event=%S" e'));
	  out "<formula>";
	  escape out (string_of_formula (simp (Ldl_conj props)));
	  out "</formula>";
	  out "</transition>\n")
	delta)
    (Fsa.alist_of_delta m);
  out "</transitions>\n";
  ()

and subst_event_name m (initial: int) (final : int list) (i, j) e =
  if e <> "_skip" then None else
  let _, acc, _ = Fsa.state_get m j in
  if List.mem j final then
    Some (if acc then "_accept" else "_reject")
  else
    if i = initial && j <> initial then Some "_init" else None

(* alist = [(rid, [tid; ...]); ..] *)
let rec print_rules_in_xml out (m : t) alist (rs : rule list) =
  out "<rules>\n";
  let alist2 : (string * formula) list =
    (* [(qid, w); ..] *)
    List.map (fun (_, (id, _, w)) -> (id, w)) (Fsa.alist_of_states m) in
  let alist3 : (string * (formula * formula)) list =
    (* [(tid, (w1, w2)]; ..] *)
    let edges : (string * label * string) list = collect_transitions m in
    (* [(qid1, (tid, _, _), qid2); ..] *)
    List.map
      (fun (qid1, (tid, _, _), qid2) ->
	assert (List.mem_assoc qid1 alist2);
	assert (List.mem_assoc qid2 alist2);
	(tid, (List.assoc qid1 alist2, List.assoc qid2 alist2)))
      edges in
  List.iter
    (fun r ->
      if List.mem_assoc (rule_id r) alist then
	Ldlrule.print_rule_in_xml out (List.assoc (rule_id r) alist) alist3 r)
    rs;
  out "</rules>\n";
  ()

(* tid = id of transition to which r is applicable.
   alist = [(tid, (w1, w2)); ..] *)
(*
and print_rule_in_xml out tid_seq alist (r : rule) =
  let rid, e, (c, c_opt), (a, a_opt) = r in
  out (sprintf "<rule id=%S>\n" rid);

  (* event *)
  out (sprintf "<event name=%S/>\n" e);

  (* condition *)
  out "<condition>";
  out (sprintf "<formula%s>" (if propositional c then "" else " modal=\"true\""));
  escape out (string_of_formula (simp c));
  out "</formula>\n";
  (match c_opt with Some str -> out "<script>"; escape out str; out "</script>\n" | None -> ());
  out "</condition>\n";

  (* action *)
  let post : formula list =
    List.fold_left
      (fun rslt -> function Act_ensure f -> rslt @ [f] | _ -> rslt)
      [] a in
  out "<action>";
  out "<formula>";
  escape out (string_of_formula (simp (Ldl_conj post)));
  out "</formula>\n";
  List.iter
    (function
      | Act_raise e -> out (sprintf "<raise event=%S/>\n" e)
      | Act_raise_sum es ->
	  out "<choice>";
	  List.iter (fun e -> out @@ sprintf "<raise event=%S/>" e) es;
	  out "</choice>\n"
      | _ -> ())
    a;
  (match a_opt with Some str -> out "<script>"; escape out str; out "</script>\n" | None -> ());
  out "</action>\n";

  (* applicable *)
  out "<applicable>";
  List.iter
    (fun tid ->
      assert (List.mem_assoc tid alist);
      let w1, w2 = List.assoc tid alist in
      let b, certainty_opt = Ldlrule.applicable r (w1, w2) in
      let certainty = match certainty_opt with None -> 0 | Some v -> v in
      (*
      assert (b || certainty land 0b11 = 0 || certainty land 0b1100 = 0);
      if deg = 0 then
	eprintf "** inapplicable: rid=%s tid=%s w1=%S c=%S w2=%S\n"
	  rid tid
	  (string_of_formula w1)
	  (string_of_formula c)
	  (string_of_formula w2);
       *)
      if b then out (sprintf "<tr name=%S certainty=\"%d\"/>" tid certainty))
    tid_seq;
  out "</applicable>\n";

  out "</rule>\n";
  ()
*)

let debug_print m =
  output_string stderr ";; debug_print\n";
  (*output_string stderr (show_nfa m);*)
  (* states *)
  (*
  output_string stderr "states\n";
  List.iter
    (fun (i, q) -> eprintf "%d: %s\n" i (show_state q))
    (Fsa.alist_of_states m);
   *)
  (* transitions *)
  (*
  output_string stderr "transitions\n";
  List.iter
    (fun (i, delta) ->
      output_string stderr ((state_name m i) ^ ":");
      List.iter
	(function
	  | None, j   -> eprintf " %s" (state_name m j)
	  | Some l, j -> eprintf " %s -> %s" (show_label (Fsa.sigma_get m l)) (state_name m j))
	delta;
      output_string stderr "\n")
    (Fsa.alist_of_delta m);
   *)
  flush_all ();
  ()
