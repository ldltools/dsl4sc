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
open Ldlrule
open Printf

type t =
    (state, label) Nfa.nfa

and state =
    string * bool * formula
      (* (id, accepting, possible_world) *)

and label =
    string * formula list * string list
      (* (id, next_world, event_name list) *)

[@@deriving show, yojson]

let verbose = ref 0

type rule = Ldlrule.rule

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

(* id *)
let _id = ref 0
let gen_id (prefix : string) =
  incr _id; prefix ^ (string_of_int !_id)

(* in_channel -> t * rule list *)
let read_in (ic : in_channel) =
  let xml = Xml.parse_in ic in
  let elts : Xml.xml list =
    (* elts = [propositions; states; transitions; rules] *)
    let rec trav (rslt : Xml.xml list) = function
      | Xml.Element ("dfa", _, children) ->
	  children
      | Xml.Element (_, _, children) ->
	  List.fold_left (Xml.fold trav) rslt children
      | _ -> rslt
    in trav [] xml in
  assert (List.length elts = 4);
  let props, nodes, edges, rules =
    (match elts with [props; nodes; edges; rules] -> props, nodes, edges, rules)
  in

  (* xml -> t *)
  let m : t = Nfa.make () in
  (* states *)
  List.iter
    (function Xml.Element ("state", attrs, _) ->
      assert (List.mem_assoc "id" attrs);
      let id = List.assoc "id" attrs in
      Nfa.add_state m
	(id, List.mem_assoc "accepting" attrs, Ldl_atomic "false")
	(Some (int_of_string id));
      ())
    (Xml.children nodes);
  (* transitions *)
  List.iter
    (function Xml.Element ("transition", attrs, _) ->
      let n1, n2 = List.assoc "from" attrs, List.assoc "to" attrs
      and lab = List.assoc "label" attrs
      and events =
	if not (List.mem_assoc "event" attrs) then [] else
	tokenize (List.assoc "event" attrs) in
      let props : formula list =
	List.fold_left
	  (fun rslt str ->
	    let i = if str.[0] = '!' then 1 else 0 in
	    if String.length str >= i + 2 && String.sub str i 2 = "_b"
	    then rslt (* skip events *)
	    else rslt @ [formula_of_string str])
	  [] (tokenize lab)
      in
      Nfa.add_transition m
	(int_of_string n1, Some (gen_id "t", props, events), int_of_string n2);
      ())
    (Xml.children edges);

  (* xml -> rule list *)
  let rs : rule list =
    List.map
      (function Xml.Element ("rule", _, [e_elt; c_elt; a_elt]) ->
	let e : string =
	  match e_elt with
	  | Xml.Element ("event", attrs, _) -> List.assoc "name" attrs
	and c : condition =
	  match c_elt with
	  | Xml.Element ("condition", _, elts) ->
	      List.fold_left
		(fun (rslt : condition) -> function
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
	and a : action =
	  match a_elt with
	  | Xml.Element ("action", _, elts) ->
	      List.fold_left
		(fun (rslt : action) -> function
		  | Xml.Element ("ensure", _, [Xml.PCData f]) ->
		      (*eprintf "** ensure %S\n" f;*)
		      (fst rslt @ [Act_ensure (formula_of_string f)], snd rslt)
		  | Xml.Element ("raise", attrs, _) ->
		      (fst rslt @ [Act_raise (List.assoc "event" attrs)], snd rslt)
		  | Xml.Element ("script", _, [Xml.PCData s]) ->
		      (fst rslt, Some s)
		  | Xml.Element ("script", _, []) ->
		      rslt
		  | Xml.Element ("choice", _, _) ->
		      eprintf "** choice action not supported (skipped)\n";
		      rslt
		  | Xml.Element (tag, _, _) ->
		      failwith (sprintf "read_in: (action) %s" tag)
		  | _ -> failwith "read_in")
		([], None) elts
	in
	gen_id "r", e, c, a)
      (Xml.children rules)
  in      
  props, m, rs

(* collect_transitions m returns [(qid1, l, qid2); ..] *)

let collect_transitions m =
  let edges = Nfa.alist_of_delta m in
  List.fold_left
    (fun (rslt : (string * label * string) list) (i, nexts) ->
      List.fold_left
	(fun rslt (lab_opt, j) ->
	  let qid1, qid2 = string_of_int i, string_of_int j in
	  match lab_opt with
	  | None -> failwith "collect_transitions"
	  | Some k -> rslt @ [qid1, Nfa.sigma_get m k, qid2])
	rslt nexts)
    [] edges

(* detect_final *)

let rec detect_final (m : t) =
  let _, final = detect_final_rec m ([], []) 1 in final

and detect_final_rec m (visited, final) i =
  if List.mem i visited then (visited, final) else
  if sink_p m i then
    visited @ [i], final @ [i]
  else
    let _, acc, _ = Nfa.get_state m i
    and nexts = Nfa.delta_get m i in
    if acc && last_p m i then
      visited @ [i], final @ [i]
    else
      let next_indices = List.map (fun (_, j) -> j) nexts in
      List.fold_left (detect_final_rec m) (visited @ [i], final) next_indices

and sink_p m i =
  let nexts = Nfa.delta_get m i in
  try
    let _ = List.find (fun (_, j) -> j <> i) nexts in false
  with Not_found ->
    true

and last_p m i =
  let _, acc, _ = Nfa.get_state m i in
  if not acc then false else
  let nexts = Nfa.delta_get m i in
  try
    let _ = List.find (fun (_, j) -> j <> i && not (sink_p m j)) nexts in false
  with Not_found ->
    true

(* split_transitions *)

let rec split_transitions (m : t) =
  if !verbose > 0 then (eprintf ";; split_transitions\n"; flush_all ());
  let final : int list = detect_final m in
  (*eprintf "** final:"; List.iter (eprintf " %d") final; eprintf "\n";*)
  let edges = Nfa.alist_of_delta m in
  List.iter (split_transition m final) edges;
  m

and split_transition (m : t) final (i, nexts) =
  List.iter
    (function (lab_opt, j) ->
      match lab_opt with
      | None -> ()
      | Some k ->
	  let (id, props, es) : label = Nfa.sigma_get m k in
	  if !verbose > 1 then
	    eprintf "** del_transition: %d -(%d)-> %d\n" i (List.length es) j;
	  Nfa.del_transition m (i, Some k, j);
	  let labs : label list = List.map (fun e -> gen_id "t", props, [e]) es in
	  List.iter
	    (fun (id, props, es) ->
	      assert (List.length es = 1);
	      let e' = List.hd es in
	      if !verbose > 1 then
		(eprintf "** add_transition: %d -> %d\n" i j; flush_all ());
	      Nfa.add_transition m (i, Some (id, props, [e']), j); ())
	    labs)
    nexts

(* update_states *)

let rec update_states (m : t) =
  let qs : (int * state) list = Nfa.alist_of_states m
  and edges = Nfa.alist_of_delta m in
  List.iter (update_state m edges) qs;
  m

and update_state m es (i, (id, accepting, f)) =
  let fs : formula list =
    List.fold_left
      (* for all edge (_, i) *)
      (fun rslt (_, nexts) ->
	List.fold_left
	  (fun rslt (l_opt, i') ->
	    if i' <> i then rslt else
	    match l_opt with
	    | None -> failwith "update_state"
	    | Some l ->
		let (_, props, _) = Nfa.sigma_get m l in rslt @ [Ldl_conj props])
	  rslt nexts)
      [] es
  in

  (* f -> f' -> f'' *)
  let f' = Ldl_disj (f :: fs) in
  if !verbose > 0 then
    (eprintf ";; updadate_state (qid=%s): %s =(simp)=> " id (string_of_formula f'); flush_all ());

  let simp f =
    (*Printf.eprintf "** simp (%s)\n" (string_of_formula f); flush_all ();*)
    Ldlsimp.simp f
    (*Ldlsimp.resolve f*)
    (*let g = Ldlsimp.simp f in try Ldlsimp.resolve g |> Ldlsimp.simp_sort |> Ldlsimp.simp_uniq with Not_found -> g*)
    (*try Ldl.resolve f with _ -> try Ldl.simp f with _ -> f*)
  in
  let f'' = simp f' in
  if !verbose > 0 then (eprintf "%s\n" (string_of_formula f''); flush_all ());

  Nfa.set_state m i (id, accepting, f'')

(* find_applicable_rules *)

let rec find_applicable_rules (m : t) (rs : rule list) =
  if !verbose > 0 then (eprintf ";; find_applicable_rules\n"; flush_all ());
  let edges = Nfa.alist_of_delta m in
  List.fold_left
    (fun rslt (i, nexts) ->
      List.fold_left
	(fun rslt (l_opt, j) ->
	  let l : label =
	    match l_opt with
	    | None -> failwith "find_applicable_rules"
	    | Some l ->  Nfa.sigma_get m l
	  in rslt @ find_applicable_rules_rec m (i, l, j) rs)
	rslt nexts)
    [] edges

and find_applicable_rules_rec m (i, l, j) rs =
  let tid, props, es = l in
  match es with
  | [] -> []
  | [e] ->
      List.fold_left
	(fun rslt r ->
	  let rid, e', (c, _), (acts, _) = r in
	  let certainty = rule_applicable m (i, j) r in
	  if !verbose > 0 then
	    (eprintf ";; applicable? (rid=%s, tid=%s): %d\n" rid tid certainty;
	     flush_all ());
	  if e' = e
	  then rslt @ [rid, tid]
	  else rslt)
	[] rs
  | _ ->
      (*failwith "find_applicable_rules_rec";*)
      []

(* returns: 0 = inapplicable, 0b0001-0b1111 = (conditionally) applicable *)
and rule_applicable m (i, j) r =
  let _, _, (w1 : formula) = Nfa.get_state m i
  and _, _, (w2 : formula) = Nfa.get_state m j in
  let b, certainty = Ldlrule.applicable r (w1, w2) in
  assert (b || certainty land 0b11 = 0 || certainty land 0b1100 = 0);
  certainty

(* update *)

let rec update m rs =
  let m' : t = m |> update_states |> split_transitions in
  let alist1 : (string * string) list = find_applicable_rules m' rs in
    (* alist1 = [(rid, tid); ..] *)
  let alist2 : (string * string list) list = aggregate_transitions alist1 in
    (* alist2 = [(rid, [tid; ..]); ..] *)
  m', alist2

and aggregate_transitions (alist : (string * string) list) =
  if !verbose > 0 then (eprintf ";; aggregate_transitions\n"; flush_all ());
  let alist2 : (string * string) list =
    List.sort
      (fun (rid1, _) (rid2, _) ->
	let i1 = int_of_string (String.sub rid1 1 (String.length rid1 - 1))
	and i2 = int_of_string (String.sub rid2 1 (String.length rid2 - 1))
	in if i1 = i2 then 0 else if i1 < i2 then -1 else 1)
      alist in
  let alist3 : (string * string list) list =
    match alist2 with
    | [] -> []
    | (rid, tid) :: rest ->
	let (rid', tid_seq'), rslt =
	  List.fold_left
	    (fun ((rid1, tid_seq1), rslt) (rid2, tid2) ->
	      if rid1 = rid2 then
		(rid1, tid_seq1 @ [tid2]), rslt
	      else
		(rid2, [tid2]), rslt @ [rid1, tid_seq1])
	    ((rid, [tid]), []) rest
	in rslt @ [rid', tid_seq']
  in alist3

(* printers *)

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
  let final = detect_final m in
  List.iter
    (fun (i, (id, acc, w)) ->
      out (sprintf "<state id=%S %s=\"true\"%s>"
	     id (if acc then "accepting" else "rejecting")
	     (if List.mem i final then " final=\"true\"" else ""));
      out "<formula>";
      escape out (string_of_formula (simp w));
      out "</formula>";
      out "</state>\n")
    (Nfa.alist_of_states m);
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
		let id, props, es = Nfa.sigma_get m l in id, props, es
	    | None -> failwith "print_transitions_in_xml" in
	  assert (List.length es = 1);
	  let e = List.hd es in
	  out (sprintf "<transition id=%S from=\"%d\" to=\"%d\" event=%S%s>"
		 id i j e
		 (match subst_event_name m final (i, j) e with
		 | None -> ""
		 | Some e' -> sprintf " alt_event=%S" e'));
	  out "<formula>";
	  escape out (string_of_formula (simp (Ldl_conj props)));
	  out "</formula>";
	  out "</transition>\n")
	delta)
    (Nfa.alist_of_delta m);
  out "</transitions>\n";
  ()

and subst_event_name m final (i, j) e =
  if e <> "_skip" then None else
  let _, acc, _ = Nfa.get_state m j in
  if List.mem j final then
    Some (if acc then "_accept" else "_reject")
  else
    if i = 1 && j <> 1 then Some "_init" else None

(* alist = [(rid, [tid; ...]); ..] *)
let rec print_rules_in_xml out (m : t) alist (rs : rule list) =
  out "<rules>\n";
  let alist2 : (string * formula) list =
    (* [(qid, w); ..] *)
    List.map (fun (_, (id, _, w)) -> (id, w)) (Nfa.alist_of_states m) in
  let alist3 : (string * (formula * formula)) list =
    (* [(tid, (w1, w2)]; ..] *)
    let edges : (string * label * string) list = collect_transitions m in
    (* [(qid1, (tid, _, _), qid2); ..] *)
    List.map
      (fun (qid1, (tid, _, _), qid2) ->
	(tid, (List.assoc qid1 alist2, List.assoc qid2 alist2)))
      edges in
  List.iter
    (fun r -> print_rule_in_xml out (List.assoc (rule_id r) alist) alist3 r)
    rs;
  out "</rules>\n";
  ()

(* tid = id of transition to which r is applicable.
   alist = [(tid, (w1, w2)); ..] *)
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
      (fun rslt -> function Act_ensure f -> rslt @ [f] | Act_raise _ -> rslt)
      [] a in
  out "<action>";
  out "<formula>";
  escape out (string_of_formula (simp (Ldl_conj post)));
  out "</formula>\n";
  List.iter
    (function Act_raise e -> out (sprintf "<raise event=%S/>\n" e) | _ -> ())
    a;
  (match a_opt with Some str -> out "<script>"; escape out str; out "</script>\n" | None -> ());
  out "</action>\n";

  (* applicable *)
  out "<applicable>";
  List.iter
    (fun tid ->
      assert (List.mem_assoc tid alist);
      let w1, w2 = List.assoc tid alist in
      let b, certainty = Ldlrule.applicable r (w1, w2) in
      assert (b || certainty land 0b11 = 0 || certainty land 0b1100 = 0);
      (*
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

let debug_print m =
  (*output_string stderr (show_nfa m);*)
  (* states *)
  output_string stderr "[states]\n";
  List.iter
    (fun (i, q) -> eprintf "%d: %s\n" i (show_state q))
    (Nfa.alist_of_states m);
  (* transitions *)
  output_string stderr "[transitions]\n";
  List.iter
    (fun (i, delta) ->
      output_string stderr ((string_of_int i) ^ ":");
      List.iter
	(function
	  | None, j   -> eprintf " %d" j
	  | Some l, j -> eprintf " %s%d" (show_label (Nfa.sigma_get m l)) j)
	delta;
      output_string stderr "\n")
    (Nfa.alist_of_delta m);
  flush_all ();
  ()

let debug_print_rule (id, e, (c, c_scr), (a, a_scr)) =
  eprintf "%s: on %s " id e;
  eprintf "when %s %s" (string_of_formula c)
    (match c_scr with None -> "" | Some str -> "{" ^ str ^ "} ");
  eprintf "do ";
  let print1 = function
    | Act_ensure f -> eprintf "%s" (string_of_formula f)
    | Act_raise e  -> eprintf "raise %s" e in
  let _ =
    match a with
    | [] -> ()
    | [act] -> print1 act
    | act :: rest -> print1 act; List.iter (fun act -> eprintf ", "; print1 act) rest
  in
  let _ =
    match a_scr with
    | None -> ()
    | Some str -> eprintf " {%s}" str
  in
  output_string stderr "\n";
  flush_all ();
  ()
