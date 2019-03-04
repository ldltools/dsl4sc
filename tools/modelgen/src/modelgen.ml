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

type model = Ldlmodel.t
type rule = Ldlrule.t

let verbose = ref 0

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

(* collect_transitions m returns [(qid1, l, qid2); ..] *)
(*
let collect_transitions (m : Ldlmodel.t) =
  let edges : (int * (int option * int) list) list = Fsa.alist_of_delta m in
  List.fold_left
    (fun (rslt : (string * Ldlmodel.label * string) list) (i, nexts) ->
      List.fold_left
	(fun rslt (lab_opt, j) ->
	  let qid1, qid2 = Ldlmodel.state_name m i, Ldlmodel.state_name m j in
	  match lab_opt with
	  | None -> failwith "collect_transitions"
	  | Some k -> rslt @ [qid1, Fsa.sigma_get m k, qid2])
	rslt nexts)
    [] edges
*)

(* detect_final *)

(*
let rec detect_final (m : Ldlmodel.t) =
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
*)

(* split_transitions
   for each transition t for e1, e2, ...
   split t to t_1, t_2, ..
 *)

let rec split_transitions (m : Ldlmodel.t) =
  if !verbose > 0 then (eprintf "** split_transitions\n"; flush_all ());
  let final : int list = Ldlmodel.detect_final m in
  (*eprintf "** final:"; List.iter (eprintf " %d") final; eprintf "\n";*)
  let edges = Fsa.alist_of_delta m in
  List.iter (split_transition m final) edges;
  m

and split_transition (m : Ldlmodel.t) final (i, nexts) =
  List.iter
    (function (lab_opt, j) ->
      match lab_opt with
      | None -> ()
      | Some k ->
	  (* transition (i -tid-> j) that accompanies events es *)
	  let (tid, props, es) : Ldlmodel.label = Fsa.sigma_get m k in
	  if !verbose > 1 then
	    eprintf "  del_transition: %s (%d -> %d)\n" tid i j;

	  (* remove (i -tid-> j) *)
	  Fsa.transition_del m i (Some k, j);

	  let (labs : Ldlmodel.label list), _ =
	    List.fold_left
	      (fun (labs, l) e ->
		labs @ [(tid ^ "_" ^ (string_of_int l)), props, [e]], l + 1)
	      ([], 1) es
	  in
	  List.iter
	    (fun (tid', props, es) ->
	      assert (List.length es = 1);
	      let e' = List.hd es in
	      if !verbose > 1 then
		(eprintf "  add_transition: %s (%d -%s-> %d)\n" tid' i e' j;
		 flush_all ());
	      Fsa.transition_add m i (Some (Fsa.sigma_add m (tid', props, [e'])), j);
	      ())
	    labs)
    nexts

(* update_states *)

let rec update_states (m : Ldlmodel.t) =
  if !verbose > 0 then eprintf "** update_states (restore possible worlds)\n";
  let qs : (int * Ldlmodel.state) list = Fsa.alist_of_states m
  and edges = Fsa.alist_of_delta m in
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
		let (_, props, _) = Fsa.sigma_get m l in rslt @ [Ldl_conj props])
	  rslt nexts)
      [] es
  in

  (* f -> f' -> f'' *)
  let f' = Ldl_disj (f :: fs) in
  if !verbose > 1 then
    (eprintf "  state %s: %s =(simp)=> " id (string_of_formula f'); flush_all ());

  let simp f =
    (*Printf.eprintf "** simp (%s)\n" (string_of_formula f); flush_all ();*)
    Ldlsimp.simp f
    (*Ldlsimp.resolve f*)
    (*let g = Ldlsimp.simp f in try Ldlsimp.resolve g |> Ldlsimp.simp_sort |> Ldlsimp.simp_uniq with Not_found -> g*)
    (*try Ldl.resolve f with _ -> try Ldl.simp f with _ -> f*)
  in
  let f'' = simp f' in
  if !verbose > 1 then (eprintf "%s\n" (string_of_formula f''); flush_all ());

  Fsa.state_set m i (id, accepting, f'')

(* find_applicable_rules *)

let rec find_applicable_rules (m : Ldlmodel.t) (rs : Ldlrule.t list) =
  if !verbose > 0 then
    (eprintf "** find applicable rules for the transitions from each state\n"; flush_all ());
  let edges : (int * (int option * int) list) list = Fsa.alist_of_delta m in
  List.fold_left
    (fun rslt (i, nexts) ->
      if !verbose > 1 then
	eprintf "  state %s (%d): %d transitions\n"
	  (let qid, _, _ = Fsa.state_get m i in qid) i (List.length nexts);
      List.fold_left
	(fun rslt (l_opt, j) ->
	  let l : Ldlmodel.label =
	    match l_opt with
	    | None -> failwith "find_applicable_rules"
	    | Some k ->  Fsa.sigma_get m k
	  in rslt @ find_applicable_rules_rec m (i, l, j) rs)
	rslt nexts)
    [] edges

and find_applicable_rules_rec m (i, l, j) (rs : Ldlrule.rule list) =
  let tid, props, es = l in
  match es with
  | [] -> []
  | [e] ->
      List.fold_left
	(fun rslt (r : Ldlrule.rule) ->
	  let rid, (e', _), (c, _), (acts, _) = r in
	  if e' <> e then rslt else
	  let q1, _, _ = Fsa.state_get m i and q2, _, _ = Fsa.state_get m j in

	  if !verbose > 1 then
	    (eprintf "  applicable? (rid=%s, event=%s) to (tid=%s, %d->%d, %s-%s->%s):"
	       rid e' tid i j q1 e q2;
	     flush_all ());

	  let applicable, certainty_opt = rule_applicable m (i, j) r in
	  if !verbose > 1 then
	    (eprintf " %s" (string_of_bool applicable);
	     match certainty_opt with
	    | Some certainty -> eprintf " (certainty=0x%02x)\n" certainty
	    | None -> eprintf "\n");

	  if applicable
	  then rslt @ [rid, tid]
	  else rslt)
	[] rs
  | _ ->
      (*failwith "find_applicable_rules_rec";*)
      []

(* returns: 0 = inapplicable, 0b0001-0b1111 = (conditionally) applicable *)
and rule_applicable m (i, j) (r : Ldlrule.rule) =
  let _, _, (w1 : formula) = Fsa.state_get m i
  and _, _, (w2 : formula) = Fsa.state_get m j in
  let b, certainty_opt = Ldlrule.applicable r (w1, w2) in
  (*assert (b || certainty land 0b11 = 0 || certainty land 0b1100 = 0);*)
  b, certainty_opt

(* update *)

let rec merge (m : Ldlmodel.t) (rs : Ldlrule.t list) =
  verbose := Ldlmodel.verbosity_get ();
  let m' : Ldlmodel.t = m |> update_states |> split_transitions in
  let alist1 : (string * string) list = find_applicable_rules m' rs in
    (* alist1 = [(rid, tid); ..] *)
  let alist2 : (string * string list) list = aggregate_transitions alist1 in
    (* alist2 = [(rid, [tid; ..]); ..] *)
  m', alist2

and aggregate_transitions (alist : (string * string) list) =
  if !verbose > 0 then (eprintf "** aggregate_transitions: (generate alist of rule-to-transitions)\n"; flush_all ());
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
  in
  if !verbose > 1 then
    List.iter
       (fun (rid, tid_seq) ->
	 eprintf "  %s:" rid;
	 List.iter (eprintf " %s") tid_seq;
	 output_string stderr "\n")
       alist3;
  alist3
