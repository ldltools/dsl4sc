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

open Ldlsat
open Ldlsat.Ldl
open Dsl4sc
open Printf

(* for debugging *)

let verbose = ref 0
let verbosity_set n =
  verbose := n
let verbosity_get () =
  !verbose

(** rule *)

module Simp = Oracle.Simp
module Rule = struct

type rule =
    string * event * condition * action * condition
      (* (id, e, c, a, post) *)

and event =
    string * string option

and condition =
    Ldl.formula * string option
      (* (c, script) *)

and action =
    action_unit list * string option
      (* (a, script) *)

and action_unit =
  | Act_ensure of Ldl.formula
  | Act_raise of string
  | Act_raise_sum of string list

[@@deriving show, yojson]

type t = rule

let pp = pp_rule
let show = show_rule

(*
let verbose = ref 0

let verbosity_set n =
  verbose := n
      
let verbosity_get () =
  !verbose
*)

let propositional f = not @@ Ldl.modal f

module SAT = Oracle.SAT
module Simp = Oracle.Simp

let id_get (id, _, _, _, _) = id

(* remove_modality *)
let rec remove_modality (f : Ldl.formula) =
  if propositional f then f else
  match f with
  | Ldl_modal (Mod_ex, _, g) -> remove_modality g
  | _ -> failwith ("[remove_modality] invalid formula: " ^ string_of_formula f)

(* simplify *)
let simp (id, e, (c, c_opt), (a, a_opt), (post, post_opt)) =
  let c' =
    Simp.simp (if propositional c then c else remove_modality c)
  and post' =
    Simp.simp post
  in
  assert (propositional c' && propositional post');
  id, e, (c', c_opt), (a, a_opt), (post', post_opt)

(* certainty (b0, b1, b2, b3) : 0b0000 - 0b1111
 *)
let certainty b0 b1 b2 b3 =
  let int_of_bool = function true -> 1 | false -> 0 in
  (int_of_bool b0) lor (int_of_bool b1) lsl 1 lor
  (int_of_bool b2) lsl 2 lor (int_of_bool b3) lsl 3 

(** applicable r (w1, w2) examines
    whether r is applicable to a transition from w1 to w2, or not

    - if there is no chance, it returns false
    - if there is a chance, it returns true with certainty.

   1. examine (pre(r) ∩ w1) and (post(r) ∩ w2)
      if either is empty, then returns
      - false
      - certainty = (b0, 0, b2, 0) where
        b0 = pre(r) ∩ w1 is non-empty?, 
        b2 = post(r) ∩ w2 is non-empty?

   2. otherwise, it returns true with a certainty value computed as follows.

      certainty = (1, b1, 1, b3) where
        b1 = (w1 -> pre(r)) is valid or not
        b2 = (post(r) -> w2) is valid or not
 *)

let rec applicable r (w1, w2) =
  (*eprintf "(applicable)"; flush_all ();*)
  let rid, _, (pre, _), (a, _), (post, _) = r
  in

  (* propositionalize pre/post conditions *)
  let rec remove_ex f =
    match f with
    | _ when propositional f -> f
    | Ldl_modal (Mod_ex, _, g) -> remove_ex g
    | _ -> invalid_arg ("[remove_ex] " ^ string_of_formula f)
  in
  let pre = if propositional pre then pre else remove_ex pre
  and post = if propositional post then post else Ldl_atomic "true"
  in

  assert (propositional pre && propositional post);
  assert (propositional w1 && propositional w2);

  (*
  let c =
    if propositional c then c else
    let c' = remove_modality c
    in
    if verbosity_get () > 1 then
      eprintf "(** chop off modality: %s ==> %s)"
	(string_of_formula c) (string_of_formula c');
    c'
  in
  assert (propositional c);
   *)

  (* post-condition of r *)
  (*
  let post =
    List.fold_left
      (fun rslt -> function Act_ensure f -> rslt @ [f] | _ -> rslt)
      [] a in
  let post : formula = Simp.simp (Ldl_conj post) in
  assert (propositional post);
   *)

  if post = Ldl_atomic "false" then false, None else
  let _ = () in
  (*
  let simp f =
    assert (propositional f);
    if verbosity_get () > 1 then
      (eprintf "(simp: %s)\n" (Ldl.string_of_formula f); flush_all ());
    (*Ldlsimp.resolve f |> Ldlsimp.flatten*)
    Ldlsimp.simp f
    (*Ldlsimp.simp_safe f*)
  in
   *)

  (* examine rule applicability -- similar to the consequence rule in the Hoare logic
     w1 -> c(=pre), {c}rule{a}, a(=post) -> w2
     ----------------------------------------- (applicable unconditionally)
                   {w1}rule{w2}
   *)

  (* possible worlds *)
  (*
  Printf.eprintf ";; applicable? (rid=%s): w1 = %S, c = %S, "
    rid (string_of_formula w1) (string_of_formula c);
  Printf.eprintf "a = %S, w2 = %S\n"
    (string_of_formula post) (string_of_formula w2);
  flush_all ();
   *)

  (* check1:  (pre & w1) & (post & w2) *)
  let check1, detail1 = applicable_check1 (pre, post) (w1, w2) in
  if not check1 then false, detail1 else
  
  (* check2 *)
  true, Some 1

  (* (w1 -> c) guarantees unconditional applicability *)
  (* (post -> w2) *)
  (*
    let g1 = simp (Ldl_impl (w1, c)) in
    let c1 = (g1 = Ldl_atomic "true") in
    (* (post -> w2) *)
    let g2 = simp (Ldl_impl (post, w2)) in
    let c2 = (g2 = Ldl_atomic "true") in

    assert (certainty true c1 true c2 > 0);
    true, Some (certainty true c1 true c2)
   *)

and applicable_check1 (pre, post) (w1, w2) =
  (* (pre & w1) should hold in some possible worlds *)
  let b1, _ =
    (*
    let f1 = Simp.simp (Ldl_conj [pre; w1])
    in f1 = Ldl_atomic "false"
    output_string stderr "\n[pre & w1]";
    print_formula (output_string stderr) (Ldl_conj [pre; w1]); output_string stderr "\n";
    flush stderr;
     *)
    Ldl_conj [pre; w1] |> Toysat.tseitin |> SAT.solve 

  (* (post & w2) *)
  and b2, _ =
    (*
    let f2 = Simp.simp (Ldl_conj [post; w2])
    in f2 = Ldl_atomic "false"
    output_string stderr "[post & w2]";
    print_formula (output_string stderr) (Ldl_conj [pre; w1]); output_string stderr "\n";
    flush stderr;
     *)
    Ldl_conj [post; w2] |> Toysat.tseitin |> SAT.solve 
  in

  if not b1 || not b2 then
    (* case:  !(pre & w1) or !(post & w2) -- no chance to apply this rule *)
    false, Some (certainty b1 false b2 false)
  else
    true, None

(** printing *)

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

(* tid = id of transition to which r is applicable.
   alist = [(tid, (w1, w2)); ..] *)
let print_rule_in_xml out tid_seq alist (r : t) =
  let rid, (e, e_opt), (c, c_opt), (a, a_opt), _ = r in
  out (sprintf "<rule id=%S>\n" rid);

  (* event *)
  out (sprintf "<event name=%S" e);
  out (match e_opt with None -> "/>\n" | Some s -> sprintf "><script>%s</script></event>\n" s);

  (* condition *)
  out "<condition>";
  out (sprintf "<formula%s>" (if propositional c then "" else " modal=\"true\""));
  escape out (string_of_formula (Simp.simp c));
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
  escape out (string_of_formula (Simp.simp (Ldl_conj post)));
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
      let b, certainty_opt = applicable r (w1, w2) in
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

let debug_print_rule (id, (e, e_opt), (c, c_scr), (a, a_scr), (post, post_scr)) =
  eprintf "%s: on %s " id e;
  eprintf "when %s %s" (string_of_formula c)
    (match c_scr with None -> "" | Some str -> "{" ^ str ^ "} ");
  let print1 = function
    | Act_ensure f -> eprintf "ensure %s" (string_of_formula f)
    | Act_raise e  -> eprintf "raise %s" e
    | Act_raise_sum [e]  -> eprintf "raise %s" e
    | Act_raise_sum (e :: rest)  ->
	eprintf "raise %s" e; List.iter (fun e -> eprintf " + %s" e) rest
  in let _ =
    match a with
    | [] -> ()
    | [act] -> print1 act
    | act :: rest -> print1 act; List.iter (fun act -> eprintf ", "; print1 act) rest
  in
  let _ =
    match a_scr with
    | None -> ()
    | Some str ->
	(*eprintf " {%s}" str*)
	()
  in
  output_string stderr "\n";
  flush_all ();
  ()

end

(** executable model *)

type model =
    { fsa : (state, label) Fsa.t;
      mutable rules : Rule.t list;
      mutable rules_map : (string * string list) list;
      elements : (string * Xml.xml) list
	[@printer fun fmt elts -> ()];
    }

and state =
    string * bool * Ldl.formula
      (* (qid, accepting, possible_world) *)

and label =
    string * formula list * string list
      (* (tid, next_world, event_name list) *)

[@@deriving show]
(*[@@deriving show, yojson]*)

type t = model
type rule = Rule.t


let rule_id = Rule.id_get

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
  let name, _, _ = Fsa.state_get m.fsa i in name

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

(* in_channel -> t *)
let from_channel (ic : in_channel) =
  if !verbose > 0 then eprintf "* [Model.from_channel]\n";

  let xml = Xml.parse_in ic
  in let legal_names = 
    ["variables"; "states"; "transitions"; "rules"; "scripts"]
  in let alist : (string * Xml.xml) list =
    let rec trav (rslt : (string * Xml.xml) list) = function
      | Xml.Element ("dfa", _, children) ->
	  List.map
	    (fun child ->
	      match child with
	      | Xml.Element (name, _, _) when List.mem name legal_names ->
		  name, child
	      | Xml.Element (name, _, _) ->
		  failwith ("[Ldlmodel.read_in] unknown element: " ^ name)
	      | _ ->
		  failwith "[Ldlmodel.read_in] non-element encountered")
	    children
      | Xml.Element (_, _, children) ->
	  List.fold_left (Xml.fold trav) rslt children
      | _ -> rslt
    in trav [] xml in
  assert (List.length alist >= 3);

  (* xml -> t *)
  let m : t = { fsa = Fsa.make (); rules = []; rules_map = []; elements = alist }
  in

  (* states *)
  let nodes = List.assoc "states" alist in
  if !verbose > 0 then eprintf "* states: %d\n" (List.length (Xml.children nodes));
  List.iter
    (function Xml.Element ("state", attrs, _) ->
      assert (List.mem_assoc "id" attrs);
      let qid : string = List.assoc "id" attrs in
      let i = Fsa.state_add m.fsa (qid, List.mem_assoc "accepting" attrs, Ldl_atomic "false") in
      if !verbose > 1 then eprintf "  state: %d, %s\n" i qid;
      ())
    (Xml.children nodes);
  if !verbose > 0 then
    (eprintf "* states:";
     List.iter (fun (i, (qid, _, _)) -> eprintf " (%d,%s)" i qid) (Fsa.alist_of_states m.fsa);
     eprintf "\n";
     flush stderr);

  (* transitions *)
  let edges = List.assoc "transitions" alist in
  if !verbose > 1 then eprintf "* transitions: %d\n" (List.length (Xml.children edges));
  List.iter
    (function Xml.Element ("transition", attrs, _) ->
      assert (List.mem_assoc "id" attrs);
      let tid = List.assoc "id" attrs in
      let q1, q2 = List.assoc "from" attrs, List.assoc "to" attrs in
      let n1, n2 =
	let states = Fsa.alist_of_states m.fsa
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
      Fsa.transition_add m.fsa n1 (Some (Fsa.sigma_add m.fsa (tid, props, events)), n2);
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
      eprintf "* transitions:";
      let n =
	List.fold_left
	  (fun n (i, edges) ->
	    List.iter
	      (fun (l_opt, j) ->
		let q1, _, _= Fsa.state_get m.fsa i and q2, _, _ = Fsa.state_get m.fsa j in
		let tid, _, _ =
		  match l_opt with
		  | None -> failwith "Ldllts.read_in"
		  | Some l -> Fsa.sigma_get m.fsa l
		in
		eprintf " (%s: %s->%s)" tid q1 q2)
	      edges;
	    n + List.length edges)
	  0 (Fsa.alist_of_delta m.fsa)
      in
      eprintf " (%d)\n" n;
      flush stderr;
    end;

  (* xml -> rule list *)
  let rules = List.assoc "rules" alist in
  if !verbose > 0 then eprintf "* rules: %d\n" (List.length (Xml.children rules));
  let rs : rule list =
    List.map
      (function Xml.Element ("rule", attrs, [e_elt; c_elt; a_elt]) ->
	assert (List.mem_assoc "id" attrs);
	let e : Rule.event =
	  match e_elt with
	  | Xml.Element ("event", attrs, elts) ->
	      List.assoc "name" attrs,
	      match List.find_opt (function Xml.Element ("script", _, _) -> true) elts with
	      | Some (Xml.Element ("script", _, [Xml.PCData s])) -> Some s
	      |	_ -> None
	and c : Rule.condition =
	  match c_elt with
	  | Xml.Element ("condition", _, elts) ->
	      List.fold_left
		(fun (rslt : Rule.condition) -> function
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
	and a : Rule.action =
	  match a_elt with
	  | Xml.Element ("action", _, elts) ->
	      List.fold_left
		(fun (rslt : Rule.action) -> function
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
	and post : Rule.condition =
	  Ldl_atomic "atomic", None
	in
	gen_id "r", e, c, a, post)
      (Xml.children (List.assoc "rules" alist))
  in      
  if !verbose > 0 then flush stderr;
  m.rules <- rs;
  m

(** helpers *)

(* collect_transitions m returns [(qid1, l, qid2); ..] *)

let collect_transitions (m : t) =
  let edges : (int * (int option * int) list) list = Fsa.alist_of_delta m.fsa in
  List.fold_left
    (fun (rslt : (string * label * string) list) (i, nexts) ->
      List.fold_left
	(fun rslt (lab_opt, j) ->
	  let qid1, qid2 = state_name m i, state_name m j in
	  match lab_opt with
	  | None -> failwith "collect_transitions"
	  | Some k -> rslt @ [qid1, Fsa.sigma_get m.fsa k, qid2])
	rslt nexts)
    [] edges

(* detect_final *)

let rec detect_final (m : t) =
  let _, final = detect_final_rec m ([], []) 0 in final

and detect_final_rec (m : t) (visited, final) i =
  if List.mem i visited then (visited, final) else
  if sink_p m i then
    visited @ [i], final @ [i]
  else
    let _, acc, _ = Fsa.state_get m.fsa i
    and nexts = Fsa.delta_get m.fsa i in
    if acc && last_p m i then
      visited @ [i], final @ [i]
    else
      let next_indices = List.map (fun (_, j) -> j) nexts in
      List.fold_left (detect_final_rec m) (visited @ [i], final) next_indices

and sink_p (m : t) i =
  let nexts = Fsa.delta_get m.fsa i in
  try
    let _ = List.find (fun (_, j) -> j <> i) nexts in false
  with Not_found ->
    true

and last_p (m : t) i =
  let _, acc, _ = Fsa.state_get m.fsa i in
  if not acc then false else
  let nexts = Fsa.delta_get m.fsa i in
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

(* print_states *)
let print_states_in_xml out (m : t) =
  if !verbose > 0 then eprintf "* [Model.print_states]\n";

  out "<states>\n";
  let final : int list = detect_final m in
  List.iter
    (fun (i, (id, acc, w)) ->
      out (sprintf "<state id=%S %s=\"true\"%s%s>"
	     id (if acc then "accepting" else "rejecting")
	     (if i = 0 then " initial=\"true\"" else "")
	     (if List.mem i final then " final=\"true\"" else ""));
      out "<formula>";
      escape out (string_of_formula (Simp.simp w));
      out "</formula>";
      out "</state>\n")
    (Fsa.alist_of_states m.fsa);
  out "</states>\n";
  flush_all ();
  ()

(* print_transitions *)
let rec print_transitions_in_xml out (m : t) =
  if !verbose > 0 then eprintf "* [Model.print_transitions]\n";

  out "<transitions>\n";
  let final = detect_final m in
  List.iter
    (fun (i, delta) ->
      List.iter
	(fun (l_opt, j) ->
	  let id, props, es =
	    match l_opt with
	    | Some l ->
		let id, props, es = Fsa.sigma_get m.fsa l in id, props, es
	    | None -> failwith "print_transitions_in_xml" in
	  (* event *)
	  let e = if es = [] then "" else List.hd es
	  in let qid1, qid2 = state_name m i, state_name m j
	  in
	  out (sprintf "<transition id=%S from=\"%s\" to=\"%s\" event=%S%s>"
		 id qid1 qid2 e
		 (* _skip -> _init/_accept/_reject *)
		 (match subst_event_name m 0 final (i, j) e with
		 | None -> ""
		 | Some e' -> sprintf " alt_event=%S" e'));
	  out "<formula>";
	  escape out (string_of_formula (Simp.simp (Ldl_conj props)));
	  out "</formula>";
	  out "</transition>\n")
	delta)
    (Fsa.alist_of_delta m.fsa);
  out "</transitions>\n";
  flush_all ();
  ()

and subst_event_name (m : t) (initial: int) (final : int list) (i, j) e =
  if e <> "_skip" then None else
  let _, acc, _ = Fsa.state_get m.fsa j in
  if List.mem j final then
    Some (if acc then "_accept" else "_reject")
  else
    if i = initial && j <> initial then Some "_init" else None

(* print_rules *)
(* alist = [(rid, [tid; ...]); ..] *)
let rec print_rules_in_xml out (m : t) =
  if !verbose > 0 then eprintf "* [Model.print_rules (%d)]\n" (List.length m.rules);
  print_rules_in_xml_helper out m m.rules_map m.rules

(* print_rules_in_xml_helper out m alist rs
   where alist is of the form [(rid, [tid; ...]); ..]
 *)
and print_rules_in_xml_helper out (m : t) alist (rs : rule list) =
  out "<rules>\n";
  let alist2 : (string * formula) list =
    (* [(qid, w); ..] *)
    List.map (fun (_, (id, _, w)) -> (id, w)) (Fsa.alist_of_states m.fsa) in
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
    (fun (r : Rule.t) ->
      if List.mem_assoc (rule_id r) alist then
	Rule.print_rule_in_xml out (List.assoc (rule_id r) alist) alist3 r)
    rs;
  out "</rules>\n";
  flush_all ();
  ()

(* tid = id of transition to which r is applicable.
   alist = [(tid, (w1, w2)); ..] *)
and print_rule_in_xml out tid_seq alist (r : rule) =
  let rid, (e, _), (c, c_opt), (a, a_opt), (post, _) = r in
  out (sprintf "<rule id=%S>\n" rid);

  (* event *)
  out (sprintf "<event name=%S/>\n" e);

  (* condition *)
  out "<condition>";
  out (sprintf "<formula%s>" (if modal c then " modal=\"true\"" else ""));
  escape out (string_of_formula (Simp.simp c));
  out "</formula>\n";
  (match c_opt with Some str -> out "<script>"; escape out str; out "</script>\n" | None -> ());
  out "</condition>\n";

  (* action *)
  let post : formula list =
    List.fold_left
      (fun rslt -> function Rule.Act_ensure f -> rslt @ [f] | _ -> rslt)
      [] a in
  out "<action>";
  out "<formula>";
  escape out (string_of_formula (Simp.simp (Ldl_conj post)));
  out "</formula>\n";
  List.iter
    (function
      | Rule.Act_raise e -> out (sprintf "<raise event=%S/>\n" e)
      | Rule.Act_raise_sum es ->
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
      let b, certainty_opt = Rule.applicable r (w1, w2) in
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

let rec to_channel ?(format = "xml") oc m =
  if !verbose > 0 then eprintf "* [Model.to_channel]\n";

  match format with
  | "caml" ->
      output_string oc (show_model m);
      output_string oc "\n"
  | "xml" | "unspecified" ->
      to_channel_xml oc m
  | _ -> invalid_arg ("[to_channel] " ^ format)

and to_channel_xml oc m =
  let elts = m.elements in

  (* output dfa (in xml) *)
  let out s = output_string oc s; flush_all ()
  in
  out "<dfa xmlns=\"https://github.com/ldltools/dsl4sc\">\n";
  out (Xml.to_string (List.assoc "variables" elts)); out "\n";

  print_states_in_xml out m;		(* states *)
  print_transitions_in_xml out m;	(* transitions *)
  (*out (Xml.to_string (List.assoc "variables" elts)); out "\n";*)
  print_rules_in_xml out m;		(* rules *)

  if List.mem_assoc "scripts" elts then
    (out (Xml.to_string (List.assoc "scripts" elts)); out "\n");
  out "</dfa>\n";
  flush_all ();
  ()

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
