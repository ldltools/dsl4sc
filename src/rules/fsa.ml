(* $Id: fsa.ml,v 1.1 2018/11/23 18:05:18 sato Exp sato $ *)
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

(** generic nondeterministic finite automaton *)

(* (Q, Σ, δ, q0, F) *)
type ('state, 'label) nfa =
    { (* states *)
      states : (int, 'state) Hashtbl.t;
        (** state_id -> state *)
      sigma : (int, 'label) Hashtbl.t;
        (** label_id -> label *)

      (* transitions *)
      delta : (int, (int option * int) list ref) Hashtbl.t;
        (** delta = [[(state_id, [(label_id, state_id); ..]); ...]  *)

      mutable initial : int;
      mutable final : int list;

      (* auxiliary *)
      mutable nnodes : int;
      mutable nedges : int;
      mutable node_max : int;
      mutable edge_max : int;
    }

type ('state, 'label) t = ('state, 'label) nfa

(*type 'label transition = int * 'label option * int*)
type transition = int * int option * int

let make () =
  let nstate = 1000 in
  let m : ('state, 'label) t =
    { states = Hashtbl.create (nstate / 4);
      sigma = Hashtbl.create 100;
      delta = (Hashtbl.create (nstate / 4));
      initial = 0;
      final = [];

      (* auxiliary *)
      nnodes = 0;
      nedges = 0;
      node_max = 0;
      edge_max = 0;
    }
  in m

let initial_get (m : ('state, 'label) nfa) =
  m.initial

let initial_set (m : ('state, 'label) nfa) i =
  m.initial <- i

let final_get (m : ('state, 'label) nfa) =
  m.final

let final_set (m : ('state, 'label) nfa) (qs : int list) =
  m.final <- qs

(** state ops *)

let state_get (m : ('state, 'label) nfa) i =
  assert (Hashtbl.mem m.states i);
  Hashtbl.find m.states i

let state_set (m : ('state, 'label) t) (i : int) (q : 'state) =
  Hashtbl.replace m.states i q;
  if not (Hashtbl.mem m.states i) then
    (m.nnodes <- m.nnodes + 1; if i > m.node_max then m.node_max <- i);
  ()

let alist_of_states (m : ('state, 'label) nfa) =
  Hashtbl.fold
    (fun i q rslt -> rslt @ [i, q])
    m.states []

let state_add m q =
  let i = m.node_max in
  assert (not (Hashtbl.mem m.states i));
  Hashtbl.add m.states i q;
  m.nnodes <- m.nnodes + 1;
  m.node_max <- m.node_max + 1;
  i

let rec state_del m i =
  assert (Hashtbl.mem m.states i);
(*
  assert (m.nnodes = List.length m.states);
  assert (List.mem_assoc i m.states);
  m.states <- List.remove_assoc i m.states;
  m.delta <- List.remove_assoc i m.delta;
  List.iter
    (fun (i, es) ->
      let es' =
	List.fold_left
	  (fun rslt (a, j) -> if j = i then rslt else rslt @ [a, j]) [] !es
      in es := es')
    m.delta;
  m.nnodes <- m.nnodes - 1;
  m.nedges <- List.fold_left (fun n (_, es) -> n + List.length !es) 0 m.delta
 *)
  assert (m.nnodes = Hashtbl.length m.states);
  Hashtbl.remove m.states i;
  m.nnodes <- m.nnodes - 1;
  m.nedges <- Hashtbl.fold (fun _ es n -> n + List.length !es) m.delta 0;

  Hashtbl.remove m.delta i;
  Hashtbl.iter
    (fun i es ->
      let es' =
	List.fold_left
	  (fun rslt (a, j) -> if j = i then rslt else rslt @ [a, j]) [] !es
      in es := es')
    m.delta

let state_mem (m : ('state, 'label) t) (i : int) =
  Hashtbl.mem m.states i

(** transition ops *)

let transition_add (m : ('state, 'label) t) i (l_opt, j) =
  if not (Hashtbl.mem m.delta i) then
    Hashtbl.add m.delta i (ref [l_opt, j])
  else
    let edges : (int option * int) list ref = Hashtbl.find m.delta i in
    if not (List.mem (l_opt, j) !edges) then
      edges := !edges @ [l_opt, j];
    ()

let transition_del (m : ('state, 'label) t) i (l_opt, j) =
  assert (Hashtbl.mem m.states i && Hashtbl.mem m.delta i);
  let edges : (int option * int) list ref = Hashtbl.find m.delta i in
  if List.mem (l_opt, j) !edges then
    edges := List.filter (fun assoc -> assoc <> (l_opt, j)) !edges;
  ()

let transition_mem (m : ('state, 'label) t) i (l_opt, j) =
  let edges : (int option * int) list ref = Hashtbl.find m.delta i in
  List.mem (l_opt, j) !edges

(** sigma *)

let sigma_get (m : ('state, 'label) nfa) i =
  Hashtbl.find m.sigma i

let sigma_set (m : ('state, 'label) nfa) i l =
  Hashtbl.replace m.sigma i l

(* label value is kept unique *)
let sigma_add (m : ('state, 'label) nfa) (l : 'label) =
  let found = ref 0 in
  try
    Hashtbl.iter
      (fun i l' -> if l = l' then (found := i; raise Exit))
      m.sigma;

    (* if l is not included in sigma, then create a new entry for l *)
    let i = Hashtbl.length m.sigma in
    Hashtbl.add m.sigma i l;
    i

  with Exit -> !found

let sigma_del (m : ('state, 'label) t) i =
  Hashtbl.remove m.sigma i

let sigma_mem (m : ('state, 'label) t) i =
  Hashtbl.mem m.sigma i

let alist_of_sigma (m : ('state, 'label) nfa) =
(*
  m.sigma
 *)
  Hashtbl.fold (fun i a rslt -> rslt @ [i, a]) m.sigma []

let sigma_iter f (m : ('state, 'label) t) =
  Hashtbl.iter f m.sigma

(** delta *)

let delta_get (m : ('state, 'label) nfa) i =
(*
  if not (List.mem_assoc i m.delta) then m.delta <- m.delta @ [i, ref []];
  assert (List.mem_assoc i m.delta);
  let es : (int option * int) list ref = List.assoc i m.delta
  in !es
 *)
  if not (Hashtbl.mem m.delta i) then Hashtbl.add m.delta i (ref []);
  assert (Hashtbl.mem m.delta i);
  let es : (int option * int) list ref = Hashtbl.find m.delta i
  in !es

let delta_set m i es =
(*
  assert (List.mem_assoc i m.delta);
  let es' = List.assoc i m.delta in es' := es
 *)
  assert (Hashtbl.mem m.delta i);
  let es' = Hashtbl.find m.delta i in es' := es

let alist_of_delta m =
(*
  List.map (fun (i, es) -> i, !es) m.delta
 *)
  Hashtbl.fold (fun i es rslt -> rslt @ [i, !es]) m.delta []

let nnodes_get m = m.nnodes
let nnodes_set m n = m.nnodes <- n
let nedges_get m = m.nedges
let nedges_set m n = m.nedges <- n

(** iteration *)

let states_iter f (m : ('state, 'label) t) =
  Hashtbl.iter f m.states

let states_fold f (m : ('state, 'label) nfa) init =
  Hashtbl.fold f m.states init

let delta_iteri f (m : ('state, 'label) t) =
(*
  List.iter
    (fun (i, es) -> f i es)
    m.delta
 *)
  Hashtbl.iter f m.delta

(** epsilon-elimination *)

(* e_closure q = {q' | q -epsilon*-> q'} *)
let rec e_closure (m : ('state, 'label) t) (q : int) =
  e_closure_rec m q []
    
and e_closure_rec m (q : int) (qs : int list) =
  if List.mem q qs then qs else
  List.fold_left
    (fun qs' -> function
      | None, q' -> e_closure_rec m q' qs'
      | Some _, _ -> qs')
    (qs @ [q]) (delta_get m q)

(* eliminate_epsilon
   m = (Q, Σ, delta, q0, F) -> m' = (Q, Σ, delta', q0, F') w/o epsilon
   where
   - delta'(q, a) = e_closure (delta (q, a))
   - F' = F ∪ {q0} (when q0 ∈ e_closure (q0), or F (o.w.)
 *)
let rec eliminate_epsilon (m : ('state, 'label) t) =
  let e_closure_tab : (int, int list) Hashtbl.t =
    (* state -> state list *)
    Hashtbl.create (Hashtbl.length m.states / 4) in
  Hashtbl.iter
    (fun q _ -> Hashtbl.add e_closure_tab q (e_closure m q))
    m.states;

  let m' : ('state, 'label) t = make () in
  Hashtbl.iter (fun i q -> state_set m' i q) m.states;
  Hashtbl.iter (fun i l -> sigma_set m' i l) m.sigma;
  Hashtbl.iter
    (fun q _ ->
      let edges' : (int option * int) list =
	List.fold_left
	  (fun rslt (l, qs) -> rslt @ List.map (fun q -> Some l, q) qs)
	  [] (labelled_edges m e_closure_tab q)
      in Hashtbl.replace m'.delta q (ref edges'))
    m.states;

  let q0 = initial_get m in
  initial_set m' q0;

  let final = final_get m in
  let final' =
    let final_includes_q0 =
      match List.find_opt (fun q -> List.mem q final) (e_closure m q0) with
      | None -> false | _ -> true
    in
    if not (List.mem q0 final) && final_includes_q0
    then final @ [q0]
    else final in
  final_set m' final';

  m'
    
(* labelled_edges m q = {(l, {q',..})} where q -epsilon*;l;epsilon*-> q' *)
and labelled_edges m e_closure_tab q =
  List.fold_left
    (fun alist q ->
      List.fold_left
	(fun alist (l, qs) ->
	  if not (List.mem_assoc l alist) then alist @ [l, qs] else
	  let qs' =
	    List.fold_left
	      (fun qs' q' -> if List.mem q' qs' then qs' else qs' @ [q'])
	      (List.assoc l alist) qs in
	  (List.remove_assoc l alist) @ [l, qs'])
	alist (labelled_edges_helper m q))
    [] (Hashtbl.find e_closure_tab q)

(* labelled_edges_helper m q = {(l, {q',..})} where q -l;epsilon*-> q' *)
and labelled_edges_helper m (q : int) =
  List.fold_left
    (fun (alist : (int * int list) list) (l_opt, q') ->
      match l_opt with
      | None -> alist
      | Some l when List.mem_assoc l alist ->
	  let qs1 : int list = List.assoc l alist
	  and qs2 : int list = e_closure m q'
	  in (List.remove_assoc l alist) @ [l, List.sort_uniq compare (qs1 @ qs2)]
      | Some l ->
	  alist @ [l, e_closure m q'])
    [] (delta_get m q)
    
(** determinization *)

(* for debugging *)
let rec dfa_p (m : ('state, 'label) t) =
  try
    let _ =
      Hashtbl.fold
	(fun i l labs -> if List.mem l labs then failwith "not dfa" else labs @ [l])
	m.sigma []
    in ();
    dfa_p_rec m [] (initial_get m)
  with
    Failure "not dfa" -> false

and dfa_p_rec m visited q =
  if List.mem q visited then true else
  let edges = delta_get m q in
  let _ =
    List.fold_left
      (fun labs (l_opt, q') ->
	match l_opt with
	| None -> failwith "not dfa"
	| Some l when List.mem l labs ->
	    failwith "not dfa"
	| Some l ->
	    let _ = dfa_p_rec m (visited @ [q]) q'
	    in labs @ [l])
      [] edges
  in true

(* ('state, 'label) t -> (int list, 'label) t *)
let rec determinize (m : ('state, 'label) t) =
  let m' : (int list, 'label) t = make () in
  Hashtbl.iter (fun i l -> sigma_set m' i l) m.sigma;
  let q0 = initial_get m in
  let s0 = state_add m' [q0] in
  initial_set m' s0;
  let visited : (int * int list) list = determinize_rec m m' s0 [] in
  let final = final_get m in
  let final' : int list =
    List.fold_left
      (fun rslt (s, _) ->
	let qs : int list = state_get m' s in
	match List.find_opt (fun q -> List.mem q final) qs with
	| None -> rslt
	| Some _ -> rslt @ [s])
      [] visited in
  final_set m' final';
  assert (dfa_p m');
  m'

and determinize_rec m m' (s : int) (visited : (int * int list) list) =
  if List.mem_assoc s visited then visited else
  let qs : int list = state_get m' s in
  let edges : (int * int list) list =
    (* edges from qs (= s) *)
    List.fold_left
      (fun rslt q ->
	List.fold_left
	  (fun rslt (l_opt, q') ->
	    match l_opt with
	    | None -> failwith "determinize_rec"
	    | Some l when not (List.mem_assoc l rslt) -> rslt @ [l, [q']]
	    | Some l ->
		let assoc = List.assoc l rslt in
		if List.mem q' assoc
		then rslt
		else List.remove_assoc l rslt @ [l, assoc @ [q']])
	  rslt (delta_get m q))
      [] qs in
  let visited' =
    List.fold_left
      (fun visited' (l, qs') ->
	let qs' = List.sort_uniq compare qs' in
	let s' =
	  match List.find_opt (fun (_, qs) -> qs' = qs) visited' with
	  | None -> state_add m' qs'
	  | Some (s', _) -> s'
	in
	(* ensure that s has no outbound 'l'-edge *)
	assert (match delta_get m' s |> List.find_opt (function Some l', _ when l' = l -> true | _ -> false) with None -> true | _ -> false);
	transition_add m' s (Some l, s');
	determinize_rec m m' s' visited')
      (visited @ [s, qs]) edges in
  visited'

(** minimization *)

(* distinguishable:
   q1 and q2 are distinguishable
   iff.
   for some input sequece w ∈ Σ*,
   - delta (q1, w) ∈ F ∧ delta (q2, w) ∉ F, or
   - delta (q1, w) ∉ F ∧ delta (q2, w) ∈ F
 *)

type distinguishable_t =
  | Dist_marked
      (** marked as 'distinguishable' *)
  | Dist_candidates of (int * int) list
      (** indicates that if the current pair becomes distinguished,
	  then the pairs in the list should also be distinguished
       *)

let distinguishable_p tbl q1 q2 =
  if q1 = q2 then false else
  let q1, q2 = min q1 q2, max q1 q2 in
  match Hashtbl.find_opt tbl (q1, q2) with
  | Some Dist_marked -> true
  | _ -> false

let rec distinguish tbl q1 q2 =
  if q1 = q2 then () else
  let q1, q2 = min q1 q2, max q1 q2 in
  match Hashtbl.find_opt tbl (q1, q2) with
  | None -> Hashtbl.add tbl (q1, q2) Dist_marked
  | Some Dist_marked -> ()
  | Some (Dist_candidates pairs) ->
      Hashtbl.replace tbl (q1, q2) Dist_marked;
      List.iter (fun (q1', q2') -> distinguish tbl q1' q2') pairs

let rec find_distinguishable (m : ('state, 'label) t) =
  let final = final_get m in
  let nstate = Hashtbl.length m.states in
  let tbl = Hashtbl.create (max 1 (nstate * nstate / 10)) in

  (* (q1, q2) in F x (Q-F) ==> q1 and q2 are distinguishable *)
  List.iter
    (fun q ->
      states_iter
	(fun q' _ ->
	  if not (List.mem q' final) then
	    let q1, q2 = min q q', max q q' in
	    assert (q1 < q2 && not (Hashtbl.mem tbl (q1, q2)));
	    Hashtbl.add tbl (q1, q2) Dist_marked)
	m)
    final;

  (* construct tbl by dynamic programming *)
  states_iter
    (fun q _ ->
      states_iter
	(fun q' _ ->
	  (* (q, q') ∈ (F x F) ∪ ((Q-F) x (Q-F)) *)
	  if q <> q' && List.mem q final = List.mem q' final then
	    find_distinguishable_helper tbl m final (min q q') (max q q'))
	m)
    m;

  tbl

and find_distinguishable_helper tbl m final q1 q2 =
  assert (q1 < q2);
  if distinguishable_p tbl q1 q2 then () else
  let edges1, edges2 = delta_get m q1, delta_get m q2 in
  try
    sigma_iter
      (fun l _ ->
	let exists1 = List.mem_assoc (Some l) edges1
	and exists2 = List.mem_assoc (Some l) edges2 in
	if exists1 && exists2 then
	  (* q1 -l-> q1', q2 -l-> q2' *)
	  let q1', q2' = List.assoc (Some l) edges1, List.assoc (Some l) edges2 in
	  match Hashtbl.find_opt tbl (q1', q2') with
	  | None ->
	      Hashtbl.add tbl (q1', q2') (Dist_candidates [q1, q2])
	  | Some Dist_marked ->
	      (* q1 and q2 are distinguishable *)
	      distinguish tbl q1 q2;
	      raise Exit;
	  | Some Dist_candidates pairs when not (List.mem (q1, q2) pairs) ->
	      Hashtbl.replace tbl (q1', q2') (Dist_candidates (pairs @ [q1, q2]))
	  | _ -> ()
	else if exists1 <> exists2 then
	  distinguish tbl q1 q2)
      m
  with
    Exit -> ()

(* (int list, 'label) t -> (int list, 'label) t *)
let minimize (m : (int list, 'label) t) =
  assert (dfa_p m);
  let tbl = find_distinguishable m
  and equiv = Hashtbl.create (max 1 (Hashtbl.length m.states / 10))
      (* q : int -> s : int *)
  and m' : (int list, 'label) t = make () in

  (* states *)
  let _ =
    states_fold
      (fun q _ s ->
	if Hashtbl.mem equiv q then s else
	let _ = 
	  (* add a new equiv class for q *)
	  let qs =
	    states_fold
	      (fun q' _ rslt ->
		if Hashtbl.mem equiv q' || distinguishable_p tbl q q' then
		  rslt
		else
		  (Hashtbl.add equiv q' s; rslt @ [q']))
	      m []
	  in state_set m' s (List.sort_uniq compare qs)
	in s + 1)
      m 0
  in

  initial_set m' (Hashtbl.find equiv (initial_get m));
  final_set m'
    (List.fold_left
       (fun final q ->
	 let s = Hashtbl.find equiv q in final @ if List.mem s final then [] else [s])
       [] (final_get m));

  (* sigma *)
  Hashtbl.iter (fun i l -> sigma_set m' i l) m.sigma;

  (* transitions *)
  states_iter
    (fun s qs ->
      let s_edges : (int option * int) list =
	List.fold_left
	  (fun rslt q ->
	    let q_edges : (int option * int) list = delta_get m q in
	    List.fold_left
	      (fun rslt -> function
		| None, _ -> failwith "minimize"
		| Some l, q' ->
		    let s' = Hashtbl.find equiv q'
		    in rslt @ if List.mem (Some l, s') rslt then [] else [Some l, s'])
	      rslt q_edges)
	  [] qs
      in
      List.iter (fun (l_opt, s') -> transition_add m' s (l_opt, s')) s_edges)
    m';

  assert (dfa_p m');
  m'

(** fixed_nfa for pretty-printing *)

(* definition w/o hashtbl *)
type ('state, 'label) fixed_nfa =
    { states : 'state option array;
      sigma : 'label option array;
      delta : (int option * int) list array;
      initial : int;
      final : int list;
    }

[@@deriving show, yojson]

let nfa2fnfa (m : ('state, 'label) nfa) =
  let node_max = Hashtbl.fold (fun i _ n -> max i n) m.states 0 in
  (*Printf.eprintf "node_max: %d\n" node_max;*)
  let states : 'state option array =
    Array.init (node_max + 1)
      (fun i ->
	if Hashtbl.mem m.states i then Some (Hashtbl.find m.states i) else None) in
  let sigma_max = Hashtbl.fold (fun i _ n -> max i n) m.sigma 0 in
  (*Printf.eprintf "sigma_max: %d\n" sigma_max;*)
  let sigma : 'label option array =
    Array.init (sigma_max + 1)
      (fun i ->
	if Hashtbl.mem m.sigma i then Some (Hashtbl.find m.sigma i) else None) in
  let delta : (int option * int) list array =
    Array.init (node_max + 1)
      (fun i ->
	if Hashtbl.mem m.delta i then !(Hashtbl.find m.delta i) else []) in
  let m' : ('state, 'label) fixed_nfa =
    { states = states;
      sigma = sigma;
      delta = delta;
      initial = initial_get m;
      final = final_get m;
    }
  in m'

let fnfa2nfa (m : ('state, 'label) fixed_nfa) =
  let states = Hashtbl.create (2 * Array.length m.states) in
  Array.iteri
    (fun i -> function Some q -> Hashtbl.add states i q | None -> ())
    m.states;
  let sigma = Hashtbl.create (2 * Array.length m.sigma) in
  Array.iteri
    (fun i -> function Some a -> Hashtbl.add sigma i a | None -> ())
    m.sigma;
  let delta = Hashtbl.create (2 * Array.length m.delta) in
  Array.iteri (fun i es -> Hashtbl.add delta i (ref es)) m.delta;
  let m' : ('state, 'label) nfa =
    { states = states;
      sigma = sigma;
      delta = delta;
      initial = m.initial;
      final = m.final;
      nnodes = Array.length m.states;
      nedges = Array.fold_left (fun n es -> n + List.length es) 0 m.delta;
      node_max = Array.length m.states - 1;
      edge_max = 0;
    }
  in m'

(* pretty-printing *)

let pp pp_state pp_label (fmt : Format.formatter) (m : ('state, 'label) nfa) =
  let m' : ('state, 'label) fixed_nfa = nfa2fnfa m
  in pp_fixed_nfa pp_state pp_label fmt m'

let show pp_state pp_label (m : ('state, 'label) nfa) = 
   let m' : ('state, 'label) fixed_nfa = nfa2fnfa m
   in show_fixed_nfa pp_state pp_label m'

(* json *)

let fsa_of_yojson
    (state_of_yojson : Yojson.Safe.json -> ('state, string) Result.result)
    (alpha_of_yojson : Yojson.Safe.json -> ('label, string) Result.result)
    (json : Yojson.Safe.json) =
  match fixed_nfa_of_yojson state_of_yojson alpha_of_yojson json with
  | Result.Ok fnfa -> Result.Ok (fnfa2nfa fnfa)
  | Result.Error msg -> Result.Error msg

let fsa_to_yojson state_to_yojson alpha_to_yojson (m : ('state, 'label) nfa) =
  m |> nfa2fnfa |> fixed_nfa_to_yojson state_to_yojson alpha_to_yojson

(* graphviz *)

let print_in_dot oc (string_of_state : 'state -> string) (string_of_label : 'label -> string) (m : ('state, 'label) t) =
  output_string oc "digraph g {\n";

  (* states *)
  Hashtbl.iter
    (fun i v ->
      Printf.fprintf oc "  %d [" i;
      if i = initial_get m then
	output_string oc "style=filled,fillcolor=lightgrey,";
      if List.mem i (final_get m) then
	output_string oc "shape=doublecircle,";
      Printf.fprintf oc "];\n")
    m.states;

  (* transitions *)
  Hashtbl.iter
    (fun i edges ->
      List.iter
	(fun (l_opt, j) ->
	  Printf.fprintf oc "  %d -> %d" i j;
	  let _ =
	    match l_opt with
	    | None -> ()
	    | Some l ->
		Printf.fprintf  oc " [label=%S]" (string_of_label (sigma_get m l));
	  in
	  Printf.fprintf oc ";\n")
	!edges)
    m.delta;
    
  output_string oc "}\n";
  ()
