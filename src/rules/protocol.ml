(* $Id: protocol.ml,v 1.1 2018/11/23 18:05:46 sato Exp sato $ *)
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
  | Proto_event of string
  | Proto_seq of protocol list
  | Proto_sum of protocol list
  | Proto_star of protocol

[@@deriving show, yojson, eq]

type t = protocol

(** pretty-printing *)

let rec print_protocol out ?(fancy=false) (p : protocol) =
  print_protocol_rec out ~fancy p

and print_protocol_rec out ?(fancy=false) (p : protocol) =
  match p with
  | Proto_event e -> out e

  (* seq *)
  | Proto_seq [] -> out "1"
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
  | Proto_sum [] -> out "0"
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

  (* star *)
  | Proto_star (Proto_seq []) ->
      out "1*"
  | Proto_star p' when proto_prec p' <= proto_prec p ->
      print_protocol out ~fancy p'; out "*"
  | Proto_star p' ->
      out "("; print_protocol out ~fancy p'; out ")*"

  | _ ->
      failwith "[print_protocol_rec]"

(* precedence: grouping (()) < *, ? < concat (;) < choice (+) *)
and proto_prec = function
  | Proto_event _ -> 0
  | Proto_seq _   -> 100
  | Proto_sum _   -> 200
  | Proto_star _  -> 30

let to_string (prn : (string -> unit) -> 'a -> unit) (x : 'a) =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  prn concat x;
  !str

let string_of_protocol =
  to_string (print_protocol ~fancy:false)

(** epsilon elimination *)

type nfa = (unit, string) Fsa.t

let rec mem_event name = function
  | Proto_event e -> e = name

  | Proto_seq [] -> name = "1" || name = "_epsilon"
  | Proto_sum [] -> name = "0" || name = "_empty"

  | Proto_seq ps | Proto_sum ps ->
      (match List.find_opt (mem_event name) ps with None -> false | Some _ -> true)
  | Proto_star p -> mem_event name p

(* protocol -> nfa *)
let rec protocol2nfa p =
  let m : nfa = Fsa.make () in
  let ini, fin = protocol2nfa_rec m p in
  Fsa.initial_set m ini;
  Fsa.final_set m [fin];
  m

(* m p -> (ini, fin) *)
and protocol2nfa_rec m = function
  | Proto_event e when List.mem e ["0"; "1"; "_empty"; "_epsilon"; "_any"] ->
      invalid_arg ("[protocol2nfa_rec] " ^ e)

  | Proto_event e ->
      let ini = Fsa.state_add m ()
      and fin = Fsa.state_add m ()
      and l = Fsa.sigma_add m e in
      Fsa.transition_add m ini (Some l, fin);
      ini, fin

  | Proto_seq [] ->
      (* case: epsilon *)
      let ini = Fsa.state_add m ()
      and fin = Fsa.state_add m () in
      Fsa.transition_add m ini (None, fin);
      ini, fin

  | Proto_seq [p] ->
      protocol2nfa_rec m p
  | Proto_seq (p :: ps) ->
      let ini1, fin1 = protocol2nfa_rec m p
      and ini2, fin2 = protocol2nfa_rec m (Proto_seq ps) in
      Fsa.transition_add m fin1 (None, ini2);
      ini1, fin2

  | Proto_sum [] ->
      (* case: empty *)
      invalid_arg "[protocol2nfa_rec] empty"

  | Proto_sum [p] ->
      protocol2nfa_rec m p
  | Proto_sum (p :: ps) ->
      let ini0, fin0 = (Fsa.state_add m ()), Fsa.state_add m ()
      and ini1, fin1 = protocol2nfa_rec m p
      and ini2, fin2 = protocol2nfa_rec m (Proto_sum ps) in
      Fsa.transition_add m ini0 (None, ini1);
      Fsa.transition_add m fin1 (None, fin0);
      Fsa.transition_add m ini0 (None, ini2);
      Fsa.transition_add m fin2 (None, fin0);
      ini0, fin0
 
  | Proto_star p ->
      let ini0, fin0 = (Fsa.state_add m ()), Fsa.state_add m ()
      and ini1, fin1 = protocol2nfa_rec m p in
      Fsa.transition_add m ini0 (None, ini1);
      Fsa.transition_add m fin1 (None, fin0);
      Fsa.transition_add m ini0 (None, fin0);
      Fsa.transition_add m fin1 (None, ini1);
      ini0, fin0

  | _ -> failwith "protocol2nfa_rec"

(* simp *)
let rec simp p =
  (*p |> simp_empty |> flatten |> elim_dup*)
  p |> flatten |> elim_dup

and flatten = function
  | Proto_event e -> Proto_event e

  | Proto_seq ps ->
      let ps' =
	List.fold_left
	  (fun rslt p' -> match p' with Proto_seq qs when qs <> [] -> rslt @ qs | _ -> rslt @ [p'])
	  [] (List.map flatten ps) in
      if List.length ps' = 1 then List.hd ps' else Proto_seq ps'
  | Proto_sum ps ->
      let ps' =
	List.fold_left
	  (fun rslt p' -> match p' with Proto_sum qs -> rslt @ qs | _ -> rslt @ [p'])
	  [] (List.map flatten ps) in
      if List.length ps' = 1 then List.hd ps' else Proto_sum ps'

  | Proto_star p -> Proto_star (flatten p)
  | _ -> failwith "[Protocol.flatten]"

and elim_dup e =
  match e with
  | Proto_event _ -> e

  | Proto_seq [] -> e
  | Proto_seq ps ->
      let ps' =	List.map elim_dup ps in
      if List.length ps' = 1 then List.hd ps' else Proto_seq ps'

  | Proto_sum [] -> e
  | Proto_sum ps ->
      let ps' =
	List.fold_left
	  (fun rslt p' -> rslt @ if List.mem p' rslt then [] else [p'])
	  [] (List.map elim_dup ps) in
      if List.length ps' = 1 then List.hd ps' else Proto_sum ps'

  | Proto_star p -> Proto_star (elim_dup p)
  | _ -> failwith "[Protocol.elim_dup]"

(* remove Proto_sum [] (= 0) in choices *)
and simp_empty e =
  match e with
  | Proto_event e -> Proto_event e

  | Proto_seq [] -> e
  | Proto_seq ps ->
      let ps' =
	List.fold_left
	  (fun rslt p' -> match p' with Proto_sum [] -> rslt | _ -> rslt @ [p'])
	  [] (List.map simp_empty ps) in
      (match ps' with [] -> Proto_sum [] | [p'] -> p' | _ -> Proto_seq ps')

  | Proto_sum [] -> e
  | Proto_sum ps ->
      let ps' =
	List.fold_left
	  (fun rslt p' -> match p' with Proto_sum [] -> rslt | _ -> rslt @ [p'])
	  [] (List.map simp_empty ps) in
      (match ps' with [] -> Proto_sum [] | [p'] -> p' | _ -> Proto_sum ps')

  | Proto_star p ->
      let p' = simp_empty p in if p' = Proto_sum [] then Proto_sum [] else Proto_star p'

  | _ -> failwith "[Protocol.simp_empty]"

(* dfa -> protocol *)
let rec dfa2protocol (m : ('state, string) Fsa.t) =
  let q :: rest = Fsa.states_fold (fun q _ rslt -> rslt @ [q]) m [] in
  let qmin, qmax =
    List.fold_left (fun (qmin, qmax) q -> min qmin q, max qmax q) (q, q) rest in
  assert (qmin = 0 && qmax = List.length rest);
  let ini = Fsa.initial_get m in
  let final : int list = Fsa.final_get m in

  let ps : t list =
    List.map (fun fin -> dfa2protocol_rec m ini fin true qmax) final in
  match ps with
  | [] -> failwith "[dfa2protocol] no protocol"
  | [p] -> p
  | _ -> Proto_sum ps

(** [dfa2protocol_rec m i j k] returns a protocol
    that correspond with transition paths, from state [i] to [j],
    each of which does not include any intermediate state larger than [k].

    [note] we assume that each final state of [m] is a sink
    (i.e., state w/o any outbound edge),
    which allows to rule out epsilon events.
 *)
and dfa2protocol_rec (m : ('state, string) Fsa.t) i j allow_trailing_epsilon k =
  (*Printf.eprintf ";; dfa2protocol (%d, %d, %d)\n" i j k;*)
  if k < 0 then
    (* base case: no intermeidate state between state i and j *)
    let ps : t list =
      (* ps = [e1; e2; ..] where qi -(e)-> qj for any e in ps *)
      List.fold_left
	(fun rslt -> function
	  | None, _ -> failwith "[dfa2protocol_rec] epsilon detected"
	  | Some l, j' when j' = j ->
	      (* direct transition from state i to j *)
	      let e : string = Fsa.sigma_get m l in
	      let p = Proto_event e in
	      assert (not @@ List.mem p rslt);
	      rslt @ [p]
	  | _, j' -> rslt)
	[] (Fsa.delta_get m i) in

    let p' : t =
      if i = j then
	(* special case: include epsilon *)
	(*if ps = [] then Proto_seq [] else Proto_sum (ps @ [Proto_seq []])*)
	  (** this is the source of epsilons *)
	if ps = [] then Proto_seq [] else Proto_sum ps
	  (** heuristics *)
      else
	if ps = [] then
	  (*(Printf.eprintf "** not_found: %d %d %d\n" i j k; raise Not_found)*)
	  raise Not_found
	else
	  Proto_sum ps
    in
    (*
    Printf.eprintf ";; dfa2protocol_rec (%d, %d, %d): " i j k;
    print_protocol (fun str -> output_string stderr str) p';
    Printf.eprintf "\n";
     *)
    p'

  else (* k >= 0 *)
    let p1_opt =
      (* i to j via k *)
      try
	let p11 = dfa2protocol_rec m i k false (k - 1)
	and p12 = dfa2protocol_rec m k k false (k - 1)
	and p13 = dfa2protocol_rec m k j allow_trailing_epsilon (k - 1)
	in
	(* p11; p12*; p13 *)
	(*Some (Proto_seq [p11; Proto_star p12; p13])*)
	let p1 =
	  match p11, p12, p13 with
	  | Proto_seq [], Proto_seq [], Proto_seq [] -> Proto_seq []
	  | Proto_seq [], Proto_seq [], _ -> p13
	  | Proto_seq [], _, Proto_seq [] -> Proto_star p12
	  | Proto_seq [], _, _ -> p11
	  | _, Proto_seq [], Proto_seq [] -> p11
	  | _, Proto_seq [], _ -> flatten (Proto_seq [p11; p13])
	  | _, _, Proto_seq [] -> flatten (Proto_seq [p11; Proto_star p12])
	  | _, _, _ -> flatten (Proto_seq [p11; Proto_star p12; p13])
	in assert (not @@ mem_event "_epsilon" p1); Some p1
      with Not_found -> None
    and p2_opt =
      (* i to j via k *)
      try
	Some (dfa2protocol_rec m i j allow_trailing_epsilon (k - 1))
      with Not_found ->	None
    in	
    let p' =
      match p1_opt, p2_opt with
      | None, None -> raise Not_found
      | None, Some p2 -> p2	(* could be epsilon *)
      | Some p1, None -> p1
      | Some p1, Some (Proto_seq []) -> p1
      | Some p1, Some p2 -> Proto_sum [p1; p2] (* (p11; p12*; p13) + p2*)
    in
    (*
    Printf.eprintf ";; dfa2protocol_rec (%d, %d, %d): " i j k;
    print_protocol (fun str -> output_string stderr str) p';
    Printf.eprintf "\n";
     *)
    p'

(* standard text-book version
   this standard translation often introduces lots of epsilon transitions.
 *)
(*
and dfa2protocol_rec (m : ('state, string) Fsa.t) i j allow_trailing_epsilon k =
  (*Printf.eprintf ";; dfa2protocol (%d, %d, %d)\n" i j k;*)
  if k < 0 then
    (* base case: no intermeidate state between state i and j *)
    let ps : t list =
      (* ps = [e1; e2; ..] where qi -(e)-> qj for any e in ps *)
      List.fold_left
	(fun rslt -> function
	  | None, _ -> failwith "[dfa2protocol_rec] epsilon detected"
	  | Some l, j' when j' = j ->
	      (* direct transition from state i to j *)
	      let e : string = Fsa.sigma_get m l in
	      let p = Proto_event e in
	      assert (not @@ List.mem p rslt);
	      rslt @ [p]
	  | _, j' -> rslt)
	[] (Fsa.delta_get m i) in

    let p' : t =
      if i = j then
	(* special case: include epsilon *)
	if ps = [] then Proto_seq [] else Proto_sum (ps @ [Proto_seq []])
	  (** this is the source of epsilons *)
      else
	if ps = [] then
	  (*(Printf.eprintf "** not_found: %d %d %d\n" i j k; raise Not_found)*)
	  raise Not_found
	else
	  Proto_sum ps
    in
    (*
    Printf.eprintf ";; dfa2protocol_rec (%d, %d, %d): " i j k;
    print_protocol (fun str -> output_string stderr str) p';
    Printf.eprintf "\n";
     *)
    p'

  else (* k >= 0 *)
    let p1_opt =
      (* i to j via k *)
      try
	let p11 = dfa2protocol_rec m i k false (k - 1)
	and p12 = dfa2protocol_rec m k k false (k - 1)
	and p13 = dfa2protocol_rec m k j allow_trailing_epsilon (k - 1)
	in
	(* p11; p12*; p13 *)
	Some (Proto_seq [p11; Proto_star p12; p13])
      with Not_found ->	None
    and p2_opt =
      (* i to j via k *)
      try
	Some (dfa2protocol_rec m i j allow_trailing_epsilon (k - 1))
      with Not_found ->	None
    in	
    let p' =
      match p1_opt, p2_opt with
      | None, None -> raise Not_found
      | Some p1, None -> p1
      | None, Some p2 -> p2
      | Some p1, Some p2 -> Proto_sum [p1; p2] (* (p11; p12*; p13) + p2*)
    in
    (*
    Printf.eprintf ";; dfa2protocol_rec (%d, %d, %d): " i j k;
    print_protocol (fun str -> output_string stderr str) p';
    Printf.eprintf "\n";
     *)
    p'
*)

(* check if there exists a terminating epsilon event *)
let rec epsilon_terminable_p p =
  p |> simp |> epsilon_terminable_rec
  (*
  if mem_event "_epsilon" p
  then p |> simp |> epsilon_terminable_rec
  else false
   *)

and epsilon_terminable_rec = function
  | Proto_event e when List.mem e ["1"; "_epsilon"] -> true
  | Proto_event _ -> false

  | Proto_seq [] -> true

  | Proto_seq ps when ps <> [] ->
      epsilon_terminable_rec (List.nth ps (List.length ps - 1))
  | Proto_sum ps when ps <> [] ->
      (match List.find_opt epsilon_terminable_rec ps with None -> false | _ -> true)
  | Proto_star p ->
      true
  | _ -> failwith "[Protocol.epsilon_terminable_rec]"

let rec subst_event e1 e2 p =
  match p with
  | Proto_event e when e = e1 ->
      if List.mem e2 ["0"; "_empty"] then
	Proto_sum []
      else if List.mem e2 ["1"; "_epsilon"] then
	Proto_seq []
      else Proto_event e2
  | Proto_event _ -> p
  | Proto_seq ps -> Proto_seq (List.map (subst_event e1 e2) ps)
  | Proto_sum ps -> Proto_sum (List.map (subst_event e1 e2) ps)
  | Proto_star p' -> Proto_star (subst_event e1 e2 p')

let minimize p =
  (*Printf.eprintf "[minimize] %s\n" (show_protocol p);*)
  if not @@ mem_event "_end" p then failwith "[minimize] no \"_end\" event included";
  (* "_end" must have been added in Rulespp.pp_add_trailer *)

  let p = p |> flatten in
  (*
  output_string stderr "[minimize] ";
  print_protocol (output_string stderr) p;
  output_string stderr "\n";
   *)

  (* protocol -> nfa *)
  let m1 : nfa = protocol2nfa p in
  (*
  let oc = open_out "fsa1.dot" in
  Fsa.print_in_dot oc (fun _ -> "") (fun l -> l) m1;
  close_out oc;
   *)

  (* nfa -> nfa w/o epsilon *)
  let m2 = Fsa.eliminate_epsilon m1 in
  (*
  let oc = open_out "fsa2.dot" in
  Fsa.print_in_dot oc (fun _ -> "") (fun l -> l) m2;
  close_out oc;
   *)  

  (* nfa w/o epsilon -> dfa *)
  let m3 = Fsa.determinize m2 in
  (*
  let oc = open_out "fsa3.dot" in
  Fsa.print_in_dot oc (fun _ -> "") (fun l -> l) m3;
  close_out oc;
   *)

  (* dfa -> dfa *)
  let m4 = Fsa.minimize m3 in
  (*
  let oc = open_out "fsa4.dot" in
  Fsa.print_in_dot oc (fun _ -> "") (fun l -> l) m4;
  close_out oc;
   *)

  let p' = dfa2protocol m4 |> simp in
  (*
  Printf.fprintf stderr "%s\n" (show_protocol p');
  output_string stderr "\n";
   *)
  p'
