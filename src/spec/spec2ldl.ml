(* $Id: spec2ldl.ml,v 1.4 2018/01/09 07:54:44 sato Exp $ *)
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

open Rules
open Protocol
open Rule
open Ldl
open Ldlsimp
open Printf

(* ================================================================================
   translation of protocol
   translate : protocol -> Ldl.formula
   translate p = <_idle; (protocol_to_path p)> (last & _idle)
   ================================================================================
 *)

type event_map = (string * Ldl.formula) list

(* Rule.protocol -> Ldl.formula *)
let rec translate_protocol nbit (es : string list) (p : Protocol.protocol) =
  let _idle : formula = protocol_prop_to_formula nbit es (PProp_event "_skip") in
  let r = protocol_to_path nbit es p
  in
  Ldl_modal (Mod_ex, Path_seq [Path_prop _idle; r], Ldl_conj [Ldl_atomic "last"; _idle])

(*
(* Rule.protocol -> Rule.protocol *)
and relax_protocol (p : Rule.protocol) =
  relax_protocol_rec p |> flatten_protocol |> elim_dup

and relax_protocol_rec (p : Rule.protocol) =
  let filler = Proto_star (Proto_prop (PProp_event "none")) in
  match p with
  | Proto_prop _ -> Proto_seq [filler; p; filler]
  | Proto_seq ps -> Proto_seq (List.map relax_protocol_rec ps)
  | Proto_sum ps -> Proto_sum (List.map relax_protocol_rec ps)
  | Proto_test p -> Proto_test (relax_protocol_rec p)
  | Proto_star p -> Proto_star (relax_protocol_rec p)
  | _ -> failwith "relax_protocol_rec"

and flatten_protocol p =
  match p with
  | Proto_seq ps ->
      let ps' =
	List.fold_left
	  (fun rslt p1 -> match p1 with Proto_seq ps1 -> rslt @ ps1 | _ -> rslt @ [p1])
	  [] (List.map flatten_protocol ps) in
      if List.length ps' = 1 then List.hd ps' else Proto_seq ps'
  | _ -> p

and elim_dup p =
  match p with
  | Proto_seq ps -> Proto_seq (elim_dup1 ps)
  | _ -> p

and elim_dup1 ps =
  match ps with
  | [] | [_] -> ps
  | Proto_star (Proto_prop (PProp_event "none")) :: Proto_star (Proto_prop (PProp_event "none")) :: rest ->
      elim_dup1 (Proto_star (Proto_prop (PProp_event "none")) :: rest)
  | Proto_star (Proto_prop (PProp_event "none")) :: p2 :: rest ->
      Proto_star (Proto_prop (PProp_event "none")) :: p2 :: elim_dup1 rest
  | p1 :: Proto_star (Proto_prop (PProp_event "none")) :: rest ->
      p1 :: elim_dup1 (Proto_star (Proto_prop (PProp_event "none")) :: rest)
  | p1 :: p2 :: rest ->
      p1 :: p2 :: elim_dup1 rest
*)

(* genmap : int -> string list -> (evmap : (string * Ldl.formula) list) *)
and genmap nbit (es : string list) =
  ("_skip", event_to_formula_aux nbit es "_skip") ::
  List.map (fun e -> e, event_to_formula_aux nbit es e) es

(* Rule.protocol -> Ldl.path *)
and protocol_to_path nbit (es : string list) (p : Protocol.protocol) =
  match p with
  | Proto_prop f -> Ldl.Path_prop (protocol_prop_to_formula nbit es f)
  | Proto_seq ps -> Ldl.Path_seq (List.map (protocol_to_path nbit es) ps)
  | Proto_sum ps -> Ldl.Path_sum (List.map (protocol_to_path nbit es) ps)
  | Proto_test p' ->
      let _idle = event_to_formula_rec nbit 0 [] 0 in
      Ldl.Path_test (Ldl.Ldl_modal (Mod_ex, (protocol_to_path nbit es p'), _idle))
  | Proto_star p' -> Ldl.Path_star (protocol_to_path nbit es p')
  | _ -> failwith "protocol_to_path"

(* Rule.protocol_prop -> Ldl.formula (proposition) *)
and protocol_prop_to_formula nbit (es : string list) f =
  match f with
  | PProp_event e -> event_to_formula_aux nbit es e
(*| PProp_neg f' ->Ldl_neg (protocol_prop_to_formula nbit es f')*)
  | _ -> failwith "protocol_prop_to_formula"

(* event list (disjunctive) -> formula *)
and event_to_formula (es : string list) (e : string) =
  let nbit = int_of_float (ceil (log (float_of_int (List.length es + 1)) /. log 2.0))
  in event_to_formula_aux nbit es e

and event_to_formula_aux nbit (es : string list) (e : string) =
  match e with
  | "_skip" -> event_to_formula_rec nbit 0 [] 0
  | e when List.mem e es -> event_to_formula_rec nbit (index e es + 1) [] 0
  | e -> failwith ("event_to_formula: undefined event (" ^ e ^ ") encountered")

and index elt seq =
  let i = ref 0 in
  try  List.iteri (fun j elt' -> i := j; if elt' = elt then raise Exit) seq; -1
  with Exit -> !i

and event_to_formula_rec nbit (bits : int) fs (i : int) =
  if i = nbit then
    Ldl_conj fs
  else
    let f = Ldl_atomic ("_b" ^ string_of_int i) in
    let f = if bits land (1 lsl i) <> 0 then f else Ldl_neg f in
    event_to_formula_rec nbit (bits : int) (fs @ [f]) (i + 1)

(* ================================================================================
   translation of property
   ================================================================================
 *)

let rec translate_property nbit es (lp : Rule.labelled_property) =
  lp |> lprop_to_formula nbit es

(* labelled_property -> formula *)
and lprop_to_formula nbit (es : string list) (p, p_opt) =
  prop_to_formula nbit es p

and prop_to_formula nbit es (p : Rule.property) =
  match p with
  | Prop_atomic "_idle" -> event_to_formula_aux nbit es "_skip"
  | Prop_atomic a -> Ldl_atomic a
  | Prop_neg p'  -> Ldl_neg (prop_to_formula nbit es p')
  | Prop_conj ps -> Ldl_conj (List.map (prop_to_formula nbit es) ps)
  | Prop_disj ps -> Ldl_disj (List.map (prop_to_formula nbit es) ps)
  | Prop_modal (m, lr, lp) ->
      let m' = match m with Mod_ex -> Ldl.Mod_ex | Mod_all -> Ldl.Mod_all in
      Ldl_modal (m', lpath_to_path nbit es lr, lprop_to_formula nbit es lp)
  | Prop_label l ->
      failwith ("prop_to_formula:label(" ^ l ^ ")")

and lpath_to_path nbit es (r, r_opt) =
  match (r : Rule.path) with
  | Path_prop p -> Ldl.Path_prop (prop_to_formula nbit es p)
  | Path_seq rs -> Ldl.Path_seq (List.map (lpath_to_path nbit es) rs)
  | Path_sum rs -> Ldl.Path_sum (List.map (lpath_to_path nbit es) rs)
  | Path_test p -> Ldl.Path_test (prop_to_formula nbit es p)
  | Path_star r -> Ldl.Path_star (lpath_to_path nbit es r)
  | Path_label l ->
      failwith ("lpath_to_path:label(" ^ l ^ ")")

(* ================================================================================
   translation of rule
   ⟦ on e when c do a ⟧ = [true*]>(<true>φ(e) -> <c>a)
                          where φ(ev) = bit-encoded proposition of e
   ================================================================================
 *)

let rec translate_rule nbit es (s : Spec.t) (r : Rule.rule) =
  let (e, _), (c, _), (a, _) = r.event, r.condition, r.action in
  let c' : formula = lprop_to_formula nbit es c in
  match c' with
  | _ when propositional c' ->
      translate_rule1 nbit es (s : Spec.t) (e, c', a)
  | Ldl_modal (Mod_ex, r, f) when propositional f ->
      translate_rule2 nbit es (s : Spec.t) (e, c', a)
  |_ ->
      failwith ("translate_rule: invalid condition " ^ string_of_formula c')

and translate_rule1 nbit es (s : Spec.t) (e, c, a) =
  assert (propositional c);
  let c' : path = Path_prop c
  and e' : formula =
    event_to_formula s.event_seq (match e with Ev_name e' -> e' | _ -> failwith "translate_rule1")
  and a' : formula = action_to_formula nbit s.event_seq a
  in
  (* [true*](<c>e -> <c>(e ^ a)) *)
  Ldl_modal (Mod_all, Path_star (Path_prop (Ldl_atomic "true")),
	     Ldl_impl (Ldl_modal (Mod_ex, c', e'), Ldl_modal (Mod_ex, c', Ldl_conj [e'; a'])))

and translate_rule2 nbit es (s : Spec.t) (e, c, a) =
  let c' : path =
    match c with
    | Ldl_modal (Mod_ex, r, f) when propositional f -> Path_seq [r; Path_prop f]
    | _ -> failwith "translate_rule2"
  and e' : formula = event_to_formula s.event_seq (event_name e)
  and a' : formula = action_to_formula nbit s.event_seq a
  in
  (* [true*](<c>e -> <c>(e ^ a)) *)
  Ldl_modal (Mod_all, Path_star (Path_prop (Ldl_atomic "true")),
	     Ldl_impl (Ldl_modal (Mod_ex, c', e'), Ldl_modal (Mod_ex, c', Ldl_conj [e'; a'])))

and action_to_formula nbit es (a : action) =
  match a with
  | None, acts -> Ldl_conj (List.map (action_unit_to_formula nbit es) acts)
  | _ -> failwith "action_to_formula"

and action_unit_to_formula nbit es (a : action_unit) =
  match a with
  | Act_ensure p -> prop_to_formula nbit es p
  | Act_raise es' ->
      assert (es' <> []);
      (* <true> \/es' *)
      Ldl_modal (Mod_ex, Path_prop (Ldl_atomic "true"),
		 Ldl_disj (List.map (event_to_formula es) es'))

(* ================================================================================
   translation of rule set
   ================================================================================
 *)

(* translate : Spec.t -> Ldl.formula list *)
let rec translate (s : Spec.t) =
  (* protocols -> formulas *)
  let es : string list = s.event_seq in
  assert (not @@ List.mem "_skip" es);
  let nev = List.length es + 1 (* es + 1 extra event for "_skip" *) in
  let nbit = int_of_float @@ ceil @@ log (float_of_int nev) /. log 2.0 in
  let map : event_map = genmap nbit es in

(*
  output_string stderr "** events: (_skip)";
  List.iter (fun e -> output_string stderr (" " ^ e)) es;
  Printf.fprintf stderr "\n** %d (+1) events -> %d bits\n" (List.length es) nbit;
  output_string stderr "** map:";
  List.iter (fun (e, f) -> output_string stderr (" " ^ e ^ "=" ^ Ldl.show_formula f)) map;
 *)
  let fs1 : formula list = List.map (translate_protocol nbit s.event_seq) s.proto_seq in
  let fs1 = fs1 @
    (* extra formula to exclude non-existent events *)
    let max = 1 lsl nbit in
    if nev = max then [] else
    let rec ev_formulas i rslt =
      if i = max
      then rslt
      else ev_formulas (i + 1) (rslt @ [event_to_formula_rec nbit i [] 0])
    in [Ldl_modal (Mod_all,
		   Path_star (Path_prop (Ldl_atomic "true")),
		   Ldl_neg (Ldl_disj (ev_formulas nev [])))]
  in

  (* properties -> formulas *)
  let fs2 = List.map (translate_property nbit es) s.prop_seq in

  (* rules -> formulas *)
  let fs3 = List.map (translate_rule nbit es s) s.rule_seq in

  (fs1 @ fs2 @ fs3), map

(*

(** labelled property term *)
type prop_term_labelled =
    prop_term * string

and prop_term =
  | PTerm_atomic of string
  | PTerm_neg of string
  | PTerm_conj of prop_term_labelled list
  | PTerm_disj of prop_term_labelled list
  | PTerm_modal of modality * path * prop_term_labelled

[@@deriving show]

type env_t =
    (string * Ldl.formula) list
      (** state name -> formula *)

[@@deriving show]

(* for debugging *)
let debug_flags = ref 0b00000
    (** flags
	0b00001:	display high-level summary report (-v)
	0b00010:	display detailed summary
	0b00100:	print intermediate values
	0b01000:	unused
	0b10000:	generate intermediate files
     *)
let debug_vacuum = open_out "/dev/null"

let debug_set_flags (flags : int) =
  debug_flags := flags land 0b11111

let debug_is_on (flags : int list) =
  List.fold_left (fun b f -> b && (f land !debug_flags <> 0)) true flags

let debug_printf (flags : int list) (fmt : ('a, out_channel, 'b) format) =
  let oc = if debug_is_on flags then stderr else debug_vacuum in
  fprintf oc fmt

(* --------------------------------------------------------------------------------
   helpers
   --------------------------------------------------------------------------------
 *)

(* prop_decls -> env_t = (state_name * forumla) list *)
let rec make_env (xs, qs, ps, rs) =
  let env0 = List.rev_map (fun x -> x, Ldl_atomic "true") qs in
  List.fold_left
    (fun env (p : property) ->
      try ("initial", translate_pprop (xs, qs, ps, rs) env p) :: env with Exit -> env)
    env0

and env_lookup env x =
  try List.assoc x env
  with Not_found ->
    eprintf "** lookup: %S not found in [" x;
    List.iter (fun (x, _) -> eprintf "%S; " x) env;
    eprintf "]\n";
    failwith "name resolution"

(* propositional property -> formula *)
and translate_pprop (xs, qs, ps, rs) env (p : property) =
  match p with
  | Prop_atomic x when List.mem x xs -> Ldl_atomic x
  | Prop_name x when List.mem x xs -> translate_pprop (xs, qs, ps, rs) env (Prop_atomic x)
  | Prop_neg p' -> Ldl_neg (translate_pprop (xs, qs, ps, rs) env p')
  | Prop_conj ps' -> Ldl_conj (List.map (translate_pprop (xs, qs, ps, rs) env) ps')
  | Prop_disj ps' -> Ldl_disj (List.map (translate_pprop (xs, qs, ps, rs) env) ps')
  | Prop_modal _ -> raise Exit
  | Prop_name q when List.mem q qs -> env_lookup env q
  | _ -> failwith "translate_pprop"

(* nnf *)
let rec nnf names p =
  match p with
  | Prop_atomic a -> p
  | Prop_name a when List.mem a names -> Prop_atomic a

  | Prop_neg (Prop_atomic a) -> p
  | Prop_neg (Prop_name a) when List.mem a names -> Prop_neg (Prop_atomic a)
  | Prop_neg (Prop_neg p') -> nnf names p'
  | Prop_neg (Prop_conj ps) -> nnf names (Prop_disj (List.map (fun p' -> Prop_neg p') ps))
  | Prop_neg (Prop_disj ps) -> nnf names (Prop_conj (List.map (fun p' -> Prop_neg p') ps))
  | Prop_neg (Prop_modal (Mod_ex, r, p')) -> nnf names (Prop_modal (Mod_all, r, Prop_neg p'))
  | Prop_neg (Prop_modal (Mod_all, r, p')) -> nnf names (Prop_modal (Mod_ex, r, Prop_neg p'))

  | Prop_conj ps -> Prop_conj (List.map (nnf names) ps)
  | Prop_disj ps -> Prop_disj (List.map (nnf names) ps)

  | Prop_modal (m, r, p') -> Prop_modal (m, r, nnf names p')

  | _ -> failwith ("nnf: " ^ Spec.show_property p)

(* --------------------------------------------------------------------------------
   property_spec list -> property term list
   --------------------------------------------------------------------------------
 *)

(* translate1: property list -> prop_term_labelled list *)
let rec translate1 (xs, qs, ps, rs) (ps' : property list) =
  List.fold_left
    (fun (rslt : prop_term_labelled list) (p : property) ->
      rslt @ [translate1_rec (xs, qs, ps, rs) "initial" p])
    [] ps'

(* translate1_rec: state -> property -> prop_term *)
and translate1_rec (xs, qs, ps, rs) q (p : property) =
  let tm =
    match p with
    | Prop_atomic x -> PTerm_atomic x
    | Prop_name x when List.mem x xs -> PTerm_atomic x
    | Prop_neg (Prop_atomic x)  -> PTerm_neg x
    | Prop_neg p' -> fst (translate1_rec (xs, qs, ps, rs) q (nnf xs p))

    | Prop_conj ps' -> PTerm_conj (List.map (translate1_rec (xs, qs, ps, rs) q) ps')
    | Prop_disj ps' -> PTerm_disj (List.map (translate1_rec (xs, qs, ps, rs) q) ps')

    | Prop_modal (m, r, Prop_atomic x)
      when (List.mem x xs) ->
	debug_printf [0b0100]  "translate1_rec (%s (modal) atomic:%s): %s\n" q x (show_property p);
	PTerm_modal (m, r, ((PTerm_atomic x), "unknown"))
   (*
    | Prop_modal (m, r, Prop_name q')
      when (List.mem q' qs) ->
	debug_printf [0b0100] "translate1_rec (%s (modal) state:%s) %s\n" q q' (show_property p);
	assert (q != q');
	let matched : property list =
	  (* props at q' *)
	  List.fold_left
	    (fun rslt -> function _, q1, p' when q1 = q' -> rslt @ [p'] | _ -> rslt)
	    [] ps in
	let children = List.map (translate1_rec (xs, qs, ps, rs) q') matched in
	assert (children != []);
	PTerm_modal (m, r, ((PTerm_conj children), q'))
    *)
    | Prop_modal (m, r, p') ->
	PTerm_modal (m, r, translate1_rec (xs, qs, ps, rs) "unknown" p')

    | _ -> failwith ("translate1_rec: " ^ Spec.show_property p)
  in
  debug_printf [0b0100] "property_term: @ %s: %s\n" q (show_prop_term tm);
  (tm, q)

(* --------------------------------------------------------------------------------
   property term list -> formula list
   --------------------------------------------------------------------------------
 *)

(* prop_term list -> formula list *)
let rec translate2 (xs, qs, ps, rs) env (tms : prop_term_labelled list)=
  List.map
    (fun ltm -> ((translate2_rec (xs, qs, ps, rs) env ltm) : formula))
    tms
  
(* prop_term -> Ldl.formula *)
and translate2_rec (xs, qs, ps, rs) env (tm, q) =
  let m2m = function Spec.Mod_all -> Ldl.Mod_all | Spec.Mod_ex -> Ldl.Mod_ex in
  match tm with
  | PTerm_atomic x when List.mem x xs -> Ldl_atomic x
  | PTerm_neg x when List.mem x xs -> Ldl_neg (Ldl_atomic x)

  | PTerm_conj ts -> Ldl_conj (translate2 (xs, qs, ps, rs) env ts)
  | PTerm_disj ts -> Ldl_disj (translate2 (xs, qs, ps, rs) env ts)
  | PTerm_modal (m, r, ltm) ->
      Ldl_modal (m2m m,
		 translate_path (xs, qs, ps, rs) env r,
		 translate2_rec (xs, qs, ps, rs) env ltm)
  | _ -> failwith ("translate2_rec: " ^ show_prop_term tm)

(* Spec.path -> Ldl.path *)
and translate_path (xs, qs, ps, rs) env (r : Spec.path) =
  match r with
  | Path_prop p -> Ldl.Path_prop (translate_prop (xs, qs, ps, rs) env "unbound" p)

  | Path_seq rs' -> Ldl.Path_seq (List.map (translate_path (xs, qs, ps, rs) env)  rs')
  | Path_sum rs' -> Ldl.Path_sum (List.map (translate_path (xs, qs, ps, rs) env)  rs')
  | Path_star r' -> Ldl.Path_star (translate_path (xs, qs, ps, rs) env r')
  | Path_test p ->
      let q = "initial" in
      let tm, _ = translate1_rec (xs, qs, ps, rs) q p in
      let f : Ldl.formula = translate2_rec  (xs, qs, ps, rs) env (tm, q) in
      Ldl.Path_test f

  | Path_name q when List.mem_assoc q env -> Ldl.Path_prop (env_lookup env q)
	(* state *)
  (*| Path_name x when List.mem x xs -> Ldl.Path_prop (Ldl_atomic x)*)
  | Path_name q -> failwith ("translate_path: unknown name (" ^ q ^ ")")

  | Path_let ([], r') -> translate_path (xs, qs, ps, rs) env r'
  | Path_let ([q, (p : property)], r') ->
      let f : Ldl.formula = translate_pprop (xs, qs, ps, rs) env p
      in translate_path (xs, qs, ps, rs) ((q, f) :: env) r'
  | Path_let ((b :: bs), r') ->
      translate_path (xs, qs, ps, rs) env (Path_let ([b], Path_let (bs, r')))
  | Path_call x when List.mem_assoc x rs ->
      translate_path (xs, qs, ps, rs) env (List.assoc x rs)

  | Path_choice cases ->
      Path_sum (List.map (translate_path_case (xs, qs, ps, rs) env) cases)

  | _ -> failwith ("translate_path: " ^ Spec.show_path r)

and translate_path_case (xs, qs, ps, rs) env = function
  | None, r   -> translate_path (xs, qs, ps, rs) env r
  | Some p, r ->
      let f : formula = translate_prop (xs, qs, ps, rs) env "unbound" p in
      Ldl.Path_seq [Ldl.Path_test f; translate_path (xs, qs, ps, rs) env r]

(* state -> property -> Ldl.formula *)
and translate_prop (xs, qs, ps, rs) env q (p : property) =
  p |> translate1_rec (xs, qs, ps, rs) q |> translate2_rec (xs, qs, ps, rs) env

(* --------------------------------------------------------------------------------
   translate: spec -> formula
   --------------------------------------------------------------------------------
 *)

(* spec -> Ldl.formula *)
let translate (spec : Spec.spec) =
  let filter f =
    List.fold_left
      (fun rslt decl -> match f decl with Some x -> rslt @ x | None -> rslt) [] in
  let xs : string list = ["true"; "false"; "last"]
    @ filter (function Decl_atomic x -> Some [x] | _ -> None) spec
  and qs : string list = ["initial"; "final"]
    @ filter (function Decl_state qs' -> Some qs' | _ -> None) spec
  and ps : Spec.property list = []
    @ filter (function Decl_property (_, ps') -> Some ps' | _ -> None) spec
  and rs : (string * Spec.path) list =
    filter
      (function Decl_path (Some x, r) -> Some [x, r] | Decl_path (None, r) -> Some ["", r] | _ -> None)
      spec
  in

  (* translate1: property -> prop_term_labelled list *)
  let tms : prop_term_labelled list = translate1 (xs, qs, ps, rs) ps in
  let env : env_t = make_env (xs, qs, ps, rs) ps in
  debug_printf [0b0100] "environ\n%s\n" (show_env_t env);

  (* translate2 : prop_term list -> formula list *)
  translate2 (xs, qs, ps, rs) env tms

*)
