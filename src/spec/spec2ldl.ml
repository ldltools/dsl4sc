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
  let _idle : formula = event_to_formula_aux nbit es "_skip" in
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
  | Proto_event e -> Ldl.Path_prop (event_to_formula_aux nbit es e)
  | Proto_seq ps  -> Ldl.Path_seq (List.map (protocol_to_path nbit es) ps)
  | Proto_sum ps  -> Ldl.Path_sum (List.map (protocol_to_path nbit es) ps)
  | Proto_star p' -> Ldl.Path_star (protocol_to_path nbit es p')
(*
  | Proto_0or1 p' ->
      let _idle = event_to_formula_rec nbit 0 [] 0 in
      Ldl.Path_test (Ldl.Ldl_modal (Mod_ex, (protocol_to_path nbit es p'), _idle))
 *)
  | _ -> failwith "protocol_to_path"

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
