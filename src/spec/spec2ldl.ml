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

open Protocol
open Property
open Rule

(** event *)

type event_map = (string * Property.t) list

(* gen_bits nbit n returns [b(0); b(1); ...; b(nbit - 1)] = bit-representation of n
 *)
let rec gen_bits nbit (n : int) =
  gen_bits_rec nbit n [] 0

and gen_bits_rec nbit (n : int) rslt i =
  if i = nbit then
    rslt
  else
    gen_bits_rec nbit n (rslt @ [n land (1 lsl i) <> 0]) (i + 1)

(* [e1; e2; ..] -> [done(e1); done(e2); ..]
   where done(e) = proposition that indicates processing of e has just been finished
 *)
let events_to_map (events : string list) =
  assert (not @@ List.mem "_skip" events);
  assert (List.length (List.sort_uniq compare events) = List.length events);
  let es = "_skip" :: List.sort_uniq compare events
  in let n = List.length es
  in let nbit = int_of_float @@ ceil @@ log (float_of_int n) /. log 2.0
  in let props, _ =
    List.fold_left
      (fun (rslt, i) e ->
	(* gen property for e (= the i-th event) *)
	let bits : bool list = gen_bits nbit i
	in let conj, _ =
	  List.fold_left
	    (fun (rslt, j) b ->
	      let p = Prop_atomic ("_b" ^ string_of_int j)
	      in rslt @ [if b then p else Prop_neg p], j + 1)
	    ([], 0) bits
	in rslt @ [e, Prop_conj conj], i + 1)
      ([], 0) es
  in props

(** protocol
    ⟦ p ⟧ = <_idle; (protocol_to_path p)> (last & _idle)
 *)

let rec property_of_protocol (map : event_map) (p : Protocol.t) =
  let _idle = List.assoc "_skip" map
  in let r : Property.labelled_path = protocol_to_path map p
  in
  Prop_modal
    (Mod_ex,
     (Path_seq [Path_prop _idle, None; r], None),
     (Prop_conj [Prop_atomic "last"; _idle], None))

(* Protocol.t -> Property.labelled_path *)
and protocol_to_path map (p : Protocol.t) =
  let r : Property.path =
    match p with
    | Proto_event "_epsilon" ->
	failwith "[protocol_to_path] protocol include _epsilon"
    | Proto_event e when List.mem e ["_any"; "_empty"] ->
	invalid_arg ("[protocol_to_path] invalid event: " ^ e)
    | Proto_event e -> Path_prop (List.assoc e map)

    | Proto_seq ps  -> Path_seq (List.map (protocol_to_path map) ps)
    | Proto_sum ps  -> Path_sum (List.map (protocol_to_path map) ps)
    | Proto_star p' -> Path_star (protocol_to_path map p')

    | _ -> failwith "[protocol_to_path]"
  in r, None

(** property
    expand built-in properties like _idle
 *)

let rec expand_property (map : event_map) p =
  match p with
  | Prop_atomic "_idle" -> List.assoc "_skip" map
  | Prop_atomic a -> Prop_atomic a
  | Prop_equal (e1, e2) -> p
  | Prop_neg p'  -> Prop_neg (expand_property map p')
  | Prop_conj ps -> Prop_conj (List.map (expand_property map) ps)
  | Prop_disj ps -> Prop_disj (List.map (expand_property map) ps)
  | Prop_modal (m, (r, r_opt), (q, q_opt)) ->
      Prop_modal (m, (expand_path map r, r_opt), (expand_property map q, q_opt))      

  | _ ->
      failwith ("[expand_property] invalid property: " ^ Property.string_of_property p)

(*
and expand_lpath map ((r, r_opt) : Property.labelled_path) =
  expand_path map r
 *)

and expand_path map (r : Property.path) =
  match r with
  | Path_prop p -> Path_prop (expand_property map p)
  | Path_seq rs ->
      Path_seq (List.map (fun (r, _) -> (expand_path map r, None)) rs)
  | Path_sum rs ->
      Path_sum (List.map (fun (r, _) -> expand_path map r, None) rs)
  | Path_test p -> Path_test (expand_property map p)
  | Path_star (r, _) -> Path_star (expand_path map r, None)
  | _ -> failwith "[expand_path]"

(** rule
    ⟦ on e when c do a ⟧ = [true*]>(<c>done(e) -> <c>(done(e) ∧ a))
    ⟦ on e when <ρ>c do a ⟧ = [true*](<ρ;c>done(e) -> <ρ;c>(done(e) ^ a))
 *)

let rec property_of_rule (map : event_map) (r : Rule.t) =
  let (e, _), ((c, _), _), acts = r.event, r.condition, r.action
  in let e_done : Property.t = List.assoc (Rule.event_name e) map
  and c_path : Property.path = property_to_path map c
  and a' = action_to_property map acts
  in
  Prop_modal (Mod_all,
	      (Path_star (Path_prop (Prop_atomic "true"), None), None),
	      (Prop_disj
		 [Prop_neg (Prop_modal (Mod_ex, (c_path, None), (e_done, None)));
		  Prop_modal (Mod_ex, (c_path, None), (Prop_conj [e_done; a'], None))],
	       None))

(* <ρ1><ρ2>ψ -> ρ1; ρ2; ..; ψ *)
and property_to_path map (p : Property.t) =
  match p with
  | _ when not (Property.modal_p p) -> Path_prop (expand_property map p)
  | Prop_modal (Mod_ex, r, (p', _)) ->
      Path_seq [r; property_to_path map @@ expand_property map p', None]
  | _ ->
      failwith ("[property_to_path] invalid property: " ^ Property.string_of_property p)

and action_to_property map (acts : Rule.action) =
  let conj = 
    List.fold_left
      (fun rslt (act, _) ->
	rslt @
	match act with
	| Act_ensure p -> [expand_property map p]
	| Act_raise es -> 
	    (* raise e1 + e2 + .. -> <true>(done(e1) ∨ done(e2) ∨ ..) *)
	    [Prop_modal (Mod_ex,
			 (Path_prop (Prop_atomic "true"), None),
			 (Prop_disj (List.map (fun e -> List.assoc e map) es), None))]
	| _ -> [])
      [] acts
  in Prop_conj conj

(** translatelation
    Spec.t -> Property.t list * event_map
 *)

let rec translate
    ?(propositionalize = true)
    ?(keep_terms = false)
    (s : Spec.t) =

  let map = events_to_map s.events in
  let ps1 = List.map (property_of_protocol map) s.protocols
  and ps2 = List.map (expand_property map) s.properties
  and ps3 = List.map (property_of_rule map) s.rules
  in
  if not propositionalize then ps1 @ ps2 @ ps3, map else
  (* propositionalize *)
  ps1 @ List.map (Property.propositionalize ~keep_terms) (ps2 @ ps3), map

(** formula *)

let rec formula_of_property (p : Property.t) =
  match p with
  | Prop_atomic a when List.mem a ["_idle"] ->
      failwith ("[formula_of_property] unrecognized property: " ^ a)
  | Prop_atomic a -> Ldl.Ldl_atomic a
  | Prop_neg p' -> Ldl.Ldl_neg (formula_of_property p')
  | Prop_conj ps -> Ldl.Ldl_conj (List.map formula_of_property ps)
  | Prop_disj ps -> Ldl.Ldl_disj (List.map formula_of_property ps)
  | Prop_modal (m, (r, _), (p', _)) ->
      let m' = match m with Mod_ex -> Ldl.Mod_ex | Mod_all -> Ldl.Mod_all in
      Ldl.Ldl_modal (m', path_to_path r, formula_of_property p')
  | _ ->
      failwith ("[formula_of_property] invalid property: " ^ Property.string_of_property p)

and path_to_path (r : Property.path) =
  match r with
  | Path_prop p -> Ldl.Path_prop (formula_of_property p)
  | Path_seq rs ->
      Ldl.Path_seq (List.map (fun (r, _) -> path_to_path r) rs)
  | Path_sum rs ->
      Ldl.Path_sum (List.map (fun (r, _) -> path_to_path r) rs)
  | Path_test p -> Ldl.Path_test (formula_of_property p)
  | Path_star (r, _) -> Ldl.Path_star (path_to_path r)
  | _ -> failwith "[path_to_path]"
    
