(* $Id: rulespp.ml,v 1.1 2018/01/23 03:02:51 sato Exp $ *)
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
open Rules

(** event / variable *)

(* find_declared -- propositions/events/labels *)
let rec find_declared decls =
  let props, events, labs =
    List.fold_left find_declared1 ([], [], []) decls
  in (props @ ["_idle"], events @ ["_epsilon"; "_skip"; "_any"], labs)

and find_declared1 ((props : string list), (events : string list), (labs : string list)) = function
  | Decl_event (e, _) when not (List.mem e events) ->
      (*Printf.printf "(event:%s)" e;*)
      (props, events @ [e], labs)
  | Decl_variable ((x, _), _) ->
      (*Printf.printf "(proposition:%s)" p;*)
      (props @ [x], events, labs)
(*
  | Decl_label l ->
      (*Printf.printf "(label:%s)" l;*)
      (props, events, labs  @ [l])
  | Decl_proposition (p, _) ->
      (*Printf.printf "(proposition:%s)" p;*)
      (props @ [p], events, labs)
 *)
  | _ -> (props, events, labs)

(* find_undeclared -- propositions/events/labels *)
let rec find_undeclared decls =
  let props1, events1, labs1 = find_declared decls in
  let props2, (events2 : string list), labs2 =
    List.fold_left find_undeclared1 (props1, events1, labs1) decls in
  let rec drop n xs = if n = 0 then xs else drop (n - 1) (List.tl xs) in
  let props3 : string list = drop (List.length props1) props2
  and events3 : string list = drop (List.length events1) events2
  and labs3 : string list = drop (List.length labs1) labs2
  in props3, events3, labs3

and find_undeclared1 declared = function
  | Decl_protocol (_, p) ->
      find_undeclared1_protocol declared p
  | Decl_property (_, f) ->
      find_undeclared1_labelled_property declared f
 (*
  | Decl_path (_, r) ->
      find_undeclared1_labelled_path declared r
  *)
  | Decl_rule (_, r) ->
      find_undeclared1_rule declared (r : Rule.t)
  | _ -> declared

and find_undeclared1_protocol (props, events, labs) = function
  | Protocol.Proto_event e when not (List.mem e events) -> (props, events @ [e], labs)
  | Protocol.Proto_event e -> (props, events, labs)

  | Protocol.Proto_seq ps ->
      List.fold_left find_undeclared1_protocol (props, events, labs) ps
  | Protocol.Proto_sum ps ->
      List.fold_left find_undeclared1_protocol (props, events, labs) ps
  | Protocol.Proto_star p ->
      find_undeclared1_protocol (props, events, labs) p

(*
  | Protocol.Proto_0or1 p ->
      find_undeclared1_protocol (props, events, labs) p
  | Protocol.Proto_prop f -> find_undeclared1_protocol_prop (props, events, labs) f
  | _ -> failwith "find_undeclared1_protocol"

and find_undeclared1_protocol_prop (props, events, labs) = function
  | Protocol.PProp_event e when not (List.mem e events) -> (props, events @ [e], labs)
(*
  | PProp_neg f -> find_undeclared1_protocol_prop (props, events, labs) f
 *)
  | _ -> (props, events, labs)
*)

and find_undeclared1_labelled_property (props, events, labs) = function
  | (f, Some l) when not (List.mem l labs) ->
      find_undeclared1_property (props, events, labs @ [l]) f
  | (f, _) ->
      find_undeclared1_property (props, events, labs) f

and find_undeclared1_property ((props : string list), events, (labs : string list)) = function
  | Property.Prop_atomic "true" | Property.Prop_atomic "false" -> (props, events, labs)
  | Property.Prop_atomic "last" -> (props, events, labs)
  | Property.Prop_atomic a when not (List.mem a props) -> (props @ [a], events, labs)
  | Property.Prop_neg f -> find_undeclared1_property (props, events, labs) f
  | Property.Prop_conj fs | Property.Prop_disj fs ->
      List.fold_left find_undeclared1_property (props, events, labs) fs
  | Property.Prop_modal (_, r, f) ->
      let rslt = find_undeclared1_labelled_path (props, events, labs) r
      in find_undeclared1_labelled_property rslt f
  | Property.Prop_label l when not (List.mem l labs) -> (props, events, labs @ [l])
  | _ -> (props, events, labs)

and find_undeclared1_labelled_path (props, events, labs) = function
  | (r, Some l) when not (List.mem l labs) ->
      find_undeclared1_path (props, events, labs @ [l]) r
  | (r, _) ->
      find_undeclared1_path (props, events, labs) r

and find_undeclared1_path (props, events, labs) = function
  | Property.Path_prop f | Property.Path_test f ->
      find_undeclared1_property (props, events, labs) f
  | Property.Path_seq rs | Property.Path_sum rs ->
      List.fold_left find_undeclared1_labelled_path (props, events, labs) rs
  | Property.Path_star r -> find_undeclared1_labelled_path (props, events, labs) r
  | Property.Path_label l when not (List.mem l labs) -> (props, events, labs @ [l])
  | _ -> (props, events, labs)

and find_undeclared1_rule (props, (events : string list), labs) (r : Rule.t) =
  let es : string list =
    match fst r.event with
    | Ev_name e -> [e]
    | Ev_name_seq es -> es
    | Ev_name_seq_compl es -> es in
  let es =
    let a : action = fst r.action in
    List.fold_left
      (fun es -> function
	| Act_raise es' ->
	    List.fold_left
	      (fun rslt e -> if List.mem e rslt then rslt else rslt @ [e])
	      es es'
	| _ -> es)
      es (snd a)
  in
  let events' =
    List.fold_left
      (fun rslt e -> if List.mem e rslt then rslt else rslt @ [e])
      events es
  in (props, events', labs)

(* add_undeclared *)
let add_undeclared (decls : Rules.decl list) =
  let (props : string list), (events : string list), (labs : string list) = find_undeclared decls in
  let decls' =
    decls
    @ (List.map (fun p -> Decl_variable ((p, VT_prop), None)) props)
    @ (List.map (fun e -> Decl_event (e, None) ) events)
    @ (List.map (fun l -> Decl_label l) labs)
  in  decls'

(* find_labels *)
let find_labels (p : Property.labelled_property) =
  let _, _, labs = find_undeclared1_labelled_property ([], [], []) p
  in labs

(** protocol *)

(* any_expand *)
let rec expand_any decls =
  let _, declared, _ = find_declared decls in
  let user_events = List.filter (fun e -> not @@ List.mem e ["_skip"; "_any"]) declared in
  let any_expanded =
    Proto_sum (List.map (fun e -> Proto_event e) user_events) in
  List.map
    (fun decl ->
      match decl with
      | Decl_protocol (None, p) ->
	  let p' = expand_any_protocol any_expanded p in Decl_protocol (None, p')
      | Decl_protocol _ -> failwith "expand_any"
      | _ -> decl)
    decls

and expand_any_protocol any_expanded p =
  match p with
  | Proto_event "_any" -> any_expanded
  | Proto_event e -> p
  | Proto_seq ps -> Proto_seq (List.map (expand_any_protocol any_expanded) ps)
  | Proto_sum ps -> Proto_sum (List.map (expand_any_protocol any_expanded) ps)
  | Proto_star p' -> Proto_star (expand_any_protocol any_expanded p')
  | _ -> failwith "expand_any_protocol"

(* eliminate_epsilon *)

let rec minimize_protocols ?(always = false) decls =
  List.fold_left
    (fun rslt -> function
      | Decl_protocol (None, p) when always || Protocol.mem_event "_epsilon" p ->
	  rslt @ [Decl_protocol (None, Protocol.minimize p)]
      | Decl_protocol (Some _, _) -> failwith "minimize_protocols"
      | decl -> rslt @ [decl])
    [] decls

(* protocol_relax *)

let rec relax_protocols decls =
  List.fold_left
    (fun rslt -> function
      | Decl_protocol (None, p) ->
	  let p' = relax_protocol p in rslt @ [Decl_protocol (None, p')]
      | Decl_protocol _ -> failwith "relax_protocols"
      | decl -> rslt @ [decl])
    [] decls

(* Rule.protocol -> Rule.protocol *)
and relax_protocol (p : Protocol.t) =
  relax_protocol_rec p |> flatten_protocol |> elim_dup

and relax_protocol_rec (p : Protocol.t) =
  let filler = Proto_star (Proto_event "_skip") in
  match p with
  | Proto_event _ -> Proto_seq [filler; p; filler]
  | Proto_seq ps  -> Proto_seq (List.map relax_protocol_rec ps)
  | Proto_sum ps  -> Proto_sum (List.map relax_protocol_rec ps)
  | Proto_star p  -> Proto_star (relax_protocol_rec p)
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
  | Proto_star (Proto_event "_skip") :: Proto_star (Proto_event "_skip") :: rest ->
      elim_dup1 (Proto_star (Proto_event "_skip") :: rest)
  | Proto_star (Proto_event "_skip") :: p2 :: rest ->
      Proto_star (Proto_event "_skip") :: p2 :: elim_dup1 rest
  | p1 :: Proto_star (Proto_event "_skip") :: rest ->
      p1 :: elim_dup1 (Proto_star (Proto_event "_skip") :: rest)
  | p1 :: p2 :: rest ->
      p1 :: p2 :: elim_dup1 rest

(** property *)

(* propositionalize *)

let rec propositionalize decls =
  let decls', _ =
    List.fold_left
      (fun (rslt, tenv) decl ->
	match decl with
	| Decl_variable ((x, VT_nat n), None) ->
	  let len = float_of_int (n + 1) in
	  let nbit = int_of_float @@ ceil (log len /. log 2.0) in
	  let decls' =
	    List.init nbit
	      (fun i ->
		Decl_variable (("_" ^ x ^ "_" ^ (string_of_int i), VT_prop), None))
	  in (rslt @ decls'), (x, n) :: tenv
	| Decl_variable ((_, VT_nat _), Some _) ->
	    failwith "expand_terms"
	| Decl_property (hd, (f, l_opt)) ->
	    let f' = propositionalize_property tenv f
	    in (rslt @ [Decl_property (hd, (f', l_opt))]), tenv
	| Decl_rule (hd, r) ->
	    let (f, l_opt), c_code = r.condition in
	    let f' = propositionalize_property tenv f in
	    let c' = (f', l_opt), c_code in
	    let (p_opt, acts), a_code = r.action in
	    let acts' = List.map (propositionalize_action tenv) acts in
	    let r' = {event = r.event; condition = c'; action = (p_opt, acts'), a_code; path = r.path}
	    in (rslt @ [Decl_rule (hd, r')]), tenv
	| _ -> (rslt @ [decl]), tenv)
      ([], []) decls
  in decls'

and propositionalize_property tenv f =
  Property.propositionalize @@ annotate_term_type_to_property tenv f

and propositionalize_action tenv (act : Rule.action_unit) =
  match act with
  | Act_ensure f -> Act_ensure (propositionalize_property tenv f)
  | Act_raise _ -> act
  | Act_preserve _ -> act

and annotate_term_type_to_property tenv f =
  match f with
  | Prop_atomic _ -> f
  | Prop_equal (e1, e2) ->
      Prop_equal ((annotate_term_type tenv e1), annotate_term_type tenv e2)
  | Prop_neg g ->
      Prop_neg (annotate_term_type_to_property tenv g)
  | Prop_conj gs ->
      Prop_conj (List.map (annotate_term_type_to_property tenv) gs)
  | Prop_disj gs ->
      Prop_disj (List.map (annotate_term_type_to_property tenv) gs)
  | Prop_modal (m, lp, (g, l_opt)) ->
      Prop_modal (m, lp, ((annotate_term_type_to_property tenv g), l_opt))
  | _ -> failwith "annotate_term_type_to_property"

and annotate_term_type tenv (e : int term) =
  match e with
  | Tm_val _ -> e
  | Tm_var (x, Ty_nat n) when List.mem_assoc x tenv ->
      Tm_var (x, Ty_nat (List.assoc x tenv))
  | Tm_var (x, _) ->
      failwith ("[annotate_term_type] unknown variable: " ^ x)
  | _ -> e

(* proposition_align
   for each proposition p, add [true*] (<p>!p & <!p>p -> <true>!_idle) as a property.
 *)

let rec align_propositions decls =
  List.fold_left
    (fun rslt decl ->
      match decl with
      | Decl_variable ((p, VT_prop), p_opt)
      | Decl_proposition (p, p_opt) ->
	  let prop : Property.property =
	    (* [true*] (<p>!p & <!p>p -> <true>!_idle) *)
	    let tt = Prop_atomic "true" in
	    let p1 : Property.property =
	      let p11 = Prop_modal (Mod_ex, (Path_prop (Prop_atomic p), None), (Prop_neg (Prop_atomic p), None))
	      and p12 = Prop_modal (Mod_ex, (Path_prop (Prop_neg (Prop_atomic p)), None), (Prop_atomic p, None))
	      in Prop_disj [p11; p12]
	    and p2 : Property.property =
	      Prop_modal (Mod_ex, (Path_prop tt, None),
			  (Prop_neg (Prop_atomic "_idle"), None))
	    in
	    Prop_modal (Mod_all, (Path_star (Path_prop tt, None), None),
			(Prop_disj [Prop_neg p1; p2], None))
	  in rslt @ [decl; Decl_property (None, (prop, None))]
      | _ -> rslt @ [decl])
    [] decls

(* code_discard
   strip off code fragments (in JS) from rules
 *)

let rec discard_codes decls =
  List.fold_left
    (fun rslt decl ->
      match decl with
      | Decl_rule (None, r) ->
	  let (e, _), (c, _), (a, _) = r.event, r.condition, r.action in
	  let r' = { event = (e, None); condition = (c, None); action = (a, None); path = None; }
	  in rslt @ [Decl_rule (None, r')]

      | Decl_rule (Some (name, _), r) ->
	  failwith ("discard_codes: named rule (" ^ name ^ ") not permitted")
      | _ -> rslt @ [decl])
    [] decls

(** rule *)

(* preserve_expand
 *)

let expand_preserve (events : string list) (decls : decl list) =
  List.fold_left
    (fun (rslt : decl list) decl ->
      match decl with
      | Decl_rule (None, r) ->
	  (*print_rule (output_string stderr) r; output_string stderr "\n";*)
	  (*output_string stderr ((show_rule r) ^ "\n");*)
	  let (e, _), (c, _), (a, _) = r.event, r.condition, r.action in
	  let rs : rule list =
	    match a with
	    | None, [Act_preserve (ps : string list)] ->
		(* event_names -> prop_names -> rules *)
		let expand (es : string list) ps =
		  List.fold_left
		    (fun rslt (e : string) ->
		      List.fold_left
			(fun rslt p ->
			  let r1 =
			    { event = (Ev_name e, None);
			      condition = ((Prop_atomic p, None), None);
			      action = ((None, [Act_ensure (Prop_atomic p)]), None);
			      path = None;
			    }
			  and r2 =
			    { event = (Ev_name e, None);
			      condition = ((Prop_neg (Prop_atomic p), None), None);
			      action = ((None, [Act_ensure (Prop_neg (Prop_atomic p))]), None);
			      path = None;
			    }
			  in rslt @ [r1; r2])
			rslt ps)
		    [] es
		(* es_all -> es -> {e' | e' ∉ es } *)
		and complement es_all es =
		  (* returns (es_all - es) *)
		  List.filter (fun e -> not (List.mem e es)) es_all
		in
		let rs' =
		  match e with
		  | Ev_name e' -> expand [e'] ps
		  | Ev_name_seq es -> expand es ps
		  | Ev_name_seq_compl es -> expand (complement events es) ps
		in rs'
	    | _ -> [r]
	  in
	  rslt @ List.map (fun r -> Decl_rule (None, r)) rs

      | Decl_rule (Some (name, _), r) ->
	  failwith ("expand_preserve: named rule (" ^ name ^ ") not permitted")
      | _ -> rslt @ [decl])
    [] decls

(* variables_declare *)

(*
let variables_declare decls =
  List.fold_left
    (fun (rslt : decl list) decl ->
      match decl with
      | Decl_variable (("", VT_impl None), Some str) ->
	  (* str = "x1=e1; x2=e2; ...;" *)
	  List.fold_left
	    (fun (rslt : decl list) str' ->
	      let pair = String.split_on_char '=' str' in
	      match List.length pair with
	      | 1 ->
		  let x :: _ = pair in
		  let x = String.trim x in
		  rslt @ if x = "" then [] else [Decl_variable ((x, VT_impl None), None)]
	      | 2 ->
		  let x :: e :: _ = pair in
		  let x = String.trim x in
		  rslt @ if x = "" then [] else [Decl_variable ((x, VT_impl None), Some (String.trim e))]
	      | _ -> failwith "variables_declare 1")
	    rslt (String.split_on_char ';' str)
      | Decl_variable (("", VT_impl _), _) ->
	  failwith "variables_declare 2"
      | _ -> rslt @ [decl])
    [] decls
 *)

(** preprocess : Rules.decl list -> Rules.decl list *)

let rec preprocess
    ?(undeclared_add = true)
    ?(any_expand = true)
    ?(protocol_relax = false)
    ?(proposition_align = true)
    ?(code_discard = false)
    ?(preserve_expand = true)
    (*?(skip_allow = false)*)
    (decls : Rules.decl list) =

  (* special properties for event processing *)
  let props_on_events : Property.property list =
    let idle1 = Prop_atomic "_idle"
	(* _idle *)
    and idle2 =
      Prop_modal (Mod_all, (Path_star (Path_prop (Prop_atomic "true"), None), None),
		  (Prop_disj [Prop_neg (Prop_atomic "last"); Prop_atomic "_idle"], None))
	(* [true*] (last -> _idle) *)
    and idle3 =
      (* this prohibits intermediate _idle states (and thus _skip events).
	 _idle appears only at the beginning/end of the trace.
	 included only when relax_protocol is not set *)
      Prop_modal (Mod_ex, (Path_prop (Prop_atomic "true"), None),
		  (Prop_modal (Mod_all, (Path_star (Path_prop (Prop_atomic "true"), None), None),
			       (Prop_disj [Prop_neg (Prop_atomic "_idle"); Prop_atomic "last"], None)), None))
	(* <true>[true*] (_idle -> last) *)
    in [idle1; idle2] @ if protocol_relax (*|| skip_allow*) then [] else [idle3]
  in

  let identity decls = decls in
  decls
  (* find/add undeclared event/variable names *)
  |> (if undeclared_add then add_undeclared else identity)

  (* event/protocol *)
  |> (if any_expand then expand_any else identity)
  (*|> (if interleaving_apply then apply_interleaving else identity)*)
  |> minimize_protocols
  |> (if protocol_relax then relax_protocols else identity)

  (* variable/property *)
  |> propositionalize
  |> (if proposition_align then align_propositions else identity)
  |> (if code_discard then discard_codes else identity)
  (*|> variables_declare*)

  (* rule *)
  (* move "preserve" rules to the last part *)
  |> (fun decls ->
      let decls', pres_rules =
	List.fold_left
	  (fun (decls', pres_rules) decl ->
	    match decl with
	    | Decl_rule (None, r)
	      when (match fst r.action with _, [Act_preserve _] -> true | _ -> false) ->
		decls', pres_rules @ [decl]
	    | _ -> decls' @ [decl], pres_rules)
	  ([], []) decls
      in decls' @ pres_rules)

  (* add a special "_skip" rule to mark the end of "normal" rules,
     which suggests the subsequent rules can be ignored in statechart generation
   *)
  |> (fun decls ->
      (* special rule r *)
      let r : rule =
	{ event = Ev_name "_skip", None;
	  condition = (Prop_atomic "true", None), None;
	  action = (None, [Act_ensure (Prop_atomic "true")]), None;
	  path = None
	} in
      (* insert r *)
      let _, decls' =
	List.fold_left
	  (fun (b, rslt) decl ->
	    if b then b, rslt @ [decl] else
	    match decl with
	    | Decl_rule (None, r')
	      when (match fst r'.action with _, [Act_preserve _] -> true | _ -> false) ->
		true, rslt @ [Decl_rule (None, r); decl]
	    | _ -> b, rslt @ [decl])
	  (false, []) decls
      in decls')

  (* expand "preserve" rules to normal ones *)
  |> (fun decls ->
      if not preserve_expand then decls else
      let events : string list =
	List.fold_left
	  (fun rslt -> function
	    | Decl_event (e, _) when not (List.mem e ["_skip"]) ->
		rslt @ [e]
	    | _ -> rslt)
	  [] decls in
      expand_preserve events decls)

  (* append special "_idle" event properties *)
  |> (fun decls ->
      List.fold_left
	(fun rslt p ->
	  (* append p to decls only when undeclared *)
	  try
	    let _ =
	      List.find
		(function
		  | Decl_property (None, (p', None)) when equal_property p' p -> true
		  | _ -> false)
		decls
	    in rslt
	  with Not_found -> rslt @ [Decl_property (None, (p, None))])
	decls props_on_events)
