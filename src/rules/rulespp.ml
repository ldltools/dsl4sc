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

(* find_declared -- events / variables *)

let rec find_declared decls =
  let events, props, terms =
    List.fold_left
      (fun (events, props, terms) -> function
	| Decl_event (e, _) when not (List.mem e events) ->
	    (*Printf.printf "(event:%s)" e;*)
	    events @ [e], props, terms
	| Decl_variable ((p, VT_prop), _) when not (List.mem p props) ->
	    (*Printf.printf "(proposition:%s)" p;*)
	    events, props @ [p], terms
	| Decl_variable ((x, VT_nat n), _) when not (List.mem_assoc x terms) ->
	    events, props, terms @ [x, n]
	| _ -> events, props, terms)
      ([], [], []) decls
  in events @ ["_epsilon"; "_skip"; "_any"], props @ ["_idle"], terms

(* find_undeclared -- events and proposition/term variables *)

let rec find_undeclared decls =
  let events1, props1, terms1 = find_declared decls in
  let events2, props2, terms2 =
    List.fold_left
      (fun declared -> function
	| Decl_protocol (_, p) ->
	    find_undeclared1_protocol declared p
	| Decl_property (_, f) ->
	    find_undeclared1_labelled_property declared f
	| Decl_rule (_, r) ->
	    find_undeclared1_rule declared (r : Rule.t)
	| _ -> declared)
      (events1, props1, terms1) decls in

  (* post-processing: remove declared *)
  let rec drop n xs =
    if n = 0 then xs else drop (n - 1) (List.tl xs)
  in let events3 = drop (List.length events1) events2
  and props3 = drop (List.length props1) props2
  and terms3 = drop (List.length terms1) terms2
  in events3, props3, terms3

(* protocol *)
and find_undeclared1_protocol (events, props, terms) = function
  | Protocol.Proto_event e when not (List.mem e events) ->
      events @ [e], props, terms
  | Protocol.Proto_event e -> events, props, terms

  | Protocol.Proto_seq ps ->
      List.fold_left find_undeclared1_protocol (events, props, terms) ps
  | Protocol.Proto_sum ps ->
      List.fold_left find_undeclared1_protocol (events, props, terms) ps
  | Protocol.Proto_star p ->
      find_undeclared1_protocol (events, props, terms) p
  | _ -> events, props, terms

(* property *)
and find_undeclared1_labelled_property (events, props, terms) (f, _) =
  find_undeclared1_property (events, props, terms) f

and find_undeclared1_property (events, props, terms) = function
  | Property.Prop_atomic "true" | Property.Prop_atomic "false" -> events, props, terms
  | Property.Prop_atomic "last" -> events, props, terms
  | Property.Prop_atomic a when not (List.mem a props) -> events, props @ [a], terms
  | Property.Prop_equal (e1, e2) ->
      List.fold_left find_undeclared1_term (events, props, terms) [e1; e2]
  | Property.Prop_neg f -> find_undeclared1_property (events, props, terms) f
  | Property.Prop_conj fs | Property.Prop_disj fs ->
      List.fold_left find_undeclared1_property (events, props, terms) fs
  | Property.Prop_modal (_, r, f) ->
      let rslt = find_undeclared1_labelled_path (events, props, terms) r
      in find_undeclared1_labelled_property rslt f
  | _ -> events, props, terms

and find_undeclared1_term (events, props, terms) = function
  | Tm_var (x, Ty_nat n) when not (List.mem_assoc x terms) ->
      (* note: "VT_nat n" part is set in the parsing stage *)
      events, props, terms @ [x, n]
  | Tm_op (_, es) ->
      List.fold_left find_undeclared1_term (events, props, terms) es
  | _ -> events, props, terms

and find_undeclared1_labelled_path (events, props, terms) (p, _) =
  find_undeclared1_path (events, props, terms) p

and find_undeclared1_path (events, props, terms) = function
  | Property.Path_prop f | Property.Path_test f ->
      find_undeclared1_property (events, props, terms) f
  | Property.Path_seq rs | Property.Path_sum rs ->
      List.fold_left find_undeclared1_labelled_path (events, props, terms) rs
  | Property.Path_star r -> find_undeclared1_labelled_path (events, props, terms) r
  | _ -> events, props, terms

(* rule *)
and find_undeclared1_rule (events, props, terms) (r : Rule.t) =
  let events1 =
    match fst r.event with
    | Ev_name e -> [e]
    | Ev_name_seq es | Ev_name_seq_compl es ->
	List.filter (fun e -> not (List.mem e events)) es
  in let events', props', terms' =
    List.fold_left
      (fun (events, props, terms) (act, _) ->
	match act with
	| Act_raise es ->
	    let events' = 
	      List.fold_left
		(fun rslt e -> if List.mem e rslt then rslt else rslt @ [e])
		events es
	    in events', props, terms
	| _ -> events, props, terms)
      (events @ events1, props, terms) r.action
  in events', props', terms'

(* upate terms *)
let rec update_terms_property tenv (f : Property.t) =
  match f with
  | Prop_equal (e1, e2) -> Prop_equal (update_term tenv e1, update_term tenv e2)
  | Prop_neg f' -> Prop_neg (update_terms_property tenv f')
  | Prop_conj fs -> Prop_conj (List.map (update_terms_property tenv) fs)
  | Prop_disj fs -> Prop_disj (List.map (update_terms_property tenv) fs)
  | Prop_modal (m, lp, (f, l_opt)) -> Prop_modal (m, lp, (update_terms_property tenv f, l_opt))
  | _ -> f

and update_term tenv e =
  match e with
  | Tm_var (x, Ty_nat n) when List.mem_assoc x tenv ->
      Tm_var (x, Ty_nat (List.assoc x tenv))
  | Tm_var (x, Ty_nat n) ->
      raise Not_found
  | Tm_op (op, es) -> Tm_op (op, List.map (update_term tenv) es)
  | _ -> e

let update_terms_rule tenv (r : Rule.t) =
  let (p, p_opt), c_opt = r.condition
  in let c' = (update_terms_property tenv p, p_opt), c_opt
  in let a' =
    List.map
      (fun (act, a_opt) ->
	(match act with
	| Act_ensure p -> Act_ensure (update_terms_property tenv p)
	| _ -> act),
	a_opt)
      r.action
  in
  { event = r.event; condition = c'; action = a'; path = r.path; }

(* add_undeclared *)
let add_undeclared (decls : Rules.decl list) =
  let (events : string list), (props : string list), terms = find_undeclared decls
  in let decls' =
    decls
    @ (List.map (fun e -> Decl_event (e, None) ) events)
    @ (List.map (fun p -> Decl_variable ((p, VT_prop), None)) props)
    @ (List.map (fun (x, n) -> Decl_variable ((x, VT_nat n), None)) terms) in

  (* post processing: update terms in properties/rules *)
  let decls', _ =
    List.fold_left
      (fun (rslt, terms) decl ->
	match decl with
	| Decl_variable ((x, VT_nat n), _) when not (List.mem_assoc x terms) ->
	    rslt @ [decl], terms @ [x, n]
	| Decl_property (hd, (p, l_opt)) ->
	    rslt @ [Decl_property (hd, (update_terms_property terms p, l_opt))], terms
	| Decl_rule (hd, r) ->
	    rslt @ [Decl_rule (hd, update_terms_rule terms r)], terms
	| _ -> rslt @ [decl], terms)
      ([], terms) decls'
  in decls'

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
      | Decl_protocol _ -> failwith "[expand_any]"
      | _ -> decl)
    decls

and expand_any_protocol any_expanded p =
  match p with
  | Proto_event "_any" -> any_expanded
  | Proto_event e -> p
  | Proto_seq ps -> Proto_seq (List.map (expand_any_protocol any_expanded) ps)
  | Proto_sum ps -> Proto_sum (List.map (expand_any_protocol any_expanded) ps)
  | Proto_star p' -> Proto_star (expand_any_protocol any_expanded p')
  | _ -> failwith "[expand_any_protocol]"

(* eliminate_epsilon *)

let rec minimize_protocols ?(always = false) decls =
  List.fold_left
    (fun rslt -> function
      | Decl_protocol (None, p) when always || Protocol.mem_event "_epsilon" p ->
	  rslt @ [Decl_protocol (None, Protocol.minimize p)]
      | Decl_protocol (Some _, _) -> failwith "[minimize_protocols]"
      | decl -> rslt @ [decl])
    [] decls

(* protocol_relax *)

let rec relax_protocols decls =
  List.fold_left
    (fun rslt -> function
      | Decl_protocol (None, p) ->
	  let p' = relax_protocol p in rslt @ [Decl_protocol (None, p')]
      | Decl_protocol _ -> failwith "[relax_protocols]"
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
  | _ -> failwith "[relax_protocol_rec]"

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

(* split and propositionalize *)

let split_and_propositionalize ?(split_only = false) decls =
  let ps, rdecls, rest =
    List.fold_left
      (fun (ps, rdecls, rest) decl ->
	match decl with
	| Decl_property (_, (p, _)) when Property.include_term_variable_p p ->
	    ps @ [p], rdecls, rest
	| Decl_rule _ -> ps, rdecls @ [decl], rest
	| _ -> ps, rdecls, rest @ [decl])
      ([], [], []) decls
  in let alist : ((string * (Property.base_t * int)) list * Property.t) list =
    (* split : ps -> alist = [(var_binding, property); ...] *)
    Property.split (Prop_conj ps)
  in let qs : Property.t list =
    (* alist -> qs = properties (for cases) *)
    List.map
      (fun (env, q) ->
	let binds, valid =
	  List.fold_left
	    (fun (rslt, valid) (x, (Ty_nat n, n')) ->
	      (* (x, (ty, n') -> x = n' *)
	      let eq = Prop_equal (Tm_var (x, Ty_nat n), Tm_const (n', Ty_nat n'))
	      in
	      rslt @ [if split_only then eq else Property.propositionalize eq],
	      valid && n > n')
	    ([], true) env
	in let q' = if valid then Property.simp q else Prop_atomic "false"
	in Prop_disj [Prop_neg (Prop_conj binds); q'])
      alist
  in let rdecls' =
    List.map
      (function
	| Decl_rule (hd, r) -> Decl_rule (hd, Rule.propositionalize r)
	| _ -> failwith "[split_and_propositionalize]")
      rdecls
  in
  (*
  Property.print_property (output_string stderr) (Prop_disj qs);
  output_string stderr "\n";
   *)
  rest
  @ (if ps = [] then [] else [Decl_property (None, ((Prop_conj qs), None))])
  @ rdecls'

(* proposition_align
   for each proposition p, add [true*] (<p>!p & <!p>p -> <true>!_idle) as a property.
 *)

let rec align_propositions decls =
  let decls' =
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
	    in rslt @ [Decl_property (None, (prop, None))]
	| _ -> rslt)
      [] decls
  in decls @ decls'

(* add special properties [_idle1; _idle2; _idle3] *)
let add_special_properties ?(protocol_relax = false) decls=
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
  List.fold_left
    (fun rslt p ->
      (* append special_events to decls when undeclared *)
      try
	let _ =
	  List.find
	    (function
	      | Decl_property (None, (p', None)) when equal_property p' p -> true
	      | _ -> false)
	    decls
	in rslt
      with Not_found -> rslt @ [Decl_property (None, (p, None))])
    decls props_on_events

(** rule *)

(* code_discard
   strip off code fragments (in JS) from rules
 *)

let rec discard_codes decls =
  List.fold_left
    (fun rslt decl ->
      match decl with
      | Decl_rule (None, r) ->
	  let (e, _), (c, _) = r.event, r.condition
	  and a' = List.map (fun (act, _) -> (act, None)) r.action in
	  let r' = { event = (e, None); condition = (c, None); action = a'; path = None; }
	  in rslt @ [Decl_rule (None, r')]

      | Decl_rule (Some (name, _), r) ->
	  failwith ("[discard_codes] named rule (" ^ name ^ ") not permitted")
      | _ -> rslt @ [decl])
    [] decls

(* preserve_expand
 *)

let expand_preserve (events : string list) (decls : decl list) =
  List.fold_left
    (fun (rslt : decl list) decl ->
      match decl with
      | Decl_rule (None, r) ->
	  (*print_rule (output_string stderr) r; output_string stderr "\n";*)
	  (*output_string stderr ((show_rule r) ^ "\n");*)
	  let (e, _), (c, _), acts = r.event, r.condition, r.action in
	  let rs : rule list =
	    match acts with
	    | [(Act_preserve (ps : string list)), None] ->
		(* event_names -> prop_names -> rules *)
		let expand (es : string list) ps =
		  List.fold_left
		    (fun rslt (e : string) ->
		      List.fold_left
			(fun rslt p ->
			  let r1 =
			    { event = (Ev_name e, None);
			      condition = ((Prop_atomic p, None), None);
			      action = [(Act_ensure (Prop_atomic p)), None];
			      path = None;
			    }
			  and r2 =
			    { event = (Ev_name e, None);
			      condition = ((Prop_neg (Prop_atomic p), None), None);
			      action = [(Act_ensure (Prop_neg (Prop_atomic p))), None];
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
	  failwith ("[expand_preserve] named rule (" ^ name ^ ") not permitted")
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
    (* protocol *)
    ?(any_expand = true)
    ?(protocol_relax = false)

    (* property *)
    ?(case_split = true)
    ?(propositionalize = true)
    ?(extra_properties = true)

    (* rule *)
    ?(code_discard = false)
    ?(preserve_expand = true)

    (decls : Rules.decl list) =

  let identity decls = decls in
  decls

  (* event / variable *)
  |> add_undeclared
  (*|> variables_declare*)

  (* protocol *)
  |> (if any_expand then expand_any else identity)
  |> minimize_protocols
  |> (if protocol_relax then relax_protocols else identity)

  (* property *)
  |> (if case_split then split_and_propositionalize ~split_only: (not propositionalize) else identity)
  |> (if extra_properties then align_propositions else identity)
  |> (if extra_properties then add_special_properties ~protocol_relax: protocol_relax else identity)

  (* rule *)
  |> (if code_discard then discard_codes else identity)

  (* move "preserve" rules to the last part *)
  |> (fun decls ->
      let decls', pres_rules =
	List.fold_left
	  (fun (decls', pres_rules) decl ->
	    match decl with
	    | Decl_rule (None, r)
	      when (match r.action with [(Act_preserve _), _] -> true | _ -> false) ->
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
	  action = [(Act_ensure (Prop_atomic "true")), None];
	  path = None
	} in
      (* insert r *)
      let _, decls' =
	List.fold_left
	  (fun (b, rslt) decl ->
	    if b then b, rslt @ [decl] else
	    match decl with
	    | Decl_rule (None, r')
	      when (match r'.action with [(Act_preserve _), _] -> true | _ -> false) ->
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
