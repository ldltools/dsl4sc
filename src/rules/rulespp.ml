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

let _builtin_events = ["_empty"; "_any"; "_epsilon"; "_skip"]

let _builtin_props = ["_idle"]

(* find_declared decls -> (ev_names, prop_vars, tm_vars) *)

let rec find_declared decls =
  let events, (props : string list), (terms : (string * base_t) list)=
    List.fold_left
      (fun (events, props, terms) -> function
	| Decl_event (e, _) when not (List.mem e events) ->
	    (*Printf.printf "(event:%s)" e;*)
	    events @ [e], props, terms
	| Decl_variable ((p, VT_prop), _) when not (List.mem p props) ->
	    (*Printf.printf "(proposition:%s)" p;*)
	    events, props @ [p], terms
	| Decl_variable ((x, VT_term ty), _) when List.mem_assoc x terms ->
	    if List.assoc x terms <> ty then
	      failwith ("[find_declared] inconsistent declaration: " ^ x);
	    events, props, terms
	| Decl_variable ((x, VT_term ty), _) ->
	    assert (not (List.mem_assoc x terms));
	    if List.mem x props then
	      failwith ("[find_declared] conflict declaration: " ^ x);
	    events, props, terms @ [x, ty]
	| Decl_variable ((x, _), _) ->
	    invalid_arg ("[find_declared] strange variable: " ^ x)
	| _ -> events, props, terms)
      ([], [], []) decls
  in events @ ["_empty"; "_epsilon"; "_skip"; "_any"], props @ ["_idle"], terms

(* find_undeclared decls -> (ev_names, prop_vars, tm_vars) *)

let rec find_undeclared decls =
  let events1, props1, terms1 = find_declared decls in
  let events2, props2, terms2 =
    List.fold_left
      (fun declared -> function
	| Decl_protocol p ->
	    find_undeclared_protocol declared p
	| Decl_property f ->
	    find_undeclared_property declared f
	| Decl_rule (r, _) ->
	    find_undeclared_rule declared (r : Rule.t)
	| _ -> declared)
      (events1, props1, terms1) decls in

  (* post-processing: remove declared *)
  let rec drop n xs =
    if n = 0 then xs else drop (n - 1) (List.tl xs)
  in let events3 = drop (List.length events1) events2
  and props3 = drop (List.length props1) props2
  and terms3 : (string * base_t) list = drop (List.length terms1) terms2
  in events3, props3, terms3

(* protocol *)
and find_undeclared_protocol (events, props, terms) = function
  | Protocol.Proto_event e when not (List.mem e events) ->
      events @ [e], props, terms
  | Protocol.Proto_event e -> events, props, terms

  | Protocol.Proto_seq ps ->
      List.fold_left find_undeclared_protocol (events, props, terms) ps
  | Protocol.Proto_sum ps ->
      List.fold_left find_undeclared_protocol (events, props, terms) ps
  | Protocol.Proto_star p ->
      find_undeclared_protocol (events, props, terms) p
  | _ -> events, props, terms

(* property *)
and find_undeclared_labelled_property (events, props, terms) (f, _) =
  find_undeclared_property (events, props, terms) f

and find_undeclared_property (events, props, terms) = function
  | Property.Prop_atomic "true" | Property.Prop_atomic "false" -> events, props, terms
  | Property.Prop_atomic "last" -> events, props, terms
  | Property.Prop_atomic a when not (List.mem a props) ->
      if List.mem_assoc a terms then
	failwith ("[find_undeclared_property] conflict declaration: " ^ a);
      events, props @ [a], terms
  | Property.Prop_equal (e1, e2) ->
      List.fold_left find_undeclared_term (events, props, terms) [e1; e2]
  | Property.Prop_neg f -> find_undeclared_property (events, props, terms) f
  | Property.Prop_conj fs | Property.Prop_disj fs ->
      List.fold_left find_undeclared_property (events, props, terms) fs
  | Property.Prop_modal (_, r, f) ->
      let rslt = find_undeclared_labelled_path (events, props, terms) r
      in find_undeclared_labelled_property rslt f
  | _ -> events, props, terms

and find_undeclared_term (events, props, terms) = function
  | Tm_var (x, Ty_nat n) when not (List.mem_assoc x terms) ->
      (* note: "Ty_nat n" part is set in the parsing stage *)
      events, props, terms @ [x, Ty_nat n]
  | Tm_op (_, es) ->
      List.fold_left find_undeclared_term (events, props, terms) es
  | _ -> events, props, terms

and find_undeclared_labelled_path (events, props, terms) (p, _) =
  find_undeclared_path (events, props, terms) p

and find_undeclared_path (events, props, terms) = function
  | Property.Path_prop f | Property.Path_test f ->
      find_undeclared_property (events, props, terms) f
  | Property.Path_seq rs | Property.Path_sum rs ->
      List.fold_left find_undeclared_labelled_path (events, props, terms) rs
  | Property.Path_star r -> find_undeclared_labelled_path (events, props, terms) r
  | _ -> events, props, terms

(* rule *)
and find_undeclared_rule (events, props, terms) (r : Rule.t) =
  let events' =
    events @
    match fst r.event with
    | Ev_name e -> if List.mem e events then [] else [e]
    | Ev_name_seq es | Ev_name_seq_compl es ->
	List.filter (fun e -> not (List.mem e events)) es
  in let events', props', terms' =
    let c, _ = r.condition
    in find_undeclared_labelled_property (events', props, terms) c
  in let events', props', terms' =
    List.fold_left
      (fun (events, props, terms) (act, _) ->
	match act with
	| Act_ensure p ->
	    find_undeclared_property (events, props, terms) p
	| Act_raise es ->
	    let events' = 
	      List.fold_left
		(fun rslt e -> if List.mem e rslt then rslt else rslt @ [e])
		events es
	    in events', props, terms
	| Act_preserve ps ->
	    List.fold_left
	      (fun (events, props, terms) p ->
		find_undeclared_property (events, props, terms) p)
	      (events, props, terms) ps
	| _ -> events, props, terms)
      (events', props', terms') r.action
  in events', props', terms'

(* upate terms *)
let rec update_terms_property (tenv : (string * base_t) list) (f : Property.t) =
  match f with
  | Prop_equal (e1, e2) -> Prop_equal (update_term tenv e1, update_term tenv e2)
  | Prop_neg f' -> Prop_neg (update_terms_property tenv f')
  | Prop_conj fs -> Prop_conj (List.map (update_terms_property tenv) fs)
  | Prop_disj fs -> Prop_disj (List.map (update_terms_property tenv) fs)
  | Prop_modal (m, lp, (f, l_opt)) ->
      Prop_modal (m, update_terms_labelled_path tenv lp, (update_terms_property tenv f, l_opt))
  | _ -> f

and update_terms_labelled_path tenv (r, l_opt) =
  update_terms_path tenv r, l_opt

and update_terms_path tenv (r : Property.path) =
  match r with
  | Path_prop f ->
      Path_prop (update_terms_property tenv f)
  | Path_seq rs ->
      Path_seq (List.map (update_terms_labelled_path tenv) rs)
  | Path_sum rs ->
      Path_sum (List.map (update_terms_labelled_path tenv) rs)
  | Path_test f ->
      Path_test (update_terms_property tenv f)
  | Path_star lp ->
      Path_star (update_terms_labelled_path tenv lp)

and update_term tenv e =
  match e with
  | Tm_var (x, Ty_nat n) when List.mem_assoc x tenv ->
      Tm_var (x, List.assoc x tenv)
  | Tm_var (x, Ty_nat n) ->
      (*raise Not_found*)
      failwith ("[update_term] unknown term: " ^ x)
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
	| Act_preserve ps -> Act_preserve (List.map (update_terms_property tenv) ps)
	| _ -> act),
	a_opt)
      r.action
  in
  { event = r.event; condition = c'; action = a'; }

let update_terms (terms : (string * base_t) list) decls =
  let decls', _ =
    List.fold_left
      (fun (rslt, terms) decl ->
	match decl with
	| Decl_variable ((x, VT_term ty), _) when not (List.mem_assoc x terms) ->
	    rslt @ [decl], terms @ [x, ty]
	| Decl_property p ->
	    rslt @ [Decl_property (update_terms_property terms p)], terms
	| Decl_rule (r, opt) ->
	    rslt @ [Decl_rule (update_terms_rule terms r, opt)], terms
	| _ -> rslt @ [decl], terms)
      ([], terms) decls
  in decls'

(* add_undeclared *)
let pp_add_undeclared ?(allow_undeclared = true) (decls : Rules.decl list) =
  let (events : string list), (props : string list), (terms : (string * base_t) list) =
    find_undeclared decls
  in

  let out str =
    ()
    (*output_string stderr str*)
  in
  List.iter 
    (fun (e : string) ->
      (if not allow_undeclared
      then invalid_arg ("undeclared event: " ^ e)
      else out ("[pp_add_undeclared] event: " ^ e ^ "\n")))
    events;
  List.iter 
    (fun (p : string) ->
      (if not allow_undeclared
      then invalid_arg ("undeclared proposition variable: " ^ p)
      else out ("[pp_add_undeclared] proposition: " ^ p ^ "\n")))
    props;
  List.iter 
    (fun (x, _) ->
      (if not allow_undeclared
      then invalid_arg ("undeclared term variable: " ^ x)
      else out ("[pp_add_undeclared] term: " ^ x ^ "\n")))
    terms;

  let decls' =
    decls
    @ (List.map (fun e -> Decl_event (e, None) ) events)
    @ (List.map (fun p -> Decl_variable ((p, VT_prop), None)) props)
    @ (List.map (fun (x, ty) -> Decl_variable ((x, VT_term ty), None)) terms)

  (* post processing: update terms in properties/rules *)
  in update_terms terms decls'

(** protocol *)

(* any_expand *)
let rec pp_expand_any decls =
  let declared, _, _ = find_declared decls in

  let user_events =
    List.filter (fun e -> not @@ List.mem e ["_empty"; "_epsilon"; "_skip"; "_any"]) declared
  in
  let any_expanded =
    if user_events <> []
    then Proto_sum (List.map (fun e -> Proto_event e) user_events)
    else Proto_sum [] (* empty *)
  in
  List.map
    (fun decl ->
      match decl with
      | Decl_protocol p ->
	  let p' = expand_any_protocol any_expanded p in Decl_protocol p'
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

(* minimize_protocol *)

let rec pp_minimize_protocols ?(always = false) decls =
  List.fold_left
    (fun rslt -> function
      | Decl_protocol p when always || Protocol.mem_event "_epsilon" p ->
	  rslt @ [Decl_protocol (Protocol.minimize p)]
      | decl -> rslt @ [decl])
    [] decls

(* add_trailer *)

let rec pp_add_trailer decls =
  List.fold_left
    (fun rslt -> function
      | Decl_protocol p when not @@ mem_event "_end" p ->
	  rslt @ [Decl_protocol (Proto_seq [p; Proto_event "_end"])]
      | decl -> rslt @ [decl])
    [] decls

(* protocol_relax *)

let rec pp_relax_protocols decls =
  List.fold_left
    (fun rslt -> function
      | Decl_protocol p ->
	  let p' = relax_protocol p in rslt @ [Decl_protocol p']
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

(** rule *)

(* expand preserve rules
   - "events" include all user-defined events (used for finding complement events)
   - rules generated by expansion are all named "_r_preserve"
     which will be looked up by Rules.print_rules_in_xml
 *)

let rec pp_expand_preserve (events : string list) (decls : decl list) =
  List.fold_left
    (fun (rslt : decl list) decl ->
      match decl with
      | Decl_rule (r, None) ->
	  (*print_rule (output_string stderr) r; output_string stderr "\n";*)
	  (*output_string stderr ((show_rule r) ^ "\n");*)
	  let (e, _), ((c, _), _), acts = r.event, r.condition, r.action in
	  let decls' =
	    match acts with
	    | [(Act_preserve (ps : Property.t list)), None] ->
		(* event_names -> prop_names -> rules *)
		let expand (es : string list) ps =
		  List.fold_left
		    (fun rslt e -> rslt @ pp_expand_preserve_helper (e, c, ps))
		    [] es
		and complement es_all es =
		  (* complement es_all -> es -> {e' | e' ∉ es } *)
		  List.filter (fun e -> not (List.mem e es)) es_all
		in
		let rs' =
		  match e with
		  | Ev_name e' -> expand [e'] ps
		  | Ev_name_seq es -> expand es ps
		  | Ev_name_seq_compl es -> expand (complement events es) ps
		in
		List.map (fun r' -> Decl_rule (r', Some "_r_preserve")) rs'
	    | _ ->
		(* non-preserve rule *)
		[Decl_rule (r, None)]
	  in
	  rslt @ decls'

      | Decl_rule (r, Some annot) ->
	  failwith ("[expand_preserve] annotated rule (" ^ annot ^ ") not permitted")
      | _ -> rslt @ [decl])
    [] decls

(* rule (on e when c preserve ps) -> normal (non-preserve) rules *)
and pp_expand_preserve_helper (e, c, ps) =
  List.fold_left
    (fun rslt p ->
      let r1 : Rule.t =
	{ event = (Ev_name e, None);
	  condition = (Property.simp (Prop_conj [c; p]), None), None;
	  action = [Act_ensure p, None];
	}
      and r2 =
	{ event = (Ev_name e, None);
	  condition = (Property.simp (Prop_conj [c; Prop_neg p]), None), None;
	  action = [Act_ensure (Prop_neg p), None];
	}
      in rslt @ [r1; r2])
    [] ps

(* discard codes
   strip off code fragments (in JS) from rules
 *)

let rec pp_discard_codes decls =
  List.fold_left
    (fun rslt decl ->
      match decl with
      | Decl_rule (r, None) ->
	  let (e, _), (c, _) = r.event, r.condition
	  and a' = List.map (fun (act, _) -> (act, None)) r.action in
	  let r' = { event = (e, None); condition = (c, None); action = a'; }
	  in rslt @ [Decl_rule (r', None)]

      | Decl_rule (r, Some annot) ->
	  failwith ("[discard_codes] annotated rule (" ^ annot ^ ") not permitted")
      | _ -> rslt @ [decl])
    [] decls

(** preprocess : Rules.decl list -> Rules.decl list *)

let rec preprocess
    ?(allow_undeclared = true)

    (* protocol *)
    ?(expand_any = true)
    ?(minimize_protocols = 1)
    ?(relax_protocols = false)

    (* property *)

    (* rule *)
    ?(expand_preserve = true)
    ?(discard_codes = false)

    (decls : Rules.decl list) =

  let identity decls = decls in
  decls

  (* event / variable *)
  |> pp_add_undeclared ~allow_undeclared: allow_undeclared

  (* protocol *)
  |> (if expand_any then pp_expand_any else identity)
  |> pp_add_trailer
  |> pp_minimize_protocols ~always: (minimize_protocols > 1)
  (*|> (if relax_protocols then pp_relax_protocols else identity)*)

  (* property *)
  (*
  |> (if split_cases then pp_split_and_propositionalize ~split_only: (not propositionalize) else identity)
  |> (if extra_properties then align_propositions else identity)
  |> (if extra_properties then add_special_properties ~protocol_relax: protocol_relax else identity)
   *)

  (* rule *)
  |> (if discard_codes then pp_discard_codes else identity)

  (* move "preserve" rules to the last part *)
  |> (fun decls ->
      let decls', pres_rules =
	List.fold_left
	  (fun (decls', pres_rules) decl ->
	    match decl with
	    | Decl_rule (r, None)
	      when (match r.action with [(Act_preserve _), _] -> true | _ -> false) ->
		decls', pres_rules @ [decl]
	    | _ -> decls' @ [decl], pres_rules)
	  ([], []) decls
      in decls' @ pres_rules)

  (* expand "preserve" rules to normal ones
     note: each expanded rule is named "_r_preserve"
     which will be looked up by Rules.print_rules_in_xml
   *)
  |> (fun decls ->
      if not expand_preserve then decls else
      let events : string list =
	(* events = all user-defined events *)
	List.fold_left
	  (fun rslt -> function
	    | Decl_event (e, _) when not (List.mem e ["_skip"]) ->
		(* collect events (except built-in events) *)
		rslt @ [e]
	    | _ -> rslt)
	  [] decls
      in
      pp_expand_preserve events decls)

(** deprecated *)

(** property *)

(* split and propositionalize *)
(*
let pp_split_and_propositionalize ?(split_only = false) decls =
  let (env : (string * variable_type) list), ps, rdecls, rest =
    (* env = [(x, t); ..]
       ps = properties that include term variables
     *)
    List.fold_left
      (fun (env, ps, rdecls, rest) decl ->
	match decl with
	| Decl_variable ((x, VT_term (Ty_nat n)), opt) when not split_only ->
	    let xs = Property.term_to_propositions (Tm_var (x, Ty_nat n)) in
	    env @ [x, VT_term (Ty_nat n)] @ List.map (fun x -> x, VT_prop) xs,
	    ps, rdecls,
	    rest @ 
	    List.map (fun x' -> Decl_variable ((x', VT_prop), None)) xs
	| Decl_variable ((x, ty), _) ->
	    env @ [x, ty],
	    ps, rdecls,
	    rest @ [decl]
	| Decl_property (hd, (p, opt)) ->
	    let p' = Property.simp p in
	    if Property.include_term_variable_p p'
	    then env, ps @ [p'], rdecls, rest
	    else env, ps, rdecls, rest @ [Decl_property (hd, (p', opt))]
	| Decl_rule _ -> env, ps, rdecls @ [decl], rest
	| _ -> env, ps, rdecls, rest @ [decl])
      ([], [], [], []) decls

  in let alist : ((string * (Property.base_t * int)) list * Property.t) list =
    (* split : ps -> alist = [(var_binding, property); ...] *)
    Property.split (Prop_conj ps)

  in let qs : Property.t list =
    (* alist -> qs = properties (for cases) *)
    List.map
      (fun (env, q) ->
	(* for each case (env, q) in alist *)
	let binds, in_range =
	  List.fold_left
	    (fun (rslt, in_range) (x, (Ty_nat n, n')) ->
	      (* (x, (ty, n') -> x = n' *)
	      let eq = Prop_equal (Tm_var (x, Ty_nat n), Tm_const (n', Ty_nat n'))
	      in
	      rslt @ [if split_only then eq else Property.propositionalize eq],
	      in_range && n > n')
	    ([], true) env
	in let q' = if in_range then Property.simp q else Prop_atomic "false"
	in Prop_disj [Prop_neg (Prop_conj binds); q'])
      alist
  in let rdecls' =
    List.fold_left
      (fun rslt decl ->
	match decl with
	| Decl_rule (hd, r) ->
(*
	    (* ensure no term variable appears in preserve rules -- unsupported *)
	    List.iter
	      (fun (act, _) ->
		match act with
		| Act_preserve ps ->
		    (* if xs includes a nat variable of nat*)
		    (match
		      List.find_opt
			(fun x ->
			  assert (List.mem_assoc x env);
			  match List.assoc x env with
			  | VT_nat _ -> true | _ -> false)
			ps
		    with None -> () | Some _ -> ())
		| _ -> ())
	      r.action;
*)
	    rslt @ [Decl_rule (hd, Rule.propositionalize r)]
	| _ -> failwith "[split_and_propositionalize]")
      [] rdecls
  in
  (*
  Property.print_property (output_string stderr) (Prop_disj qs);
  output_string stderr "\n";
   *)
  rest
  @ (if ps = [] then [] else [Decl_property (None, ((Prop_conj qs), None))])
  @ rdecls'
*)


(* variables_declare *)

(*
let variables_declare decls =
  List.fold_left
    (fun (rslt : decl list) decl ->
      match decl with
      | Decl_variable (("", Ty_impl None), Some str) ->
	  (* str = "x1=e1; x2=e2; ...;" *)
	  List.fold_left
	    (fun (rslt : decl list) str' ->
	      let pair = String.split_on_char '=' str' in
	      match List.length pair with
	      | 1 ->
		  let x :: _ = pair in
		  let x = String.trim x in
		  rslt @ if x = "" then [] else [Decl_variable ((x, Ty_impl None), None)]
	      | 2 ->
		  let x :: e :: _ = pair in
		  let x = String.trim x in
		  rslt @ if x = "" then [] else [Decl_variable ((x, Ty_impl None), Some (String.trim e))]
	      | _ -> failwith "variables_declare 1")
	    rslt (String.split_on_char ';' str)
      | Decl_variable (("", Ty_impl _), _) ->
	  failwith "variables_declare 2"
      | _ -> rslt @ [decl])
    [] decls
 *)

(* proposition_align
   for each proposition p, add [true*] (<p>!p & <!p>p -> <true>!_idle) as a property.
 *)

let rec align_propositions decls =
  let decls' =
    List.fold_left
      (fun rslt decl ->
	match decl with
	| Decl_variable ((p, VT_prop), p_opt) ->
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
	    in rslt @ [Decl_property prop]
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
	      | Decl_property p' when equal_property p' p -> true
	      | _ -> false)
	    decls
	in rslt
      with Not_found -> rslt @ [Decl_property p])
    decls props_on_events

(*
  (* if a "preserve" rule is included in (the last part of) decls,
     insert a special "_skip" rule to mark the end of non-preserve rules,
     which suggests the subsequent rules can be ignored in statechart generation
     (this marker event will be looked up by Rules.print_rules_in_xml)
   *)
  |> (fun decls ->
      (* special rule r *)
      let r : rule =
	{ event = Ev_name "_skip", None;
	  condition = (Prop_atomic "true", None), None;
	  action = [(Act_ensure (Prop_atomic "true")), None];
	}
      (* insert r *)
      in let _, decls' =
	List.fold_left
	  (fun (b, rslt) decl ->
	    (* "b = true" indicates that _skip rule (r) has already been inserted *)
	    if b then b, rslt @ [decl] else
	    match decl with
	    | Decl_rule (None, r')
	      when (match r'.action with [(Act_preserve _), _] -> true | _ -> false) ->
		true, rslt @ [Decl_rule (None, r); decl]
	    | _ -> b, rslt @ [decl])
	  (false, []) decls
      in decls')
 *)
