(* $Id: rules.ml,v 1.4 2018/01/10 11:24:36 sato Exp $ *)
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

(** rules *)
type rules =
    { event_decls : event_spec list;
      proto_decls : protocol_spec list;

      var_decls : variable_spec list;
      prop_decls : property_spec list;

      rule_decls : rule_spec list;
      extra_decls : string list;
    }

(** event *)
and event_spec =
    string * string option

(** protocol *)
and protocol_spec =
    (string * string list) option * Protocol.protocol

(** variable *)
and variable_spec =
    (* (name, type), code *)
    (string * variable_type) * string option

and variable_type =
  | VT_prop			(* type of propositional variables. *)
  | VT_term of Property.base_t	(* term type *)

(** property *)
and property_spec =
    (string * string list) option * Property.labelled_property
    (* (name, args), property *)

(** rule *)
and rule_spec =
    (string * string list) option * Rule.rule
    (* (name, args), rule *)

[@@deriving show, yojson]

type t = rules

(** for parsing *)

type decl =
  (* protocol *)
  | Decl_event of event_spec
  | Decl_protocol of protocol_spec

  (* property *)
  | Decl_variable of variable_spec
  | Decl_property of property_spec

  (* eca rule *)
  | Decl_rule of rule_spec

  (* extras *)
  | Decl_impl of string

(* decls to rules *)
let rec decls_to_rules ?(event_sort = true) (decls : decl list) =
  let events, protos, vars, props, rules, impls =
    List.fold_left decls_to_rules_rec ([], [], [], [], [], []) decls
  in
  { event_decls =
      if event_sort then List.sort_uniq (fun (e1, _) (e2, _) -> compare e1 e2) events else events;
    proto_decls = protos;

    var_decls = vars;
    prop_decls = props;

    rule_decls = rules;
    extra_decls = impls;
  }

and decls_to_rules_rec (events, protos, vars, props, rules, impls) = function
  | Decl_event ev ->
      (events @ [ev], protos, vars, props, rules, impls)
  | Decl_protocol proto ->
      (events, protos @ [proto], vars, props, rules, impls)

  | Decl_variable var ->
      (events, protos, vars @ [var], props, rules, impls)
  | Decl_property prop ->
      (events, protos, vars, props @ [prop], rules, impls)

  | Decl_rule rule ->
      (events, protos, vars, props, rules @ [rule], impls)
  | Decl_impl impl ->
      (events, protos, vars, props, rules, impls @ [impl])

(*
  | Decl_proposition pvar ->
      (events, protos, pvars @ [pvar], vars, props, rules)
  | Decl_path path ->
      (pvars, props, paths @ [path], labs, events, protos, rules)
  | Decl_label lab ->
      (pvars, props, paths, labs @ [lab], events, protos, rules)
 *)

  | _ -> failwith "decls_to_rules_rec"

(** pretty-printing *)

let print_rules out (rs : t) =
  let filter f = List.fold_left (fun rslt decl -> rslt @ f decl) [] in

  (* event *)
  let es = List.map fst rs.event_decls in
  if es <> [] then
    begin
      out "event ";
      out (List.hd es);
      List.iter	(function e -> out ", "; out e) (List.tl es);
      out ";\n";
    end;

  (* protocol *)
  if rs.proto_decls <> [] then
    begin
      out "protocol\n";
      List.iter
	(function
	  (*
	  | None, p ->
	      out " { "; Rule.print_protocol out p; out " }\n"
	  | Some (name, []), p ->
	      out " "; out name; out " ()";
	      out " { "; Rule.print_protocol out p; out " }\n"
	  | Some (name, arg :: args), p ->
	      out " "; out name;
	      out " ("; out arg; List.iter (fun arg -> out ", "; out arg) args; out ")";
	      out " { "; Rule.print_protocol out p; out " }\n"
	   *)
	  | _, p ->
	      out " "; Protocol.print_protocol out p; out ";;\n")
	rs.proto_decls;
    end;

  (* variable *)
  if rs.var_decls <> [] then
    begin
      out "variable\n";
      List.iter
	(function
	  | (x, VT_prop), None ->
	      out " "; out x;
	      out " : prop;\n"
	  | (x, VT_prop), Some e ->
	      out " "; out x; out " { "; out e; out " }";
	      out " : prop;\n"
	  | (x, VT_term (Ty_nat n)), None ->
	      out " "; out x;
	      out " : nat ("; out (string_of_int n); out ");\n"
	  | (x, VT_term (Ty_nat n)), Some e ->
	      out " "; out x; out " { "; out e; out " }";
	      out " : nat ("; out (string_of_int n); out ");\n"
	  | _ -> ())
	rs.var_decls;
    end;

  (* propery *)
  if rs.prop_decls <> [] then
    begin
      out "property\n";
      List.iter
	(function
	  (*
	  | None, p ->
	      out " { "; Rule.print_labelled_property out p; out " }\n"
	  | Some (name, []), p ->
	      out " "; out name; out " ()";
	      out " { "; Rule.print_labelled_property out p; out " }\n"
	  | Some (name, arg :: args), p ->
	      out " "; out name;
	      out " ("; out arg; List.iter (fun arg -> out ", "; out arg) args; out ")";
	      out " { "; Rule.print_labelled_property out p; out " }\n"
	   *)
	  | _, p ->
	      out " "; Property.print_labelled_property out p; out ";\n")
	rs.prop_decls;
    end;

  (* rule *)
  if rs.rule_decls <> [] then
    begin
      out "rule\n";
      List.iter
	(function
	  (*
	  | None, r ->
	      out " { "; Rule.print_rule out r; out " }\n"
	  | Some (name, []), r ->
	      out " "; out name; out " ()";
	      out " { "; Rule.print_rule out r; out " }\n"
	  | Some (name, arg :: args), r ->
	      out " "; out name;
	      out " ("; out arg; List.iter (fun arg -> out ", "; out arg) args; out ")";
	      out " { "; Rule.print_rule out r; out " }\n"
	   *)
	  | _, r ->
	      out " "; Rule.print_rule out r; out "\n")
	rs.rule_decls;
    end;

  (* implementation *)
  if rs.extra_decls <> [] then
    begin
      out "implementation\n";
      out "{\n";
      List.iter out rs.extra_decls;
      out "}\n";
    end;

(*
  (* proposition *)
  let ps : string list = List.map fst rs.pvar_decls in
  if ps <> [] then
    begin
      out "proposition ";
      out (List.hd ps);
      List.iter	(function p -> out ", "; out p) (List.tl ps);
      out " ;\n";
    end;

  (* label *)
  let ls = rs.label_decls in
  if ls <> [] then
    begin
      out "label ";
      out (List.hd ls);
      List.iter	(function l -> out ", "; out l) (List.tl ls);
      out " ;\n";
    end;
 *)

  ()

let string_of_rules (rs : t) =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  print_rules concat rs;
  !str

(** pretty-printing (xml) *)

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

let rec print_rules_in_xml out (rules : t) =
  let filter f = List.fold_left (fun rslt r -> rslt @ f r) [] in
  let print tag seq p =
    match seq with
    | [] -> ()
    | seq ->
	out ("<" ^ tag ^ ">\n");
	List.iter (p out) seq;
	out ("</" ^ tag ^ ">\n")
  in    
  out "<rules xmlns=\"https://github.com/ldltools/dsl4sc\">\n";

  out "<preamble>\n";

  print "events"
    rules.event_decls
    print_event_in_xml;
  print "protocols"
    rules.proto_decls
    print_protocol_in_xml;

  print "variables"
    rules.var_decls
    print_variable_in_xml;
  print "properties"
    rules.prop_decls
    print_property_in_xml;

  out "</preamble>\n";

  (* rule -- strip off the special "_skip" rule and its successors.
     cf. Rulespp.preprocess
   *)
  (*
  let _, (r_specs : rule_spec list) =
    List.fold_left
      (fun (b, rslt) (rspec : rule_spec) ->
	if b then b, rslt else
	let special_rule : Rule.t = 
	  { event = Ev_name "_skip", None;
	    condition = (Prop_atomic "true", None), None;
	    action = [(Act_ensure (Prop_atomic "true")), None];
	  }
	in let name_opt, (r : Rule.t) = rspec in
	assert (name_opt = None);
	if r = special_rule then true, rslt else b, rslt @ [rspec])
      (false, []) rules.rule_decls
   *)
  let r_specs =
    List.filter
      (function Some ("_r_preserve", _), _ -> false | _ -> true)
      rules.rule_decls
  in
  let _ =
    List.fold_left
      (fun i r_spec ->
	print_rule_in_xml out ~id: (Some ("r" ^ (string_of_int i))) r_spec;
	i + 1)
      1 r_specs
  in

  print "implementation"
    rules.extra_decls
    (fun out -> escape out);

  out "</rules>\n"

(*
and print_proposition_in_xml out ((str, opt) : proposition_spec) =
  out "<proposition variable=\""; out str; out "\"";
  match opt with
  | None -> out "/>\n"
  | Some str' ->
      out ">\n<script>"; escape out str'; out "</script>\n";
      out "</proposition>\n"
 *)

and print_property_in_xml out (name_opt, lp) =
  out "<property";
  let _ =
    match name_opt with
    | None -> out ">"
    | Some (str', _) ->
	out " name=\""; out str'; out "\">"
  in
  escape out (Property.string_of_labelled_property lp);
  out "</property>\n";

(*
  match lp_seq with
  (*
    | [lp] ->
    out "<property>\n";
    let _ =
    match name_opt with
    | None -> ()
    | Some str' ->
    out "<name>"; out str'; out "</name>\n"
    in
    out "<proposition>";
    escape out (Rule.string_of_labelled_property lp);
    out "</proposition>\n";
    out "</property>\n"
   *)
  | _ ->
  out "<properties";
  let _ =
  match name_opt with
  | None -> out ">\n"
  | Some str' ->
  out " name=\""; out str'; out "\">\n"
  in
  List.iter
  (fun lp ->
  out "<property>";
  escape out (Rule.string_of_labelled_property lp);
  out "</property>\n")
  lp_seq;
  out "</properties>\n"
 *)

and print_path_in_xml out (name_opt, lr) =
  ()

and print_label_in_xml out l =
  out "<label name=\"";
  out l;
  out "\"/>\n"

and print_event_in_xml out ((str, opt) : event_spec) =
  out "<event name=\""; out str; out "\"";
  match opt with
  | None -> out "/>\n"
  | Some str' ->
      out ">\n<script>"; escape out str'; out "</script>\n";
      out "</event>\n"

and print_protocol_in_xml out ((name_opt, p) : protocol_spec) =
  out "<protocol";
  let _ =
    match name_opt with
    | None -> out ">"
    | Some (str', _) ->
	out " name=\""; out str'; out "\">"
  in
  escape out (Protocol.string_of_protocol p);
  out "</protocol>\n"

and print_rule_in_xml out ?(id = None) ((name_opt, r) : rule_spec) =
  out "<rule";
  let _ =
    match id with
    | None -> ()
    | Some str ->
	out " id=\""; out str; out "\""
  in ();

  let _ =
    match name_opt with
    | None -> out ">\n"
    | Some (str', _) ->
	out " name=\""; out str'; out "\"";
	out ">\n"
  in ();

  (* event *)
  let ev, ev_opt = r.event in
  let e = Rule.event_name ev in
  out "<event name=\""; out e; out "\"";
  let _ = 
    match ev_opt with
    | None -> out "/>\n"
    | Some s ->
	out ">";
	out "<script>"; escape out s; out "</script>";
	out "</event>\n";
  in ();

  (* condition *)
  let c, c_opt = r.condition in
  out "<condition><formula>";
  escape out (Property.string_of_labelled_property c);
  out "</formula>";
  let _ =
    match c_opt with
    | None -> ()
    | Some s -> out "<script>"; escape out s; out "</script>"
  in
  out "</condition>\n";

  (* action *)
  let acts = r.action in
  out "<action>";
  let scripts : string list =
    List.fold_left
      (fun rslt (act, code_opt) ->
	match act with
	| Rule.Act_ensure p ->
	    out "<ensure>";
	    escape out (Property.string_of_labelled_property (p, None));
	    out "</ensure>";
	    rslt
	| Rule.Act_raise [e] ->
	    out "<raise event=\""; out e; out "\"/>";
	    rslt @ (match code_opt with None -> [] | Some code -> [code])
	| Rule.Act_raise es when List.length es > 1 ->
	    out "<choice>";
	    List.iter (fun e -> out @@ "<raise event=\"" ^ e ^ "\"/>") es;
	    out "</choice>";
	    rslt @ (match code_opt with None -> [] | Some code -> [code])
	| Rule.Act_do ->
	    rslt @ (match code_opt with None -> [] | Some code -> [code])
	| _ -> rslt)
      [] acts in
(*
  match a_seq with
  | [] -> ()
  (*| [a] -> out "<path>"; escape out (Rule.string_of_action a); out "</path>"*)
  | a :: a_seq' ->
  out "<path>";
  escape out (Rule.string_of_action a);
  List.iter (fun a -> out "; "; escape out (Rule.string_of_action a)) a_seq';
  out "</path>"
 *)
  out "<script>";
  List.iter (escape out) scripts;
  out "</script>";
  out "</action>\n";

  (* path *)
(*
  let _ =
  match r.path with
  | None -> ()
  | Some lp ->
  out "<path>"; escape out (Rule.string_of_labelled_path lp); out "</path>\n";
  in ();
 *)
  out "</rule>\n"
(*
  match rs with
  | [r] ->
  | _ when name_opt = None ->
  List.iter	(fun r -> print_rule_in_xml out (None, [r])) rs;
  | _ ->
  out "<rules";
  let _ =
  match name_opt with
  | None -> out ">\n"
  | Some str' ->
  out " name=\""; out str'; out "\">\n";
  in
  List.iter	(fun r -> print_rule_in_xml out (None, [r])) rs;
  out "</rules>\n"
 *)

(*
  let rname_opt, _ = r in
  out "<rule";
  (*if rname <> "" then (out " name=\""; out rname; out "\"");*)
  out ">\n";
  out "<event/>\n";

  out "<condition>\n";
  out "<proposition>"; out ""; out "</proposition>\n";
(*
  if List.mem_assoc pname pspecs then
  (match List.assoc pname pspecs with
  | None -> ()
  | Some exp ->
  out "<expression><script>";
  List.iter out (escape exp);
  out "</script></expression>\n");
 *)
  out "</condition>\n";

  out "<action><script>";
  (*List.iter out (escape act);*)
  out "</script></action>\n";
  out "</rule>\n"
 *)

(*
  let rec string_of_spec s =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  print_spec concat s;
  !str

  and print_spec out (s : spec) =
  failwith "not yet implemented"

  let rec print_rules_in_xml out (s : spec) =
  let pspecs =
  List.fold_left
  (fun rslt -> function
  |	Decl_proposition pspecs -> rslt @ pspecs
  | _ -> rslt)
  [] s
  and rspecs =
  List.fold_left
  (fun rslt -> function
  | Decl_rule rspecs -> rslt @ rspecs
  | _ -> rslt)
  [] s
  in
  out "<rules>\n";
  List.iter (print_rule_in_xml out pspecs) rspecs;
  out "</rules>\n"

  and print_rule_in_xml out (pspecs : proposition_spec list) (r : rule_spec) =
  let rname_opt, _ = r in
  out "<rule";
  (*if rname <> "" then (out " name=\""; out rname; out "\"");*)
  out ">\n";
  out "<event/>\n";

  out "<condition>\n";
  out "<proposition>"; out ""; out "</proposition>\n";
(*
  if List.mem_assoc pname pspecs then
  (match List.assoc pname pspecs with
  | None -> ()
  | Some exp ->
  out "<expression><script>";
  List.iter out (escape exp);
  out "</script></expression>\n");
 *)
  out "</condition>\n";

  out "<action><script>";
  (*List.iter out (escape act);*)
  out "</script></action>\n";
  out "</rule>\n"

 *)

and print_variable_in_xml out (((name, ty), init_opt) : variable_spec) =
  assert (name <> "");
  out "<variable name=\"";
  out (if name <> "" then name else invalid_arg "variable with no name");
  out "\"";
  let _ =
    match ty with
    | VT_prop ->
	out " type=\"prop\""
    | VT_term (Ty_nat n) ->
	out (Printf.sprintf " type=\"nat(%d)\"" n)
  in ();
  match init_opt with
  | None -> out "/>\n"
  | Some str -> out ">"; out str; out "</variable>\n"
