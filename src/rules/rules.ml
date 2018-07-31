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

      pvar_decls : proposition_spec list;
      var_decls : variable_spec list;
      prop_decls : property_spec list;

      rule_decls : rule_spec list;

      path_decls : path_spec list;	(* deprecated *)
      label_decls : string list;	(* deprecated *)
    }

(** event *)
and event_spec =
    string * string option

(** protocol *)
and protocol_spec =
    (string * string list) option * Rule.protocol

(** proposition *)
and proposition_spec =
    (* name, exp *)
    string * string option

(** variable *)
and variable_spec =
    (* (name, type), code *)
    (string * variable_type) * string option

and variable_type =
  | VT_bool
  | VT_range of int * int

(** property *)
and property_spec =
    (string * string list) option * Rule.labelled_property
    (* (name, args), property *)

(** rule *)
and rule_spec =
    (string * string list) option * Rule.rule
    (* (name, args), rule *)

(** path -- deprecated *)
and path_spec =
    string option * Rule.labelled_path

[@@deriving show, yojson]

type t = rules

(** for parsing *)

type decl =
  (* protocol *)
  | Decl_event of event_spec
  | Decl_protocol of protocol_spec

  (* property *)
  | Decl_proposition of proposition_spec
  | Decl_variable of variable_spec
  | Decl_property of property_spec

  (* eca rule *)
  | Decl_rule of rule_spec

  | Decl_path of path_spec	(* deprecated *)
  | Decl_label of string	(* deprecated *)


(* decls to rules *)
let rec decls_to_rules ?(event_sort = true) (decls : decl list) =
  let events, protos, pvars, vars, props, rules =
    List.fold_left decls_to_rules_rec ([], [], [], [], [], []) decls
  in
  { event_decls =
      if event_sort then List.sort (fun (e1, _) (e2, _) -> compare e1 e2) events else events;
    proto_decls = protos;
    pvar_decls = pvars; var_decls = vars; prop_decls = props;
    rule_decls = rules;
    path_decls = []; label_decls = [];	(* deprecated *)
  }

and decls_to_rules_rec (events, protos, pvars, vars, props, rules) = function
  | Decl_event ev ->
      (events @ [ev], protos, pvars, vars, props, rules)
  | Decl_protocol proto ->
      (events, protos @ [proto], pvars, vars, props, rules)

  | Decl_proposition pvar ->
      (events, protos, pvars @ [pvar], vars, props, rules)
  | Decl_variable var ->
      (events, protos, pvars, vars @ [var], props, rules)
  | Decl_property prop ->
      (events, protos, pvars, vars, props @ [prop], rules)

  | Decl_rule rule ->
      (events, protos, pvars, vars, props, rules @ [rule])

(*
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
      out " ;\n";
    end;

  (* protocol *)
  if rs.proto_decls <> [] then
    begin
      out "protocol\n";
      List.iter
	(function
	  | None, p ->
	      out " { "; Rule.print_protocol out p; out " }\n"
	  | Some (name, []), p ->
	      out " "; out name; out " ()";
	      out " { "; Rule.print_protocol out p; out " }\n"
	  | Some (name, arg :: args), p ->
	      out " "; out name;
	      out " ("; out arg; List.iter (fun arg -> out ", "; out arg) args; out ")";
	      out " { "; Rule.print_protocol out p; out " }\n")
	rs.proto_decls;
    end;

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

  (* propery *)
  if rs.prop_decls <> [] then
    begin
      out "property\n";
      List.iter
	(function
	  | None, p ->
	      out " { "; Rule.print_labelled_property out p; out " }\n"
	  | Some (name, []), p ->
	      out " "; out name; out " ()";
	      out " { "; Rule.print_labelled_property out p; out " }\n"
	  | Some (name, arg :: args), p ->
	      out " "; out name;
	      out " ("; out arg; List.iter (fun arg -> out ", "; out arg) args; out ")";
	      out " { "; Rule.print_labelled_property out p; out " }\n")
	rs.prop_decls;
    end;

  (* rule *)
  if rs.rule_decls <> [] then
    begin
      out "rule\n";
      List.iter
	(function
	  | None, r ->
	      out " { "; Rule.print_rule out r; out " }\n"
	  | Some (name, []), r ->
	      out " "; out name; out " ()";
	      out " { "; Rule.print_rule out r; out " }\n"
	  | Some (name, arg :: args), r ->
	      out " "; out name;
	      out " ("; out arg; List.iter (fun arg -> out ", "; out arg) args; out ")";
	      out " { "; Rule.print_rule out r; out " }\n")
	rs.rule_decls;
    end;

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
  out "<rules>\n";

  out "<preamble>\n";
  print "propositions"
    rules.pvar_decls
    print_proposition_in_xml;
  print "properties"
    rules.prop_decls
    print_property_in_xml;
  print "paths"
    rules.path_decls
    print_path_in_xml;
  print "labels"
    rules.label_decls
    print_label_in_xml;

  print "events"
    rules.event_decls
    print_event_in_xml;
  print "protocols"
    rules.proto_decls
    print_protocol_in_xml;
  out "</preamble>\n";

  (* rule -- strip off the special "_skip" rule and its successors.
     cf. Rulespp.preprocess
   *)
  let _, (r_specs : rule_spec list) =
    List.fold_left
      (fun (b, rslt) (rspec : rule_spec) ->
	if b then b, rslt else
	let special_rule : Rule.t = 
	  { event = Ev_name "_skip", None;
	    condition = (Prop_atomic "true", None), None;
	    action = (None, [Act_ensure (Prop_atomic "true")]), None;
	    path = None
	  } in
	let name_opt, (r : Rule.t) = rspec in
	assert (name_opt = None);
	if r = special_rule then true, rslt else b, rslt @ [rspec])
      (false, []) rules.rule_decls
  in
  List.iter (print_rule_in_xml out) r_specs;

  out "</rules>\n"

(*
  let pspecs =
    List.fold_left
      (fun rslt -> function
	|	Decl_proposition pspecs -> rslt @ pspecs
	| _ -> rslt)
      [] s
  and rspecs =
    List.fold_left
      (fun rslt -> function
	| Decl_rule (name_opt, (rs : Rule.rule list)) -> rslt @ rspecs
	| _ -> rslt)
      [] s
  in
*)

and print_proposition_in_xml out (str, opt) =
  out "<proposition variable=\""; out str; out "\"";
  match opt with
  | None -> out "/>\n"
  | Some str' ->
      out ">\n<script>"; escape out str'; out "</script>\n";
      out "</proposition>\n"

and print_property_in_xml out (name_opt, lp) =
  out "<property";
  let _ =
    match name_opt with
    | None -> out ">"
    | Some (str', _) ->
	out " name=\""; out str'; out "\">"
  in
  escape out (Rule.string_of_labelled_property lp);
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

and print_event_in_xml out (str, opt) =
  out "<event name=\""; out str; out "\"";
  match opt with
  | None -> out "/>\n"
  | Some str' ->
      out ">\n<script>"; escape out str'; out "</script>\n";
      out "</event>\n"

and print_protocol_in_xml out (name_opt, p) =
  out "<protocol";
  let _ =
    match name_opt with
    | None -> out ">"
    | Some (str', _) ->
	out " name=\""; out str'; out "\">"
  in
  escape out (Rule.string_of_protocol p);
  out "</protocol>\n"

and print_rule_in_xml out (name_opt, r) =
      out "<rule";
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
      escape out (Rule.string_of_labelled_property c);
      out "</formula>";
      let _ =
	match c_opt with
	| None -> ()
	| Some s -> out "<script>"; escape out s; out "</script>"
      in
      out "</condition>\n";

      (* action *)
      let a, a_opt = r.action in
      out "<action>";
      let a_path_opt, a_seq = a in
      let _ =
	match a_path_opt with
	| Some a_path ->
	    out "<path>";
	    escape out (Rule.string_of_labelled_path (a_path, None));
	    out "</path>"
	| _ -> () in
      List.iter
	(function
	  | Rule.Act_ensure p ->
	      out "<ensure>";
	      escape out (Rule.string_of_labelled_property (p, None));
	      out "</ensure>"
	  | Rule.Act_raise [e] ->
	      out "<raise event=\""; out e; out "\"/>";
	  | Rule.Act_raise es when List.length es > 1 ->
	      out "<choice>";
	      List.iter (fun e -> out @@ "<raise event=\"" ^ e ^ "\"/>") es;
	      out "</choice>";
	  | _ -> ())
	a_seq;
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
      let _ =
	match a_opt with
	| None -> ()
	| Some s -> out "<script>"; escape out s; out "</script>"
      in
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
