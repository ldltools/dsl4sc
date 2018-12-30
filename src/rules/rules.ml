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
    { events : event_spec list;
      protocols : Protocol.protocol list;

      variables : variable_spec list;
      properties : Property.property list;

      rules : rule_spec list;
      extras : string list;
    }

and event_spec =
    string * string option
      (** event_name with optional expression *)

and variable_spec =
    (string * variable_type) * string option
      (** (var_name, var_type) with optional expression *)

and variable_type =
  | VT_prop			(* proposition type *)
  | VT_term of Property.base_t	(* term type *)

and rule_spec =
    Rule.rule * string option
      (** rule with optional annotation *)

[@@deriving show, yojson]

type t = rules

(** for parsing *)

type decl =
  (* protocol *)
  | Decl_event of event_spec
  | Decl_protocol of Protocol.t

  (* property *)
  | Decl_variable of variable_spec
  | Decl_property of Property.t

  (* eca rule *)
  | Decl_rule of rule_spec

  (* extra *)
  | Decl_extra of string

(* decls to rules *)
let rec decls_to_rules ?(event_sort = true) (decls : decl list) =
  let events, protos, vars, props, rules, impls =
    List.fold_left decls_to_rules_rec ([], [], [], [], [], []) decls
  in
  { events =
      if event_sort then List.sort_uniq (fun (e1, _) (e2, _) -> compare e1 e2) events else events;
    protocols = protos;

    variables = vars;
    properties = props;

    rules = rules;
    extras = impls;
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
  | Decl_extra impl ->
      (events, protos, vars, props, rules, impls @ [impl])

  | _ -> failwith "decls_to_rules_rec"

(** pretty-printing *)

let print_rules out (rs : t) =
  let filter f = List.fold_left (fun rslt decl -> rslt @ f decl) [] in

  (* event *)
  let es : string list = List.map fst rs.events in
  if es <> [] then
    begin
      out "event ";
      out (List.hd es);
      List.iter	(function e -> out ", "; out e) (List.tl es);
      out ";\n";
    end;

  (* protocol *)
  if rs.protocols <> [] then
    begin
      out "protocol\n";
      List.iter
	(fun p -> out " "; Protocol.print_protocol out p; out ";;\n")
	rs.protocols;
    end;

  (* variable *)
  if rs.variables <> [] then
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
	  | (x, _), _ -> failwith ("[print_rules] strange variable: " ^ x))
	rs.variables;
    end;

  (* propery *)
  if rs.properties <> [] then
    begin
      out "property\n";
      List.iter
	(fun p -> out " "; Property.print_property out p; out ";\n")
	rs.properties;
    end;

  (* rule *)
  if rs.rules <> [] then
    begin
      out "rule\n";
      List.iter
	(fun (r, _) -> out " "; Rule.print_rule out r; out "\n")
	rs.rules;
    end;

  (* extra *)
  if rs.extras <> [] then
    begin
      out "script\n";
      out "{\n";
      List.iter out rs.extras;
      out "}\n";
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
  out "<system xmlns=\"https://github.com/ldltools/dsl4sc\">\n";

  print "events"
    rules.events
    print_event_in_xml;
  print "protocols"
    rules.protocols
    print_protocol_in_xml;

  print "variables"
    rules.variables
    print_variable_in_xml;
  print "properties"
    rules.properties
    print_property_in_xml;

  out "<rules>\n";
  (* rule -- strip off the special "_skip" rule and its successors.
     cf. Rulespp.preprocess
   *)
  let r_specs =
    List.filter
      (function _, Some "_r_preserve" -> false | _ -> true)
      rules.rules
  in
  let _ =
    List.fold_left
      (fun i r_spec ->
	print_rule_in_xml out ~id: (Some ("r" ^ (string_of_int i))) r_spec;
	i + 1)
      1 r_specs
  in
  out "</rules>\n";

  print "scripts"
    rules.extras
    print_script_in_xml;

  out "</system>\n"

and print_property_in_xml out p =
  out "<property>";
  escape out (Property.string_of_property p);
  out "</property>\n";

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

and print_protocol_in_xml out (p : Protocol.t) =
  out "<protocol>";
  escape out (Protocol.string_of_protocol p);
  out "</protocol>\n"

and print_script_in_xml out (str : string) =
  out "<script>";
  escape out str;
  out "</script>\n"

and print_rule_in_xml out ?(id = None) ((r, annot_opt) : rule_spec) =
  out "<rule";
  let _ =
    match id with
    | None -> ()
    | Some str ->
	out " id=\""; out str; out "\""
  in ();

  let _ =
    match annot_opt with
    | None -> out ">\n"
    | Some str' ->
	out " annotation=\""; out str'; out "\"";
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
  let (c, _), c_opt = r.condition in
  out "<condition><formula>";
  escape out (Property.string_of_property @@ Property.propositionalize c);
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
	    escape out (Property.string_of_property @@ Property.propositionalize p);
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

  out "<script>";
  List.iter (escape out) scripts;
  out "</script>";
  out "</action>\n";

  out "</rule>\n"

and print_variable_in_xml out (vspec : variable_spec) =
print_variable_in_xml_rec out [vspec]

and print_variable_in_xml_rec out (vspec_seq : variable_spec list) =
  if vspec_seq = [] then () else
  let ((x, ty), init_opt) :: rest = vspec_seq
  in
  assert (x <> "");
  if x = "" then invalid_arg "[print_variable_in_xml] variable with no name";
  out ("<variable name=\"" ^ x ^ "\"");
  let rest' =
    match ty with
    | VT_prop ->
	out " type=\"prop\"";
	rest
    | VT_term Ty_bool ->
	out " type=\"bool\"";
	rest
    | VT_term (Ty_nat n) ->
	let props = Property.term_to_propositions (Tm_var (x, Ty_nat n))
	in let nbit = List.length props
	in
	(* print x *)
	out (Printf.sprintf " type=\"nat%d\" max=\"%d\"" nbit (n - 1));
	(* print new variables introduced by propositionalization of x *)
	(List.map (fun p -> (p, VT_prop), None) props) @ rest
  in let _ =
    match init_opt with
    | None -> out "/>\n"
    | Some str -> out ">"; out str; out "</variable>\n"
  in
  print_variable_in_xml_rec out rest'
