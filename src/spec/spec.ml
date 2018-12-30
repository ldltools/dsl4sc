(* $Id: $ *)
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

type t =
    { events : string list;
      protocols : Protocol.t list;

      variables : (string * Rules.variable_type) list;
      properties : Property.t list;

      rules : Rule.t list;
    }

(** ctor *)

let rec spec_of_rules (rules : Rules.t) =
  { events = List.map fst rules.events;
    protocols = rules.protocols;

    variables = List.map (function ((x, ty), _) -> x, ty) rules.variables;
    properties = rules.properties;

    rules = List.map filter_rule_spec rules.rules;
  }

and filter_rule_spec (rspec : Rules.rule_spec) =
  let r, _ = rspec in
  let acts' =
    List.fold_left
      (fun rslt (act, code_opt) ->
	match act with
	| Rule.Act_do -> rslt
	| _ -> rslt @ [act, code_opt])
      [] r.action
  in { event = r.event; condition = r.condition; action = acts'; }

(** pretty-printing *)

let print_spec out (s : t) =
  (* event *)
  if s.events <> [] then
    begin
      out "event ";
      out (List.hd s.events);
      List.iter	(function e -> out ", "; out e) (List.tl s.events);
      out ";\n";
    end;

  (* protocol *)
  if s.protocols <> [] then
    begin
      out "protocol\n";
      List.iter
	(fun p -> out " "; Protocol.print_protocol out p; out ";;\n")
	s.protocols;
    end;

  (* variable *)
  if s.variables <> [] then
    begin
      out "variable\n";
      List.iter
	(function
	  | x, Rules.VT_prop ->
	      out " "; out x;
	      out " : prop;\n"
	  | x, Rules.VT_term (Property.Ty_nat n) ->
	      out " "; out x;
	      out " : nat ("; out (string_of_int n); out ");\n")
	s.variables;
    end;

  (* propery *)
  if s.properties <> [] then
    begin
      out "property\n";
      List.iter
	(fun f -> out " "; Property.print_property out f; out ";\n")
	s.properties;
    end;

  (* rule *)
  if s.rules <> [] then
    begin
      out "rule\n";
      List.iter (fun r -> out " "; Rule.print_rule out r; out "\n") s.rules;
    end;

  ()

let string_of_spec (s : t) =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  print_spec concat s;
  !str
