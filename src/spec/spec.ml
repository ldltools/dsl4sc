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
    { event_seq : string list;
      proto_seq : Protocol.t list;

      pvar_seq : string list;
      prop_seq : Property.labelled_property list;

      rule_seq : Rule.t list;
    }

let rec rules_to_spec (rules : Rules.t) =
  { event_seq = List.map fst rules.event_decls;
    proto_seq = List.map snd rules.proto_decls;

    pvar_seq = List.map (fun ((x, t), _) -> x) rules.var_decls;
    prop_seq = List.map snd rules.prop_decls;

    rule_seq = List.map filter_rule_spec rules.rule_decls;
  }

and filter_rule_spec (rspec : Rules.rule_spec) =
  let _, r = rspec in
  let acts' =
    List.fold_left
      (fun rslt (act, code_opt) ->
	match act with
	| Rule.Act_do -> rslt
	| _ -> rslt @ [act, code_opt])
      [] r.action
  in { event = r.event; condition = r.condition; action = acts'; }
