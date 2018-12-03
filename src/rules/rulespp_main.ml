(* $Id: rulespp_main.ml,v 1.1 2018/01/22 10:23:35 sato Exp $ *)
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

open Printf

let stdin = open_in "/dev/stdin"
let opt_o = ref "unspecified"
let opt_o_channel = ref stdout
let opt_fmt_in = ref "unspecified"
let opt_fmt_out = ref "unspecified"
let opt_verbose = ref false
let opt_parse_only = ref false

(* protocol *)
let opt_relax_protocols = ref false
(* property *)
let opt_split_cases = ref true
let opt_propositionalize = ref true
let opt_extra_properties = ref true
(* rule *)
let opt_discard_codes = ref false

(* deprecated *)
(*
let opt_expand_macros = ref false
let opt_expand_arrays = ref false
let opt_add_undeclared = ref true
let opt_align_propositions = ref true
let opt_apply_interleaving = ref false
let opt_label2proposition = ref true
let opt_allow_skip = ref false
*)

let synopsis prog =
  printf "usage: %s <option>* <rules_file>\n" (Filename.basename prog);
  let msg =
    "options:\n"
    ^ "  -o <file>\t\toutput to <file>\n"
    ^ "  -t <fmt>\t\toutput rules in <fmt> (rules, caml, json, xml)\n"
    ^ "  -p\t\t\tparse-only\n"
    ^ "  -h\t\t\tdisplay this message\n"
  in output_string stdout msg

let rec input_rules ic = function
  | "rules" | "unspecified" ->
      let lbuf : Rules_l.lexbuf = Rules_l.create_lexbuf (Sedlexing.Utf8.from_channel ic)
      in Rules_l.parse Rules_p.decl_seq lbuf
  | fmt ->
      failwith ("input_spec: unknown format (" ^ fmt ^ ")")

let output_rules oc (rs : Rules.t) = function
  | "rules" | "unspecified" ->
      Rules.print_rules (fun s -> output_string oc s; flush_all ()) rs
  | "caml" ->
      output_string oc (Rules.show_rules rs);
      output_string oc "\n";
  | "json" ->
      let json = Rules.rules_to_yojson rs in
      Yojson.Safe.to_channel oc json;
      output_string oc "\n"
  | "xml" ->
      Rules.print_rules_in_xml (fun s -> output_string oc s; flush_all ()) rs
  | fmt ->
      failwith ("output_spec: unknown format (" ^ fmt ^ ")")

(* *)
let main argc argv =
  let i = ref 1
  and infile = ref "/dev/stdin"
  and outfile = ref "/dev/stdout" in
  while !i < argc do
    let len = String.length argv.(!i)
    in let _ =
      match argv.(!i) with
      | "-" ->
	  infile := "/dev/stdin";
      | "-o" | "--output" ->
	  outfile := argv.(!i+1); incr i;
      | "-t" ->
	  opt_fmt_out := argv.(!i+1); incr i;
      | "--json" ->
	  opt_fmt_in := "json"

      | "-v" | "--verbose" ->
	  opt_verbose := true
      | "-q" | "--silent" ->
	  opt_verbose := false
      | "-h" | "--help"  ->
	  synopsis argv.(0); exit 0

      | "-p" | "--parse-only"  ->
	  opt_parse_only := true

      (* protocol *)
      | _ when 6 < len && 0 <= compare "--relax-protocols" argv.(!i) ->
	  opt_relax_protocols := true
      (* property *)
      | _ when 6 < len && 0 <= compare "--no-split-cases" argv.(!i) ->
	  opt_split_cases := false
      | _ when 6 < len && 0 <= compare "--no-propositionalize" argv.(!i) ->
	  opt_propositionalize := false
      | _ when 6 < len && 0 <= compare "--no-extra-properties" argv.(!i) ->
	  opt_extra_properties := false
      (* rule *)
      | "--discard-codes" ->
	  opt_discard_codes := true
      | "--no-discard-codes" ->
	  opt_discard_codes := false

      | _  when argv.(!i).[0] = '-' ->
	  failwith ("unknown option: " ^ argv.(!i))

      | _ ->
	  infile :=argv.(!i)
    in incr i
  done;

  (* input *)
  if not (Sys.file_exists !infile) then
    (synopsis argv.(0); invalid_arg ("file does not exist: '" ^ !infile ^ "'"));

  (* output *)
  let oc = open_out !outfile in

  (* read a set of declarations from file *)
  let ic = open_in !infile in
  let decls : Rules.decl list = input_rules ic !opt_fmt_in in
  if !opt_parse_only then
    (output_rules oc (Rules.decls_to_rules decls) !opt_fmt_out; raise Exit);

  (* decls -> decls' *)
  let decls' =
    Rulespp.preprocess
      ~protocol_relax: !opt_relax_protocols
      ~case_split: !opt_split_cases
      ~propositionalize: !opt_propositionalize
      ~extra_properties: !opt_extra_properties
      ~code_discard: !opt_discard_codes
      decls in
  let rules : Rules.t = Rules.decls_to_rules decls' in
  output_rules oc rules !opt_fmt_out;

  (* clean-up *)
  ()
;;

(* toplevel *)
assert (not !Sys.interactive);
try
  main (Array.length Sys.argv) Sys.argv
with   
| Exit -> exit 0
| Failure s ->
    flush_all ();
    eprintf ";; Failure: %s\n" s;
    exit 1;
| Not_found ->
    flush_all ();
    eprintf ";; Something seems missing!\n";
    exit 1;
| End_of_file ->
    flush_all ();
    eprintf ";; Unexpected end of file\n";
    exit 1;
