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

open Dsl4sc
open Printf

module Ldl = Ldlsat.Ldl

let stdin = open_in "/dev/stdin"
let opt_o = ref "unspecified"
let opt_o_channel = ref stdout
let opt_fmt_in = ref "unspecified"
let opt_fmt_out = ref "unspecified"
let opt_map_out = ref "/dev/null"
let opt_verbose = ref false

let opt_until = ref "ldl"
let opt_parse_only = ref false

let opt_skip_rulespp = ref false
let opt_skip_specpp = ref false
let opt_skip_p18n = ref false
let opt_keep_terms = ref false
let opt_skip_ldlgen = ref false

let opt_strict = ref false

let synopsis prog =
  printf "%s (version %s)\n" (Filename.basename prog) (Version.get ());
  printf "usage: %s <option>* <rules_file>\n" (Filename.basename prog);
  List.iter (output_string stdout)
    ["options:\n";
     "  -o <file>\t\toutput to <file>\n";
     "  -t <fmt>\t\toutput in <fmt> (\"caml\", \"json\")\n";
     "  --map <file>\t\toutput event mappings to <file> in xml\n";
     "  -p\t\t\tparse-only\n";
     "  -V, --version\t\tdisplay version\n";
     "  -h, --help\t\tdisplay this message\n"]

let extra_synopsis () =
  List.iter (output_string stdout)
    ["\n";
     "  [advanced/experimental options]\n";
     "  --until <stage>\tterminate at <stage> (\"rules\", \"spec\", \"ldl\")\n";
     "  --skip-rulespp\tskip rules-preprocessing\n";
     "  --skip-specpp\t\tskip spec-preprocessing\n";
     "  --skip-p18n\t\tskip propositionalization\n";
     "  --keep-terms\t\tkeep terms in p18n\n";
     "  --skip-ldlgen\t\tskip ldl-generation\n";
     "\n"]

(* rules in/out *)
let input_rules ic = function
  | "rules" | "unspecified" ->
      (*Rules_p.decl_seq Rules_l.token (Lexing.from_channel ic)*)
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

(* spec in *)
let rec input_spec ic = function
  | "json" ->
      let json =  Yojson.Safe.from_channel ic in
      (match Rules.rules_of_yojson json with
	Ok f -> f | Error msg -> failwith msg)
  | "unspecified" ->
      let lst = ref [] in
      let _ =
	try
	  while true do
	    let str = Bytes.make 1024 (Char.chr 0) in
	    lst := !lst @ [str];
	    really_input ic str 0 1024;
	  done
	with End_of_file -> () in
      let str' = Bytes.concat "" !lst in
      let str' = Bytes.sub str' 0 (Bytes.index str' (Char.chr 0)) in
      spec_from_string str' "unspecified"
  | fmt ->
      failwith ("input_spec: unknown format (" ^ fmt ^ ")")

and spec_from_string str = function
  | "spec" | "rules" ->
      (*Rules_p.rules Rules_l.token (Lexing.from_string str)*)
      let lbuf : Rules_l.lexbuf = Rules_l.create_lexbuf (Sedlexing.Utf8.from_string str)
      in Rules_l.parse Rules_p.rules lbuf
  | "json" ->
      let json =  Yojson.Safe.from_string str in
      (match Rules.rules_of_yojson json with
	Ok f -> f | Error msg -> failwith msg)
  | "unspecified" when Bytes.length str < 6 ->
      spec_from_string str "spec"
  | "unspecified" ->
      let ch = (Bytes.sub str 0 1).[0] in
      if ch = '[' || ch = '{' then
	spec_from_string str "json"
      else
	spec_from_string str "spec"
  | fmt ->
      failwith ("spec_from_string: unknown format (" ^ fmt ^ ")")

(* spec out *)
(*
let output_spec oc (s : Rules.t) = function
  | "spec" | "rules" ->
      Rules.print_rules (fun s -> output_string oc s; flush_all ()) s
  | "caml" ->
      output_string oc (Rules.show_rules s);
      output_string oc "\n";
  | "json" | "unspecified" ->
      let json = Rules.rules_to_yojson s in
      Yojson.Safe.to_channel oc json;
      output_string oc "\n"
  | fmt ->
      failwith ("output_spec: unknown format (" ^ fmt ^ ")")
 *)
let output_spec oc (s : Spec.t) = function
  | "spec" | "rules" | "unspecified" ->
      Spec.print_spec (output_string oc) s
(*
  | "caml" ->
      output_string oc (Spec.show_spec s);
      output_string oc "\n";
  | "json" ->
      let json = Rules.rules_to_yojson s in
      Yojson.Safe.to_channel oc json;
      output_string oc "\n"
 *)
  | fmt ->
      failwith ("output_spec: unknown format (" ^ fmt ^ ")")

(* ldl out *)
let output_formula oc (f : Ldl.formula) = function
  | "ldl" | "unspecified" ->
      Ldl.print_formula (fun s -> output_string oc s; flush_all ()) f;
      output_string oc "\n"
  | "caml" ->
      output_string oc (Ldl.show_formula f);
      output_string oc "\n";
  | "json" ->
      let json = Ldl.formula_to_yojson f in
      Yojson.Safe.to_channel oc json;
      output_string oc "\n"
  | fmt ->
      failwith ("output_formula: unknown format (" ^ fmt ^ ")")

(* map out *)
let output_map oc (m : Spec2ldl.event_map) = function
  | "xml" | "unspecified" ->
      output_string oc "<mappings xmlns=\"https://github.com/ldltools/dsl4sc\">\n";
      List.iter
	(function
	  | e, Property.Prop_conj fs ->
	      fprintf oc "<bits type=\"event\" name=%S>" e;
	      List.iter
		(fun f -> fprintf oc "<bit>%s</bit>" (Property.string_of_property f))
		fs;
	      fprintf oc "</bits>\n"
	  | _ -> ())
	m;
      output_string oc "</mappings>\n"
  | fmt ->
      failwith ("output_map: unknown format (" ^ fmt ^ ")")

(* *)
let main argc argv =
  let i = ref 1
  and infile = ref "/dev/stdin"
  and outfile = ref "/dev/stdout"
  in
  while !i < argc do
    let _ =
      match argv.(!i) with
      | "-" ->
	  infile := "/dev/stdin";
      | "-o" | "--output" ->
	  outfile := argv.(!i+1); incr i;
      | "--map"  ->
	  opt_map_out := argv.(!i+1); incr i;

      | "-s" ->
	  opt_fmt_in := argv.(!i+1); incr i;
      | "--json" ->
	  opt_fmt_in := "json"
      | "-t" ->
	  opt_fmt_out := argv.(!i+1); incr i;

      | "-V" | "--version" ->
	  printf "%s\n" (Version.get ());
	  raise Exit
      | "-v" | "--verbose" ->
	  opt_verbose := true
      | "-q" | "--silent" ->
	  opt_verbose := false
      | "-h" | "--help"  ->
	  synopsis argv.(0); exit 0
      | "-hh" ->
	  synopsis argv.(0); extra_synopsis (); exit 0

      | "-p" | "--parse-only" ->
	  opt_parse_only := true
      | "-E" ->
	  opt_until := "rules"
      | "-u" | "--until"  ->
	  let stage = argv.(!i+1) in
	  opt_until := stage; incr i;

      (* rulespp *)
      | "--skip-rulespp" | "--no-rulespp" ->
	  opt_skip_rulespp := true
      (* specpp *)
      | "--skip-specpp" | "--no-specpp" ->
	  opt_skip_specpp := true
      (* spec2ldl *)
      | "--skip-p18n" | "--no-p18n" ->
	  opt_skip_p18n := true
      | "--keep-terms" ->
	  opt_keep_terms := true
      | "--skip-ldlgen" | "--no-ldlgen" ->
	  opt_skip_ldlgen := true

      | "--strict" ->
	  opt_strict := true

      | _  when argv.(!i).[0] = '-' ->
	  failwith ("unknown option: " ^ argv.(!i))
      | _    ->
	  infile :=argv.(!i)
    in incr i
  done;

  (* input *)
  if not (Sys.file_exists !infile) then
    (synopsis argv.(0); invalid_arg ("file does not exist: '" ^ !infile ^ "'"));

  (* output *)
  let oc = open_out !outfile

  (* parse a set of declarations *)
  in let ic = open_in !infile
  in let decls : Rules.decl list = input_rules ic !opt_fmt_in in
  if !opt_parse_only then
    (output_rules oc (Rules.decls_to_rules decls) !opt_fmt_out; raise Exit);

  (* rulespp: decls -> decls *)
  let decls =
    if !opt_skip_rulespp
    then decls
    else Rulespp.preprocess
	~allow_undeclared: (not !opt_strict)
	~discard_codes: true
	decls
  in

  (* decls -> rules *)
  let rules : Rules.t = Rules.decls_to_rules decls
  in
  if !opt_until = "rules" then (output_rules oc rules !opt_fmt_out; raise Exit);

  (* ensure rules include no code *)
  List.iter
    (function
      | (r : Rule.t), None
      | r, Some "_r_preserve" ->
	  let (e, e_opt), (_, c_opt), a = r.event, r.condition, r.action in
	  (match e_opt, c_opt with
	  | Some _, _ | _, Some _ ->
	      failwith ("[rules2ldl] rule for event (" ^ (Rule.event_name e) ^ ") carries code")
	  | _ -> ())
      | _, Some annot ->
	  failwith ("[rules2ldl] annotated rule (" ^ annot ^ ") not allowed")
      | _ -> ())
    rules.rules;

  (* rules -> spec *)
  let spec : Spec.t = Spec.spec_of_rules rules in

  (* specpp: spec -> spec *)
  let spec =
    if !opt_skip_specpp
    then spec
    else Specpp.preprocess spec
  in
  if !opt_until = "spec" then (output_spec oc spec !opt_fmt_out; raise Exit);

  (* spec -> property list *)
  let props, map =
    Spec2ldl.translate
      ~propositionalize: (not !opt_skip_p18n)
      ~keep_terms: !opt_keep_terms
      spec
  in
  if !opt_map_out != "/dev/null" then
    (let oc_map = open_out !opt_map_out in
     output_map oc_map map "unspecified"; close_out oc_map);
  if !opt_skip_ldlgen || !opt_until <> "ldl" then
    (output_string oc "property\n";
     List.iter
       (fun p -> Property.print_property (output_string oc) p; output_string oc ";\n")
       props;
     raise Exit);

  (* print out *)
  assert (!opt_until = "ldl");
  let f : Ldl.formula = Spec2ldl.formula_of_property (Property.Prop_conj props) in
  output_formula oc f !opt_fmt_out;

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
