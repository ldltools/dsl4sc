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

open Dsl4sc
open Printf

let stdin = open_in "/dev/stdin"
let opt_o = ref "unspecified"
let opt_o_channel = ref stdout
let opt_fmt_in = ref "unspecified"
let opt_fmt_out = ref "unspecified"
let opt_verbose = ref false
let opt_parse_only = ref false

(* variables *)
let opt_allow_undeclared = ref true
(* protocol *)
let opt_expand_any = ref true
let opt_proto_min = ref 1
let opt_relax_protocols = ref false
(* rule *)
let opt_expand_preserve = ref true
let opt_discard_codes = ref false

(* deprecated *)
(*
let opt_mark_conditions = ref false
let opt_split_cases = ref true
let opt_propositionalize = ref true
let opt_extra_properties = ref true
let opt_expand_macros = ref false
let opt_expand_arrays = ref false
let opt_add_undeclared = ref true
let opt_align_propositions = ref true
let opt_apply_interleaving = ref false
let opt_label2proposition = ref true
let opt_allow_skip = ref false
*)

let synopsis prog =
  printf "%s (version %s)\n" (Filename.basename prog) (Version.get ());
  printf "usage: %s <option>* <rules_file>\n" (Filename.basename prog);
  List.iter (output_string stdout)
    ["options:\n";
     "  -o <file>\t\toutput to <file>\n";
     "  -t <fmt>\t\toutput in <fmt> (\"caml\", \"json\", \"xml\")\n";
     "  -p\t\t\tparse-only\n";
     "  -V, --version\t\tdisplay version\n";
     "  -h, --help\t\tdisplay this message\n"]

let extra_synopsis () =
  List.iter (output_string stdout)
    ["\n";
     "  [advanced/experimental options]\n";
     "  --force-proto-min\tforce protocol minimization\n";
     "  --relax-proto\t\tinsert intermediate _skip transitions\n";
     "  --keep-preserve\tkeep preserve-rules as they are\n";
     "  --discard-codes\tdiscard codes carried by rules\n";
     "\n"]

let input_rules ic = function
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
  and outfile = ref "/dev/stdout"
  in
  while !i < argc do
    let len = String.length argv.(!i)
    in let matches min_len str =
      len >= min_len && len <= String.length str && argv.(!i) = String.sub str 0 len
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

      | "-V" | "--version" ->
	  printf "%s\n" (Version.get ());
	  raise Exit
      | "-v" | "--verbose" ->
	  opt_verbose := true
      | "-q" | "--silent" ->
	  opt_verbose := false
      | "-h" | "--help" ->
	  synopsis argv.(0); exit 0
      | "-hh" ->
	  synopsis argv.(0); extra_synopsis (); exit 0

      | "-p" | "--parse-only"  ->
	  opt_parse_only := true

      (* variables *)
      | _ when matches 11 "--no-undeclared" ->
	  opt_allow_undeclared := false
      (* protocol *)
      | _ when matches 7 "--relax-protocols" ->
	  opt_relax_protocols := true
      | _ when matches 7 "--force-proto_min" ->
	  opt_proto_min := 2
      (* property *)
      (* rule *)
      | _ when matches 8 "--keep-preserve" ->
	  opt_expand_preserve := false
      | _ when matches 9 "--discard-codes" ->
	  opt_discard_codes := true
      | _ when matches 9 "--no-discard-codes" ->
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
  let ic = open_in !infile
  in let decls : Rules.decl list = input_rules ic !opt_fmt_in
  in
  if !opt_parse_only then
    (output_rules oc (Rules.decls_to_rules decls) !opt_fmt_out; raise Exit);

  (* decls -> decls' *)
  let decls' =
    Rulespp.preprocess
      ~allow_undeclared: !opt_allow_undeclared
      ~expand_any: !opt_expand_any
      ~minimize_protocols: !opt_proto_min (* 0: no_min, 1: min (if '?' included), 2: min (always) *)
      ~relax_protocols: !opt_relax_protocols
      ~discard_codes: !opt_discard_codes
      ~expand_preserve: !opt_expand_preserve

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
| Invalid_argument s ->
    flush_all ();
    eprintf ";; Invalid argument: %s\n" s;
    exit 1
| Not_found ->
    flush_all ();
    eprintf ";; Something seems missing!\n";
    exit 1;
| End_of_file ->
    flush_all ();
    eprintf ";; Unexpected end of file\n";
    exit 1;
