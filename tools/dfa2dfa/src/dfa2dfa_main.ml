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

let synopsis prog =
  printf "usage: %s <dfa_file>\n" (Filename.basename prog)

let input_nfa ic = function
  | "xml" | "unspecified" ->
      Ldllts.read_in ic
  | fmt ->
      failwith ("input_spec: unknown format (" ^ fmt ^ ")")

(* *)
let main argc argv =
  let i = ref 1
  and infile = ref "/dev/stdin"
  and outfile = ref "/dev/stdout" in
  while !i < argc do
    let _ =
      match argv.(!i) with
      | "-" ->
	  infile := "/dev/stdin";
      | "-o" | "--output" ->
	  outfile := argv.(!i+1); incr i;
      | "-t" ->
	  opt_fmt_out := argv.(!i+1); incr i;

      | "-v" | "--verbose" ->
	  opt_verbose := true
      | "-q" | "--silent" ->
	  opt_verbose := false
      | "-h" | "--help"  ->
	  synopsis argv.(0); exit 0

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

  (* verbosity *)
  Ldllts.verbose := if !opt_verbose then 1 else 0;

  (* read dfa (in xml) from file into m *)
  let ic = open_in !infile in
  let alist, (m : Ldllts.t), (rs : Ldllts.rule list) = Ldllts.read_in ic in
  if !opt_verbose then
    (eprintf "** parsed from %S\n" !infile;
     Ldllts.debug_print m; List.iter Ldllts.debug_print_rule rs);

  (* update m to m' *)
  let m', (alist' : (string * string list) list) = Ldllts.update m rs in
  if !opt_verbose then
    (eprintf "** updated\n";
     Ldllts.debug_print m';
     List.iter
       (fun (rid, tid_seq) ->
	 eprintf "%s:" rid;
	 List.iter (eprintf " %s") tid_seq;
	 output_string stderr "\n")
       alist');

  (* output (in xml) *)
  let out s = output_string oc s in
  out "<dfa xmlns=\"https://github.com/ldltools/dsl4sc\">\n";
  out (Xml.to_string (List.assoc "variables" alist)); out "\n";
  Ldllts.print_states_in_xml out m;		(* states *)
  Ldllts.print_transitions_in_xml out m;	(* transitions *)
  (*out (Xml.to_string (List.assoc "variables" alist)); out "\n";*)
  Ldllts.print_rules_in_xml out m alist' rs;	(* rules *)
  if List.mem_assoc "implementation" alist then
    (out (Xml.to_string (List.assoc "implementation" alist)); out "\n");
  out "</dfa>\n";

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
