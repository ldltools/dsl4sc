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
let opt_verbose = ref 0
let opt_parse_only = ref false

let synopsis prog =
  printf "%s (version %s)\n" (Filename.basename prog) (Version.get ());
  printf "usage: %s <dfa_file>\n" (Filename.basename prog);
  List.iter (output_string stdout)
    ["options:\n";
     "  -o <file>\t\toutput to <file>\n";
     "  -V, --version\t\tdisplay version\n";
     "  -h, --help\t\tdisplay this message\n"]

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
      | "-p" | "--parse-only" ->
	  opt_parse_only := true

      | "-V" | "--version" ->
	  printf "%s\n" (Version.get ());
	  raise Exit
      | "-v" | "--verbose" ->
	  opt_verbose := 1
      | "-vv" ->
	  opt_verbose := 2
      | "-q" | "--silent" ->
	  opt_verbose := 0
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

  (* verbosity *)
  Modelgen.verbosity_set !opt_verbose;

  (* read dfa (in xml) from file into m *)
  let ic = open_in !infile in
  let m : Model.t = Model.from_channel ic in

  let oc = open_out !outfile in at_exit (fun _ -> close_out oc);

  (* parse-only *)
  if !opt_parse_only then (Model.to_channel oc m; raise Exit);

  (* transformation *)
  let m' =
    Modelgen.restore_possible_worlds m |> Modelgen.split_transitions |> Modelgen.chart_rules
  in

  (* output *)
  Model.to_channel oc m';
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
