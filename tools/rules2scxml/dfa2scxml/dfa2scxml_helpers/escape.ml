(* $Id:$ *)
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

(* escape *)
(* <,>,& => &lt;,&gt;,&amp; *)
let rec escape out (str : string) =
  escape_rec out str 0 0 (String.length str)

and escape_rec out str prev curr len =
  let tab = ['"', "&quot;"; '&', "&amp;"; '\'', "&apos;"; '<', "&lt;"; '>', "&gt;"] in
  if curr >= len then out (String.sub str prev (curr - prev)) else
  if List.mem_assoc str.[curr] tab
  then
    (out (String.sub str prev (curr - prev));
     out (List.assoc str.[curr] tab);
     escape_rec out str (curr + 1) (curr + 1) len)
  else
    escape_rec out str prev (curr + 1) len

(* unescape *)
let rec unescape out (str : string) =
  unescape_rec out str 0 0 (String.length str)

and unescape_rec out str prev curr len =
  let tab = ["&quot;", "\""; "&amp;", "&"; "&apos;", "'"; "&lt;", "<"; "&gt;", ">"] in
  if curr >= len then out (String.sub str prev (curr - prev)) else
  if str.[curr] = '&'
  then
    match String.index_from_opt str (curr + 1) ';' with
    | Some i when i < curr + 6 ->
	assert (i > curr && str.[i] = ';');
	let entity = String.sub str curr (i - curr + 1) in
	if List.mem_assoc entity tab
	then
	  (out (String.sub str prev (curr - prev));
	   out (List.assoc entity tab);
	   unescape_rec out str (i + 1) (i + 1) len)
	else
	  unescape_rec out str prev (curr + 1) len
    | _ -> unescape_rec out str prev (curr + 1) len
  else
    unescape_rec out str prev (curr + 1) len

let synopsis prog =
  Printf.printf "usage: %s [--unescape] <infile>\n" (Filename.basename prog)

(* *)
let main argc argv =
  let infile = ref "/dev/stdin" and outfile = ref "/dev/stdout" in
  let mode = ref "escape" in
  let i = ref 1 in
  while !i < argc do
    let _ =
      match argv.(!i) with
      | "-" ->
	  infile := "/dev/stdin";
      | "-o" | "--output" ->
	  outfile := argv.(!i+1); incr i;

      | "-u" | "--unescape" ->
	  mode := "unescape"

      | "-h" | "--help" ->
	  synopsis argv.(0); exit 0
      | _ ->
	  infile :=argv.(!i)
    in incr i
  done;
    
  let ic = open_in !infile in
  let str = ref "" in
  let _ =
    try
      while true do str := !str ^ (input_line ic) ^ "\n" done
    with
    | End_of_file -> () in
  close_in ic;

  let oc = open_out !outfile in
  (*output_string oc !str;*)
  
  let out str = output_string oc str; flush_all () in
  if !mode = "escape"
  then escape out !str
  else unescape out !str;
  close_out oc
;;

assert (not !Sys.interactive);
main (Array.length Sys.argv) Sys.argv
