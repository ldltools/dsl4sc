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

open Ldl
open Printf

type rule =
    string * event * condition * action * condition
      (* (id, e, c, a, post) *)

and event =
    string * string option

and condition =
    Ldl.formula * string option
      (* (c, script) *)

and action =
    action_unit list * string option
      (* (a, script) *)

and action_unit =
  | Act_ensure of Ldl.formula
  | Act_raise of string
  | Act_raise_sum of string list

[@@deriving show]

type t = rule

let verbose = ref 0

let verbosity_set n =
  verbose := n
      
let verbosity_get () =
  !verbose

let propositional f = not @@ Ldl.modal f

module SAT = Oracle.SAT
module Simp = Oracle.Simp

let id_get (id, _, _, _, _) = id

(* remove_modality *)
let rec remove_modality (f : Ldl.formula) =
  if propositional f then f else
  match f with
  | Ldl_modal (Mod_ex, _, g) -> remove_modality g
  | _ -> failwith ("[remove_modality] invalid formula: " ^ string_of_formula f)

(* simplify *)
let simp (id, e, (c, c_opt), (a, a_opt), (post, post_opt)) =
  let c' =
    Simp.simp (if propositional c then c else remove_modality c)
  and post' =
    Simp.simp post
  in
  assert (propositional c' && propositional post');
  id, e, (c', c_opt), (a, a_opt), (post', post_opt)

(* certainty (b0, b1, b2, b3) : 0b0000 - 0b1111
 *)
let certainty b0 b1 b2 b3 =
  let int_of_bool = function true -> 1 | false -> 0 in
  (int_of_bool b0) lor (int_of_bool b1) lsl 1 lor
  (int_of_bool b2) lsl 2 lor (int_of_bool b3) lsl 3 

(** applicable r (w1, w2) examines
    whether r is applicable to a transition from w1 to w2, or not

    - if there is no chance, it returns false
    - if there is a chance, it returns true with certainty.

   1. examine (pre(r) ∩ w1) and (post(r) ∩ w2)
      if either is empty, then returns
      - false
      - certainty = (b0, 0, b2, 0) where
        b0 = pre(r) ∩ w1 is non-empty?, 
        b2 = post(r) ∩ w2 is non-empty?

   2. otherwise, it returns true with a certainty value computed as follows.

      certainty = (1, b1, 1, b3) where
        b1 = (w1 -> pre(r)) is valid or not
        b2 = (post(r) -> w2) is valid or not
 *)

let rec applicable r (w1, w2) =
  (*eprintf "(applicable)"; flush_all ();*)
  let rid, _, (pre, _), (a, _), (post, _) = r
  in

  assert (propositional pre && propositional post);
  assert (propositional w1 && propositional w2);

  (*
  let c =
    if propositional c then c else
    let c' = remove_modality c
    in
    if verbosity_get () > 1 then
      eprintf "(** chop off modality: %s ==> %s)"
	(string_of_formula c) (string_of_formula c');
    c'
  in
  assert (propositional c);
   *)

  (* post-condition of r *)
  (*
  let post =
    List.fold_left
      (fun rslt -> function Act_ensure f -> rslt @ [f] | _ -> rslt)
      [] a in
  let post : formula = Simp.simp (Ldl_conj post) in
  assert (propositional post);
   *)

  if post = Ldl_atomic "false" then false, None else
  let _ = () in
  (*
  let simp f =
    assert (propositional f);
    if verbosity_get () > 1 then
      (eprintf "(simp: %s)\n" (Ldl.string_of_formula f); flush_all ());
    (*Ldlsimp.resolve f |> Ldlsimp.flatten*)
    Ldlsimp.simp f
    (*Ldlsimp.simp_safe f*)
  in
   *)

  (* examine rule applicability -- similar to the consequence rule in the Hoare logic
     w1 -> c(=pre), {c}rule{a}, a(=post) -> w2
     ----------------------------------------- (applicable unconditionally)
                   {w1}rule{w2}
   *)

  (* possible worlds *)
  (*
  Printf.eprintf ";; applicable? (rid=%s): w1 = %S, c = %S, "
    rid (string_of_formula w1) (string_of_formula c);
  Printf.eprintf "a = %S, w2 = %S\n"
    (string_of_formula post) (string_of_formula w2);
  flush_all ();
   *)

  (* check1:  (pre & w1) & (post & w2) *)
  let check1, detail1 = applicable_check1 (pre, post) (w1, w2) in
  if not check1 then false, detail1 else
  
  (* check2 *)
  true, None

  (* (w1 -> c) guarantees unconditional applicability *)
  (* (post -> w2) *)
  (*
    let g1 = simp (Ldl_impl (w1, c)) in
    let c1 = (g1 = Ldl_atomic "true") in
    (* (post -> w2) *)
    let g2 = simp (Ldl_impl (post, w2)) in
    let c2 = (g2 = Ldl_atomic "true") in

    assert (certainty true c1 true c2 > 0);
    true, Some (certainty true c1 true c2)
   *)

and applicable_check1 (pre, post) (w1, w2) =
  (* (pre & w1) should hold in some possible worlds *)
  let b1, _ =
    (*
    let f1 = Simp.simp (Ldl_conj [pre; w1])
    in f1 = Ldl_atomic "false"
     *)
    Ldl_conj [pre; w1] |> Toysat.tseitin |> SAT.solve 

  (* (post & w2) *)
  and b2, _ =
    (*
    let f2 = Simp.simp (Ldl_conj [post; w2])
    in f2 = Ldl_atomic "false"
     *)
    Ldl_conj [post; w2] |> Toysat.tseitin |> SAT.solve 
  in

  if not b1 || not b2 then
    (* case:  !(pre & w1) or !(post & w2) -- no chance to apply this rule *)
    false, Some (certainty b1 false b2 false)
  else
    true, None

(** printing *)

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

(* tid = id of transition to which r is applicable.
   alist = [(tid, (w1, w2)); ..] *)
let print_rule_in_xml out tid_seq alist (r : t) =
  let rid, (e, e_opt), (c, c_opt), (a, a_opt), _ = r in
  out (sprintf "<rule id=%S>\n" rid);

  (* event *)
  out (sprintf "<event name=%S" e);
  out (match e_opt with None -> "/>\n" | Some s -> sprintf "><script>%s</script></event>\n" s);

  (* condition *)
  out "<condition>";
  out (sprintf "<formula%s>" (if propositional c then "" else " modal=\"true\""));
  escape out (string_of_formula (Simp.simp c));
  out "</formula>\n";
  (match c_opt with Some str -> out "<script>"; escape out str; out "</script>\n" | None -> ());
  out "</condition>\n";

  (* action *)
  let post : formula list =
    List.fold_left
      (fun rslt -> function Act_ensure f -> rslt @ [f] | _ -> rslt)
      [] a in
  out "<action>";
  out "<formula>";
  escape out (string_of_formula (Simp.simp (Ldl_conj post)));
  out "</formula>\n";
  List.iter
    (function
      | Act_raise e -> out (sprintf "<raise event=%S/>\n" e)
      | Act_raise_sum es ->
	  out "<choice>";
	  List.iter (fun e -> out @@ sprintf "<raise event=%S/>" e) es;
	  out "</choice>\n"
      | _ -> ())
    a;
  (match a_opt with Some str -> out "<script>"; escape out str; out "</script>\n" | None -> ());
  out "</action>\n";

  (* applicable *)
  out "<applicable>";
  List.iter
    (fun tid ->
      assert (List.mem_assoc tid alist);
      let w1, w2 = List.assoc tid alist in
      let b, certainty_opt = applicable r (w1, w2) in
      let certainty = match certainty_opt with None -> 0 | Some v -> v in
      (*
      assert (b || certainty land 0b11 = 0 || certainty land 0b1100 = 0);
      if deg = 0 then
	eprintf "** inapplicable: rid=%s tid=%s w1=%S c=%S w2=%S\n"
	  rid tid
	  (string_of_formula w1)
	  (string_of_formula c)
	  (string_of_formula w2);
       *)
      if b then out (sprintf "<tr name=%S certainty=\"%d\"/>" tid certainty))
    tid_seq;
  out "</applicable>\n";

  out "</rule>\n";
  ()

let debug_print_rule (id, (e, e_opt), (c, c_scr), (a, a_scr), (post, post_scr)) =
  eprintf "%s: on %s " id e;
  eprintf "when %s %s" (string_of_formula c)
    (match c_scr with None -> "" | Some str -> "{" ^ str ^ "} ");
  let print1 = function
    | Act_ensure f -> eprintf "ensure %s" (string_of_formula f)
    | Act_raise e  -> eprintf "raise %s" e
    | Act_raise_sum [e]  -> eprintf "raise %s" e
    | Act_raise_sum (e :: rest)  ->
	eprintf "raise %s" e; List.iter (fun e -> eprintf " + %s" e) rest
  in let _ =
    match a with
    | [] -> ()
    | [act] -> print1 act
    | act :: rest -> print1 act; List.iter (fun act -> eprintf ", "; print1 act) rest
  in
  let _ =
    match a_scr with
    | None -> ()
    | Some str ->
	(*eprintf " {%s}" str*)
	()
  in
  output_string stderr "\n";
  flush_all ();
  ()
