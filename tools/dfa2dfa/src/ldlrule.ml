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
open Ldlsimp
open Printf

type rule =
    string * string * condition * action
      (* (id, e, c, a) *)

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

let applicable (r : rule) (w1, w2) =
  let rid, _, (c, _), (a, _) = r in

  if not (propositional c) then failwith ("rule_applicable: " ^ (string_of_formula c));

  (* post-condition of r *)
  let post =
    List.fold_left
      (fun rslt -> function Act_ensure f -> rslt @ [f] | _ -> rslt)
      [] a in
  let post : formula = simp (Ldl_conj post) in
  assert (propositional post);

  if post = Ldl_atomic "false" then false, None else
  let simp f =
    (*Ldlsimp.resolve f*)
    Ldlsimp.simp f
    (*Ldlsimp.simp_safe f*)
  in

  (* examine rule applicability -- similar to the consequence rule in the Hoare logic
     w1 -> c(=pre), {c}rule{a}, a(=post) -> w2
     ----------------------------------------- (applicable unconditionally)
                   {w1}rule{w2}
   *)

  (* possible worlds *)
  assert (propositional w1 && propositional w2);

 (*
  Printf.eprintf ";; applicable? (rid=%s): w1 = %S, c = %S, "
    rid (string_of_formula w1) (string_of_formula c);
  Printf.eprintf "a = %S, w2 = %S\n"
    (string_of_formula post) (string_of_formula w2);
  flush_all ();
  *)

  (* (c & w1) should hold in some possible worlds *)
  let f1 = simp (Ldl_conj [c; w1])
  in let b1 = (f1 = Ldl_atomic "false")
  (* (post & w2) *)
  in let f2 = simp (Ldl_conj [post; w2])
  in let b2 = (f2 = Ldl_atomic "false")
  in

  if b1 || b2 then
    (* case:  !(c & w1) or !(post & w2) -- no chance to apply this rule *)
    false, Some (certainty (not b1) false (not b2) false)

  else
    (* (w1 -> c) guarantees unconditional applicability *)
    let g1 = simp (Ldl_impl (w1, c)) in
    let c1 = (g1 = Ldl_atomic "true") in
    (* (post -> w2) *)
    let g2 = simp (Ldl_impl (post, w2)) in
    let c2 = (g2 = Ldl_atomic "true") in

    assert (certainty true c1 true c2 > 0);
    true, Some (certainty true c1 true c2)

(** printing *)

let debug_print_rule (id, e, (c, c_scr), (a, a_scr)) =
  eprintf "%s: on %s " id e;
  eprintf "when %s %s" (string_of_formula c)
    (match c_scr with None -> "" | Some str -> "{" ^ str ^ "} ");
  eprintf "do ";
  let print1 = function
    | Act_ensure f -> eprintf "%s" (string_of_formula f)
    | Act_raise e  -> eprintf "raise %s" e in
  let _ =
    match a with
    | [] -> ()
    | [act] -> print1 act
    | act :: rest -> print1 act; List.iter (fun act -> eprintf ", "; print1 act) rest
  in
  let _ =
    match a_scr with
    | None -> ()
    | Some str -> eprintf " {%s}" str
  in
  output_string stderr "\n";
  flush_all ();
  ()
