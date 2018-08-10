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

(* returns: 0 = inapplicable, 1 = conditionally, 3 = always *)
let applicable (r : rule) (w1, w2) =
  let rid, _, (c, _), (a, _) = r in

  if not (propositional c) then failwith ("rule_applicable: " ^ (string_of_formula c));

  (* post-condition *)
  let post =
    List.fold_left
      (fun rslt -> function Act_ensure f -> rslt @ [f] | _ -> rslt)
      [] a in
  let post : formula = Ldl_conj post in
  assert (propositional post);

  (* possible worlds *)
  assert (propositional w1 && propositional w2);

  (* examine rule applicability -- similar to the consequence rule in the Hoare logic
     w1 -> c(=pre), {c}rule{a}, a(=post) -> w2
     ----------------------------------------- (applicable unconditionally)
                   {w1}rule{w2}
   *)
  let simp f =
    (*Printf.eprintf "** simp (%s)\n" (string_of_formula f); flush_all ();*)
    Ldlsimp.simp f
    (*Ldlsimp.resolve f*)
    (*let g = Ldlsimp.simp f in try Ldlsimp.resolve g |> Ldlsimp.simp_sort |> Ldlsimp.simp_uniq with Not_found -> g*)
    (*try Ldl.resolve f with _ -> try Ldl.simp f with _ -> f*)
  in

 (*
  Printf.eprintf ";; applicable? (rid=%s): w1 = %S, c = %S, "
    rid (string_of_formula w1) (string_of_formula c);
  Printf.eprintf "a = %S, w2 = %S\n"
    (string_of_formula post) (string_of_formula w2);
  flush_all ();
  *)

  let int_of_bool = function true -> 1 | false -> 0 in
  (* certainty : 0 (inapplicable) - 0b1111 *)
  let certainty (pre1, pre2) (post1, post2) =
    (int_of_bool pre1) lor (int_of_bool pre2) lsl 1 lor
    (int_of_bool post1) lsl 2 lor (int_of_bool post2) lsl 3 
  in
  (* (c & w1) should hold in some possible worlds *)
  let f1 = simp (Ldl_conj [c; w1]) in
  let b1 = (f1 = Ldl_atomic "false") in
  (* (post & w2) *)
  let f2 = simp (Ldl_conj [post; w2]) in
  let b2 = (f2 = Ldl_atomic "false") in

  if b1 || b2 then
    (* case:  !(c & w1) or !(post & w2) -- no chance to apply this rule *)
    false, certainty (not b1, false) (not b2, false)

  else
    (* (w1 -> c) guarantees unconditional applicability *)
    let g1 = simp (Ldl_impl (w1, c)) in
    let c1 = (g1 = Ldl_atomic "true") in
    (* (post -> w2) *)
    let g2 = simp (Ldl_impl (post, w2)) in
    let c2 = (g2 = Ldl_atomic "true") in

    (assert (certainty (true, c1) (true, c2) > 0);
     true, certainty (true, c1) (true, c2))

  
