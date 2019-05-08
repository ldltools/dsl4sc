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

open Ldlsat
open Ldl

(** propositional simplifier *)

module type Simp_t = sig

  val simp : Ldl.formula -> Ldl.formula

end

module Simp : Simp_t = struct

let simp = Ldlsimp.simp

end

(** propositional SAT solver (ToySat-based) *)

module type SAT_t = sig
  val solve : Ldl.formula list -> bool * (string * bool) list
end

module ToySAT : SAT_t = struct

let solve = Toysat.solve

end

(** propositional SAT solver (Z3-based) *)

module Z3SAT : SAT_t = struct

open Z3

let _context =
  mk_context []

let _solver =
  Solver.mk_simple_solver _context

(* alist, clause -> alist', expr *)
let rec expr_of_formula alist f =
  match f with
  | Ldl_atomic "true" -> alist, Boolean.mk_true _context
  | Ldl_atomic "false" -> alist, Boolean.mk_false _context
  | Ldl_atomic p when List.mem_assoc p alist -> alist, List.assoc p alist
  | Ldl_atomic p ->
      let e = Expr.mk_const _context (Symbol.mk_string _context p) (Boolean.mk_sort _context)
      in let alist' = alist @ [p, e]
      in alist', e
  | Ldl_neg g ->
      let alist', e = expr_of_formula alist g in alist', Boolean.mk_not _context e
  | Ldl_conj fs ->
      let alist', es =
	List.fold_left
	  (fun (alist, es) f -> let alist', e = expr_of_formula alist f in alist', es @ [e])
	  ([], []) fs
      in alist', Boolean.mk_and _context es
  | Ldl_disj fs ->
      let alist', es =
	List.fold_left
	  (fun (alist, es) f -> let alist', e = expr_of_formula alist f in alist', es @ [e])
	  ([], []) fs
      in alist', Boolean.mk_or _context es
  | Ldl_impl (f1, f2) ->
      let alist1, e1 = expr_of_formula alist f1
      in let alist2, e2 = expr_of_formula alist1 f2
      in alist2, Boolean.mk_implies _context e1 e2
  | Ldl_modal _ -> invalid_arg "[expr_of_formula]"

let solve (clauses : formula list) = 
  (*assert (let f = Ldl_conj clauses in not (modal f) && cnf_p f);*)
  Solver.reset _solver;
  let _, exprs =
    List.fold_left
      (fun (alist, exprs) c -> let alist', e = expr_of_formula alist c in alist', exprs @ [e])
      ([], []) clauses
  in
  Solver.add _solver exprs;
  match Solver.check _solver [] with
  | UNSATISFIABLE -> false, []
  | UNKNOWN -> failwith "unknown"
  | SATISFIABLE ->
      true, []
end

(** binding *)

(*module SAT = ToySAT*)
module SAT = Z3SAT
