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

(** term *)
type _ term =
  | Tm_const : 'a * base_t -> 'a term
  | Tm_var : string * base_t -> 'a term
  | Tm_app : ('a -> 'b) term * 'a term -> 'b term
  | Tm_abs : ('a term -> 'b term) -> ('a -> 'b) term
  | Tm_op : string * 'a term list -> 'a term
  | Tm_eq : 'a term * 'a term -> bool term

and base_t =
  | Ty_prop
  | Ty_nat of int
(*| Ty_fun : 'a base_t * 'b base_t -> ('a -> 'b) base_t*)

(* ppx does not support gadt *)

(* term ops *)

let equal_term tm1 tm2 = failwith "[equal_term]"

(* eval env e -> n *)
let rec eval_term_int (env : (string * (base_t * int)) list) = function
  | Tm_const (n, Ty_nat _) -> n
  | Tm_var (x, Ty_nat _) when List.mem_assoc x env -> snd (List.assoc x env)
  | Tm_var (x, Ty_nat _) -> raise Not_found
  | Tm_var (x, Ty_prop) -> invalid_arg "[eval_term_int]"
  | Tm_op ("+", es) ->
      List.fold_left (fun rslt e -> rslt + eval_term_int env e) 0 es
  | Tm_op ("-", e :: es) ->
      List.fold_left (fun rslt e -> rslt - eval_term_int env e) (eval_term_int env e) es
  | Tm_op ("<", [e1; e2]) ->
      (* work-around: this should be regarded as a bool term *)
      let n1, n2 = eval_term_int env e1, eval_term_int env e2
      in if n1 < n2 then 1 else 0
  | Tm_op (op, es) -> invalid_arg op
  | _ -> failwith "[eval_term]"

(* variable of nat (n) -> m propositions where m = log2 (n) *)
let term_to_propositions = function
  | Tm_var (x, Ty_nat n) ->
      let len = float_of_int n in
      let nbit = int_of_float @@ ceil (log len /. log 2.0) in
      List.init nbit (fun i -> "_" ^ x ^ "_" ^ string_of_int i)
  | _ -> invalid_arg "[term_to_propositions]"

(** pretty-printing *)

let rec print_term (out : string -> unit) (e : int term) =
  match e with
  | Tm_const (n, Ty_nat _) ->
      out (string_of_int n)
  | Tm_var (x, _) ->
      out x
  | Tm_op ("+", e :: rest) ->
      out "(";
      print_term out e;
      List.iter (fun e -> out " + "; print_term out e) rest;
      out ")";
  | _ -> failwith "[print_term]"

(** pretty-printing -- ppx-compliant *)

let rec pp_term (pp : Format.formatter -> int -> unit) (fmt : Format.formatter) (tm : int term) =
  match tm with
  | Tm_const (n, Ty_nat _) -> Format.pp_print_int fmt n
  | Tm_var (x, Ty_nat _) -> Format.pp_print_string fmt x
  | Tm_op ("+", e :: rest) ->
      Format.pp_print_string fmt "(";
      pp_term pp fmt e;
      List.iter (fun e -> Format.pp_print_string fmt " + "; pp_term pp fmt e) rest;
      Format.pp_print_string fmt ")"
  | _ -> failwith "[pp_term]"

let term_of_yojson json =
  failwith "[term_of_yojson]"

let term_to_yojson tm =
  failwith "[term_to_yojson]"

(** property *)
type property =
  | Prop_atomic of string
  | Prop_equal of int term * int term
  | Prop_neg of property
  | Prop_conj of property list
  | Prop_disj of property list
  | Prop_modal of modality * labelled_path * labelled_property

  (* deprecated *)
  | Prop_atomic_elt of string * int term list
  | Prop_label of string

and modality =
  | Mod_all | Mod_ex

and labelled_property =
    property * label option

(** path *)
and path =
  | Path_prop of property
	(** property (propositional) *)
  | Path_seq of labelled_path list
  | Path_sum of labelled_path list
  | Path_test of property
  | Path_star of labelled_path

  (* deprecated *)
  | Path_label of string

and labelled_path =
    path * label option

and label =
    string

[@@deriving show, yojson, eq]

type t = property

(** property ops *)

(*
let rec propositional = function
  | Prop_atomic _ -> true
  | Prop_equal _ -> true
  | Prop_neg p -> propositional p
  | Prop_conj ps | Prop_disj ps ->
      List.fold_left (fun rslt p -> rslt && propositional p) true ps
  | Prop_modal _ -> false
  | Prop_label _ -> failwith "propositional"
 *)

let rec modal_p = function
  | Prop_atomic _ -> false
  | Prop_equal _ -> false
  | Prop_neg p -> modal_p p
  | Prop_conj ps | Prop_disj ps ->
      (match List.find_opt modal_p ps with None -> false | _ -> true)
  | Prop_modal _ -> true
  | Prop_label _ -> failwith "[modal_p]"

(* simplifaction w/o term value info *)

let rec flatten (f : t) =
  match f with
  | Prop_atomic _ -> f
  | Prop_equal _ -> f
  | Prop_neg f' -> Prop_neg (flatten f')
  | Prop_conj fs ->
      let fs' = flatten_conj (List.map flatten fs) in
      if List.length fs' = 1 then List.hd fs' else Prop_conj fs'
  | Prop_disj fs ->
      let fs' = flatten_disj (List.map flatten fs) in
      if List.length fs' = 1 then List.hd fs' else Prop_disj fs'
  | Prop_modal _ -> f
  | _ -> failwith "[flatten]"

(* f1 ∧ f2 ∧ ... -> f1' ∧ f2' ∧ ...
 *)
and flatten_conj (fs : t list) =
  List.fold_left
    (fun rslt f -> match f with Prop_conj fs' -> rslt @ fs' | _ -> rslt @ [f])
    [] fs

(* ** non-recursive *)
and flatten_disj (fs : t list) =
  List.fold_left
    (fun rslt f -> match f with Prop_disj fs' -> rslt @ fs' | _ -> rslt @ [f])
    [] fs

(* equiv
   - !!a = a
   - true /\ a = a
   - false /\ a = false
   - true \/ a = true
   - false \/ a = a
   - a /\ a = a
   - a \/ a = a
   - a /\ !a = false
   - a \/ !a = true
 *)

let rec simp_equiv f =
  let tt = Prop_atomic "true" and ff = Prop_atomic "false" in
  match f with
  | Prop_atomic _ -> f

  | Prop_neg f' when f' = tt -> ff
  | Prop_neg f' when f' = ff -> tt
  | Prop_neg (Prop_atomic _) -> f
  | Prop_neg (Prop_neg f') -> simp_equiv f'
  | Prop_neg f' -> Prop_neg (simp_equiv f')

  | Prop_conj [] -> tt
  | Prop_conj fs ->
      let f' =
	match simp_equiv_conj [] (List.map simp_equiv fs) with
	| [] -> tt | [f'] -> f'	| fs' -> Prop_conj fs'
      in f'
  | Prop_disj [] -> ff
  | Prop_disj fs ->
      let f' =
	match simp_equiv_disj [] (List.map simp_equiv fs) with
	| [] -> ff | [f'] -> f'	| fs' -> Prop_disj fs'
      in f'

  | _ -> f

and simp_equiv_conj rslt conj =
  let tt = Prop_atomic "true" and ff = Prop_atomic "false" in
  match conj with
  | [] -> rslt
  | f :: rest when f = ff || f = Prop_neg tt -> [ff]
  | f :: rest when f = tt || f = Prop_neg ff -> simp_equiv_conj rslt rest
  | f :: rest when List.mem (Prop_neg f) rest (* f ∧ ¬f *) -> [ff]
  | Prop_neg f :: rest when List.mem f rest (* ¬f ∧ f *) -> [ff]
  | f :: rest when List.mem f rslt (* duplicate *) ->
      simp_equiv_conj rslt rest
  | f :: rest -> simp_equiv_conj (rslt @ [f]) rest

and simp_equiv_disj rslt disj =
  let tt = Prop_atomic "true" and ff = Prop_atomic "false" in
  match disj with
  | [] -> rslt
  | f :: rest when f = tt || f = Prop_neg ff -> [tt]
  | f :: rest when f = ff || f = Prop_neg tt -> simp_equiv_disj rslt rest
  | f :: rest when List.mem (Prop_neg f) rest (* f ∨ ¬f *) -> [tt]
  | Prop_neg f :: rest when List.mem f rest (* ¬f ∨ f *) -> [tt]
  | f :: rest when List.mem f rslt (* duplicate *) ->
      simp_equiv_disj rslt rest
  | f :: rest -> simp_equiv_disj (rslt @ [f]) rest

let rec simp p =
  p |> flatten |> simp_equiv

(* *)

let rec find_term_variables f =
  find_term_variables_rec [] f

and find_term_variables_rec (rslt : (string * base_t) list) f =
  match f with
  | Prop_atomic _ -> rslt
  | Prop_equal (e1, e2) ->
      let rec find_variable rslt (e : int term) =
	match e with
	| Tm_var (x, ty) when not (List.mem_assoc x rslt) -> rslt @ [x, ty]
	| Tm_op (_, es) ->
	    List.fold_left (fun rslt -> find_variable rslt) rslt es
	| _ -> rslt
      in List.fold_left (fun rslt e -> find_variable rslt e) rslt [e1; e2]
  | Prop_neg f' -> find_term_variables_rec rslt f'
  | Prop_conj fs | Prop_disj fs ->
      List.fold_left (fun rslt f' -> find_term_variables_rec rslt f') rslt fs
  | Prop_modal (_, (r, _), (f', _)) ->
      find_term_variables_rec rslt f'      
  | _ -> failwith "[find_term_variables_rec]"

let rec include_term_variable_p f =
  match f with
  | Prop_atomic _ -> false
  | Prop_equal (e1, e2) ->
      let rec include_p (e : int term) =
	match e with
	| Tm_const _ -> false
	| Tm_var (x, _) -> true
	| Tm_op (_, es) ->
	    (match List.find_opt include_p es with None -> false | Some _ -> true)
	| _ -> failwith "[include_ter_variable_p]"
      in include_p e1 || include_p e2
  | Prop_neg f' -> include_term_variable_p f'
  | Prop_conj fs | Prop_disj fs ->
      (match List.find_opt include_term_variable_p fs with None -> false | Some _ -> true)
  | Prop_modal (m, (r, r_opt), (f', f_opt)) ->
      include_term_variable_p f'
  | _ -> failwith "[include_term_variable_p]"

(* property instantiation using term value info *)

let rec instantiate (env : (string * (base_t * int)) list) (p : t) =
  instantiate_rec env p

and instantiate_rec env p =
  match p with
  | Prop_equal (e1, e2) ->
      let n1, n2 = eval_term_int env e1, eval_term_int env e2
      in Prop_atomic (string_of_bool (n1 = n2))
  | Prop_neg p' -> Prop_neg (instantiate_rec env p')
  | Prop_conj ps -> Prop_conj (List.map (instantiate_rec env) ps)
  | Prop_disj ps -> Prop_disj (List.map (instantiate_rec env) ps)
  | Prop_modal (m, (r, r_opt), (p, p_opt)) ->
      Prop_modal (m, (instantiate_path env r, r_opt), (instantiate env p, p_opt))
  | _ -> p

and instantiate_path env r =
  match r with
  | Path_prop p -> Path_prop (instantiate env p)
  | Path_seq rs -> Path_seq (List.map (fun (r, r_opt) -> instantiate_path env r, r_opt) rs)
  | Path_sum rs -> Path_sum (List.map (fun (r, r_opt) -> instantiate_path env r, r_opt) rs)
  | Path_test p -> Path_test (instantiate env p)
  | Path_star (r', r_opt) -> Path_star (instantiate_path env r', r_opt)
  | _ -> r

(* split *)

let rec split p =
  let alist, _ =
    (* alist = [x_1, (t_1, pos_1, len_1); x_2, (t_2, pos_2, len_2); ..]
       where
       - x_i : name of the i-th term variable in p
       - t_i : type (base_t) of the variable
       - len_i = (bit-)length of the variable
       - pos_i = (bit-)position in a single int value representing term values
     *)
    List.fold_left
      (fun (rslt, i) (x, ty) ->
	match ty with
	| Ty_nat n when not (List.mem_assoc x rslt) ->
	    let nbit = int_of_float @@ ceil (log (float_of_int n) /. log 2.0) in
	    (rslt @ [x, (Ty_nat n, i, nbit)]), i + nbit
	| _ -> rslt, i)
      ([], 0) (find_term_variables p)
  in let nbit = List.fold_left (fun rslt (_, (_, _, len)) -> rslt + len) 0 alist
  in
  if nbit > 8 then failwith ("[split] too many combinations: 2^" ^ string_of_int nbit);
  split_rec p alist nbit [] 0

and split_rec p alist nbit rslt i =
  if i = 1 lsl nbit then rslt else
  let bits : bool list = gen_bits nbit i
  in let env : (string * (base_t * int)) list =
    List.fold_left
      (fun rslt (x, (ty, pos, len)) ->
	let n =
	  List.fold_left
	    (fun rslt k ->
	      assert (0 <= pos + k && pos + k < List.length bits);
	      rslt + if List.nth bits (pos + k) then 1 lsl k else 0)
	    0 (List.init len (fun k -> k))
	in
	rslt @ [x, (ty, n)])
      [] alist
  in let q = instantiate env p
  in
  assert (find_term_variables q = []);
  split_rec p alist nbit (rslt @ [env, q]) (i + 1)

(* gen_bits nbit c returns [b(0); b(1); ...; b(nbit - 1)] = bit-representation of c
 *)
and gen_bits nbit (n : int) =
  gen_bits_rec nbit n [] 0

and gen_bits_rec nbit (n : int) rslt i =
  if i = nbit then
    rslt
  else
    gen_bits_rec nbit n (rslt @ [n land (1 lsl i) <> 0]) (i + 1)

(* propositionalization *)

let rec propositionalize_eq (e1 : int term) (e2 : int term) =
  match e1, e2 with
  | Tm_const (c1, Ty_nat n1), Tm_const (c2, Ty_nat n2) ->
      (* c1 = c2 *)
      Prop_atomic (string_of_bool (c1 = c2))

  | Tm_var (x1, Ty_nat n1), Tm_const (c2, Ty_nat n2) when n1 > c2 ->
      (* x1 = c2 *)
      let xs : string list = term_to_propositions e1 in
      let nbit = int_of_float @@ ceil (log (float_of_int n1) /. log 2.0) in
      let bits : bool list = gen_bits nbit c2 in
      let props, _  =
	List.fold_left
	  (fun (rslt, i) x ->
	    let f =
	      if List.nth bits i then Prop_atomic x else Prop_neg (Prop_atomic x)
	    in (rslt @ [f]), i + 1)
	  ([], 0) xs
      in Prop_conj props
  | Tm_var (x1, Ty_nat n1), Tm_const (c2, Ty_nat n2) ->
      (* x1 = c2 *)
      assert (n1 <= c2);
      Prop_atomic "false"
  | Tm_const _, Tm_var _ ->
      (* c1 = x2 *)
      propositionalize_eq e2 e1

  | Tm_var (x1, Ty_nat n1), Tm_var (x2, Ty_nat n2) when x1 = x2 ->
      (* x = x *)
      Prop_atomic "true"
  | Tm_var (x1, Ty_nat n1), Tm_var (x2, Ty_nat n2) when n1 >= n2 ->
      (* x1 = x2 *)
      let xs1 : string list = term_to_propositions e1
      and nbit1 = int_of_float @@ ceil (log (float_of_int n1) /. log 2.0)
      and xs2 : string list = term_to_propositions e2
      and nbit2 = int_of_float @@ ceil (log (float_of_int n2) /. log 2.0) in
      let conj1 =
	List.init nbit2
	  (fun i ->
	    let p1 = Prop_atomic (List.nth xs1 i) and p2 = Prop_atomic (List.nth xs2 i)
	    in Prop_disj [Prop_conj [p1; p2]; Prop_conj [Prop_neg p1; Prop_neg p2]])
      and conj2 =
	List.init (nbit1 - nbit2)
	  (fun i -> Prop_neg (Prop_atomic (List.nth xs1 (nbit2 + i))))
      in Prop_conj (conj1 @ conj2)

(*
  | Tm_const _, Tm_op ("+", es2)
  | Tm_var _, Tm_op ("+", es2) ->
      propositionalize_eq_summands [e1] es2
  | Tm_op ("+", es1), Tm_const _
  | Tm_op ("+", es1), Tm_var _ ->
      propositionalize_eq_summands es1 [e2]
  | Tm_op ("+", es1), Tm_op ("+", es2) ->
      propositionalize_eq_summands es1 es2

  | _ -> failwith "[propositionalize_eq]"

and propositionalize_eq_summands (es1 : int term list) es2 =
  let extract_vars es =
    let alist, _ =
      List.fold_left
	(fun (rslt, i) e ->
	  match e with
	  | Tm_var (x, Ty_nat n) when not (List.mem_assoc x rslt) ->
	      let nbit = int_of_float @@ ceil (log (float_of_int n) /. log 2.0) in
	      (rslt @ [x, (i, nbit)]), i + nbit
	  | _ -> rslt, i)
	([], 0) es
    in alist
  in let alist : (string * (int * int)) list = extract_vars (es1 @ es2)
  in let nbit = List.fold_left (fun rslt (_, (_, n)) -> rslt + n) 0 alist
  in
  assert (alist <> [] && 0 < nbit);
  if (nbit > 8) then
    failwith ("[propositionalize_eq_summands] too many combinations: 2^" ^ (string_of_int nbit));
  let pos, neg = propositionalize_eq_summands_rec es1 es2 alist ([], []) nbit 0
  in
  Prop_conj ((Prop_disj pos) :: neg)

and propositionalize_eq_summands_rec es1 es2 alist rslt nbit i =
  if i = 1 lsl nbit then rslt else
  let bits : bool list = gen_bits nbit i
  in let eval_term alist bits es =
    (* es = [e1; e2; ...] -> e1 + e2 + ... *)
    List.fold_left
      (fun rslt e ->
	match e with
	| Tm_const (n, Ty_nat _) -> rslt + n
	| Tm_var (x, Ty_nat _) when List.mem_assoc x alist ->
	    let pos, len = List.assoc x alist in
	    let n =
	      (* n = value of e *)
	      List.fold_left
		(fun rslt k ->
		  assert (0 <= pos + k && pos + k < List.length bits);
		  rslt + if List.nth bits (pos + k) then 1 lsl k else 0)
		0 (List.init len (fun k -> k))
	    in
	    rslt + n
	| _ -> failwith "[propositionalize_eq_summands_rec] eval_term")
      0 es
  in let n1, n2 = eval_term alist bits es1, eval_term alist bits es2
  in
  (*
  output_string stderr "bits:"; List.iter (fun b -> Printf.eprintf " %b" b) bits;
  output_string stderr "\n";
  print_term (output_string stderr) (Tm_op ("+", es1)); Printf.eprintf " = %d\n" n1;
  print_term (output_string stderr) (Tm_op ("+", es2)); Printf.eprintf " = %d\n" n2;
   *)
  let var_to_prop alist bits es =
    (* es = {e_i}_i -> {conj_i}_i where conj_i = conj of props for e_i *)
    List.fold_left
      (fun rslt -> function
	| Tm_var (x, Ty_nat n) ->
	    assert (List.mem_assoc x alist);
	    let pos, len = List.assoc x alist in
	    let ys = term_to_propositions (Tm_var (x, Ty_nat (1 lsl len))) in
	    assert (List.length ys = len);
	    let rslt, _ =
	      List.fold_left
	      (fun (rslt, k) y ->
		let rslt' =
		  rslt @
		  if List.nth bits (pos + k)
		  then [Prop_atomic y]
		  else [Prop_neg (Prop_atomic y)]
		in rslt', k + 1)
		(rslt, 0) ys
	    in rslt
	| _ -> rslt)
      [] es
  in let pos, neg = rslt
  in let pos', neg' =
    let conj = var_to_prop alist bits (es1 @ es2)
    in if n1 = n2
    then pos @ [Prop_conj conj], neg
    else pos, neg @ [Prop_neg (Prop_conj conj)]
  in
  propositionalize_eq_summands_rec es1 es2 alist (pos', neg') nbit (i + 1)
 *)

(* propositionalize f *)

let rec propositionalize f =
  match f with
  | Prop_atomic _ -> f
  | Prop_equal (e1, e2) ->
      propositionalize_eq e1 e2
  | Prop_neg f' -> Prop_neg (propositionalize f')
  | Prop_conj fs -> Prop_conj (List.map (propositionalize) fs)
  | Prop_disj fs -> Prop_disj (List.map (propositionalize) fs)
  | Prop_modal (m, p, (f, opt)) ->
      let p' = propositionalize_lpath p and f' = propositionalize f
      in Prop_modal (m, p', (f', opt))
  | _ -> failwith "[propositionalize]"

and propositionalize_lpath (p, l_opt) =
  (propositionalize_path p), l_opt

and propositionalize_path p =
  match p with
  | Path_prop f -> Path_prop (propositionalize f)
  | Path_seq ps -> Path_seq (List.map (propositionalize_lpath) ps)
  | Path_sum ps -> Path_sum (List.map (propositionalize_lpath) ps)
  | Path_test f -> Path_test (propositionalize f)
  | Path_star p' -> Path_star (propositionalize_lpath p')
  | _ -> failwith "[propositionalize_path]"

(** pretty-printing *)

let rec print_labelled_property out (p, l_opt) =
  match l_opt with
  | None -> print_property out p
  | Some l ->
      out l; out ":";
      let _ =
	match p with
	| Prop_atomic _ ->
	    print_property out p
	| _ ->
	    out "("; print_property out p; out ")"
      in ()

and print_property out ?(fancy=false) (p : property) =
  print_property_rec out ~fancy p

and print_property_rec out ?(fancy=false) (f : property) =
  let top, bot, neg, conj, disj, impl =
    if not fancy
    then "top", "bot", "!", "&", "|", "->"
    else
      (Printf.sprintf "%c%c%c" (Char.chr 0xe2) (Char.chr 0x8a) (Char.chr 0xa4)),
      (* ⊤ *)
      (Printf.sprintf "%c%c%c" (Char.chr 0xe2) (Char.chr 0x8a) (Char.chr 0xa5)),
      (* ⊥ *)
      (Printf.sprintf "%c%c" (Char.chr 0xc2) (Char.chr 0xac)),
      (* ¬ *)
      (Printf.sprintf "%c%c%c" (Char.chr 0xe2) (Char.chr 0x88) (Char.chr 0xa7)),
      (* ∧ *)
      (Printf.sprintf "%c%c%c" (Char.chr 0xe2) (Char.chr 0x88) (Char.chr 0xa8)),
      (* ∨ *)
      (Printf.sprintf "%c%c%c" (Char.chr 0xe2) (Char.chr 0x86) (Char.chr 0x92))
      (* → *)
  in
  match f with
  | Prop_atomic "true" when fancy -> out top
  | Prop_atomic "false" when fancy -> out bot
  | Prop_atomic a -> out a

  | Prop_equal (e1, e2) ->
      print_term out e1;
      out " = ";
      print_term out e2

  | Prop_neg f' when prec f' <= prec f -> out neg; print_property_rec out ~fancy f'
  | Prop_neg f' -> out (neg ^ "("); print_property_rec out ~fancy f'; out ")"

  | Prop_conj [] -> out "true"
  | Prop_conj [f'] when prec f' <= prec f -> print_property_rec out ~fancy f'
  | Prop_conj [f'] -> out "("; print_property_rec out ~fancy f'; out ")"
  | Prop_conj (f' :: fs) when prec f' <= prec f ->
      print_property_rec out ~fancy f';
      out (" " ^ conj ^ " ");
      print_property_rec out ~fancy (Prop_conj fs)
  | Prop_conj (f' :: fs) ->
      out "("; print_property_rec out ~fancy f'; out ")";
      out (" " ^ conj ^ " ");
      print_property_rec out ~fancy (Prop_conj fs)

  | Prop_disj [] -> out "false"
  | Prop_disj [f'] when prec f' <= prec f -> print_property_rec out ~fancy f'
  | Prop_disj [f'] -> out "("; print_property_rec out ~fancy f'; out ")"
  | Prop_disj (f' :: fs) when prec f' <= prec f ->
      print_property_rec out ~fancy f';
      out (" " ^ disj ^ " ");
      print_property_rec out ~fancy (Prop_disj fs)
  | Prop_disj (f' :: fs) ->
      out "("; print_property_rec out ~fancy f'; out ")";
      out (" " ^ disj ^ " ");
      print_property_rec out ~fancy (Prop_disj fs)

(*
  | Prop_impl (f', g) ->
      let _ =
	match (prec f' < prec f), (prec g <= prec f) with
	| false, false ->
	    out "("; print_property out ~fancy f'; out ")";
	    out (" " ^ impl ^ " ");
	    out "("; print_property out ~fancy g; out ")"
	| false, true  ->
	    out "("; print_property out ~fancy f'; out ")";
	    out (" " ^ impl ^ " ");
	    print_property out ~fancy g
	| true, false  ->
	    print_property out ~fancy f';
	    out (" " ^ impl ^ " ");
	    out "("; print_property out ~fancy g; out ")"
	| true, true   ->
	    print_property out ~fancy f';
	    out (" " ^ impl ^ " ");
	    print_property out ~fancy g
      in ()
 *)

  | Prop_modal (m, r, f') ->
      out (match m with Mod_all -> "[" | _ -> "<");
      print_labelled_path out r;
      out (match m with Mod_all -> "] " | _ -> "> ");
      if prec (fst f') < prec f
      then print_labelled_property out f'
      else (out "("; print_labelled_property out f'; out ")")

  | Prop_label l ->
      out "$"; out l

  | _ -> failwith ("[print_property_rec] " ^ show_property f)

(* precedence: neg < and < lor < implies *)
and prec = function
  | Prop_atomic _ -> 0
  | Prop_equal _ -> 20
  | Prop_neg _ -> 10
  | Prop_conj _ -> 100
  | Prop_disj _ -> 200
  (*| Prop_impl _ -> 300*)
  | Prop_modal _ -> 30
  | Prop_label _ -> 0

and print_labelled_path out (r, l_opt) =
  match l_opt with
  | None -> print_path out r
  | Some l ->
      out l; out ":";
      let _ =
	match r with
	| Path_prop _ | Path_label _ ->
	    print_path out r
	| _ ->
	    out "("; print_path out r; out ")"
      in ()

and print_path out ?(fancy=false) r =
  match r with
  | Path_prop f ->
      out "{"; print_property out ~fancy f; out "}"

  (* seq *)
  | Path_seq [r'] when path_prec (fst r') <= path_prec r ->
      print_labelled_path out r'
  | Path_seq [r'] ->
      out "("; print_labelled_path out r'; out ")"
  | Path_seq (r' :: rs) when path_prec (fst r') <= path_prec r ->
      print_labelled_path out r';
      out "; ";
      print_path out (Path_seq rs)
  | Path_seq (r' :: rs) ->
      out "("; print_labelled_path out r'; out ")";
      out "; ";
      print_path out (Path_seq rs)

  (* sum *)
  | Path_sum [r'] when path_prec (fst r') <= path_prec r ->
      print_labelled_path out r'
  | Path_sum [r'] ->
      out "("; print_labelled_path out r'; out ")"
  | Path_sum (r' :: rs) when path_prec (fst r') <= path_prec r ->
      print_labelled_path out r';
      out " + ";
      print_path out (Path_sum rs)
  | Path_sum (r' :: rs) ->
      out "("; print_labelled_path out r'; out ")";
      out "; ";
      print_path out (Path_sum rs)

  (* test *)
  | Path_test f ->
      out "{"; print_property out ~fancy f; out "}?"

  (* star *)
  | Path_star r' when path_prec (fst r') <= path_prec r ->
      print_labelled_path out r'; out "*"
  | Path_star r' ->
      out "("; print_labelled_path out r'; out ")*"

  (* label *)
  | Path_label l ->
      out "$"; out l

  | _ -> failwith ("[print_path] " ^ show_path r)

(* precedence: grouping (()) < *, ? < concat (;) < choice (+) *)
and path_prec = function
  | Path_prop _ -> 0
  | Path_seq rs -> 100
  | Path_sum rs -> 200
  | Path_test f -> 50
  | Path_star r -> 30
  | Path_label _ -> 0

(** pretty-printing (string conversion) *)

let string_of_labelled_property lp =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  print_labelled_property concat lp;
  !str

let string_of_labelled_path lp =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  print_labelled_path concat lp;
  !str
