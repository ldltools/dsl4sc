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
type base_t =
  | Ty_nat of int
  | Ty_bool
(*| Ty_fun : 'a base_t * 'b base_t -> ('a -> 'b) base_t*)
[@@deriving show, yojson, eq]

type _ term =
  | Tm_const : 'a * base_t -> 'a term
  | Tm_var : string * base_t -> 'a term
  | Tm_app : ('a -> 'b) term * 'a term -> 'b term
  | Tm_abs : ('a term -> 'b term) -> ('a -> 'b) term
  | Tm_op : string * 'a term list -> 'a term
  | Tm_eq : 'a term * 'a term -> bool term

(* ppx does not support gadt *)

(* term ops *)

let equal_term tm1 tm2 = failwith "[equal_term]"

(* eval env e -> n *)
let rec eval_term_int (env : (string * (base_t * int)) list) = function
  | Tm_const (n, Ty_nat _) -> n
  | Tm_var (x, Ty_nat _) when List.mem_assoc x env -> snd (List.assoc x env)
  | Tm_var (x, Ty_nat _) -> raise Not_found
  | Tm_var (x, Ty_bool) -> invalid_arg "[eval_term_int]"

  | Tm_op ("+", es) ->
      List.fold_left (fun rslt e -> rslt + eval_term_int env e) 0 es
  | Tm_op ("-", e :: es) ->
      List.fold_left (fun rslt e -> rslt - eval_term_int env e) (eval_term_int env e) es
  | Tm_op ("*", es) ->
      List.fold_left (fun rslt e -> rslt * eval_term_int env e) 1 es
  | Tm_op ("/", e :: es) ->
      List.fold_left (fun rslt e -> rslt / eval_term_int env e) (eval_term_int env e) es
  | Tm_op ("<", [e1; e2]) ->
      (* work-around: this actually should be regarded as a bool term, instead of a int term *)
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
  | Tm_op (op, [e']) when prec e' <= prec e ->
      print_term out e';
  | Tm_op (op, [e']) ->
      out "("; print_term out e'; out ")";
  | Tm_op (op, e' :: rest) when prec e' <= prec e ->
      print_term out e';
      out " "; out op; out " ";
      print_term out (Tm_op (op, rest))
  | Tm_op (op, e' :: rest) ->
      out "("; print_term out e'; out ")";
      out " "; out op; out " ";
      print_term out (Tm_op (op, rest))
  | _ -> failwith "[print_term]"

(* precedence: *, / < +, - *)
and prec = function
  | Tm_const _ | Tm_var _ -> 0
  | Tm_app _ -> 50
  | Tm_op ("*", _) | Tm_op ("/", _) -> 80
  | Tm_op ("+", _) | Tm_op ("-", _) -> 100
  | Tm_op ("<", _) -> 200
  | Tm_op _ -> 200

(** pretty-printing -- ppx-compliant *)

let rec pp_term (pp : Format.formatter -> int -> unit) (fmt : Format.formatter) (tm : int term) =
  match tm with
  | Tm_const (n, Ty_nat _) -> Format.pp_print_int fmt n
  | Tm_var (x, Ty_nat _) -> Format.pp_print_string fmt x
  | Tm_op (op, e :: rest) ->
      Format.pp_print_string fmt "(";
      pp_term pp fmt e;
      List.iter
	(fun e -> Format.pp_print_string fmt (" " ^ op ^ " "); pp_term pp fmt e)
	rest;
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
	| _ -> failwith "[include_term_variable_p]"
      in include_p e1 || include_p e2
  | Prop_neg f' -> include_term_variable_p f'
  | Prop_conj fs | Prop_disj fs ->
      (match List.find_opt include_term_variable_p fs with None -> false | Some _ -> true)
  | Prop_modal (_, (r, _), (f', _)) ->
      include_term_variable_in_path_p r || include_term_variable_p f'
  | _ -> failwith "[include_term_variable_p]"

and include_term_variable_in_path_p (r : path) =
  match r with
  | Path_prop p -> include_term_variable_p p
  | Path_seq rs | Path_sum rs ->
      (match List.find_opt (fun (r', _) -> include_term_variable_in_path_p r') rs with
      | Some _ -> true | None -> false)
  | Path_test p -> include_term_variable_p p
  | Path_star (r', _) -> include_term_variable_in_path_p r'

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
  | Prop_equal (e1, e2) when not (include_term_variable_p f) ->
      Prop_atomic (string_of_bool (eval_term_int [] e1 = eval_term_int [] e2))

  | Prop_neg f' ->
      (match simp_equiv f' with
      | Prop_atomic "true" -> ff
      | Prop_atomic "false" -> tt
      | g -> Prop_neg g)

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
  (* each elt of conj is already simpliefied *)
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
  (* each elt of disj is already simpliefied *)
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

(* property instantiation using term value info (env) *)

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
  | Prop_modal _ -> p
  | _ -> p

(*
and instantiate_path env r =
  match r with
  | Path_prop p -> Path_prop (instantiate env p)
  | Path_seq rs -> Path_seq (List.map (fun (r, r_opt) -> instantiate_path env r, r_opt) rs)
  | Path_sum rs -> Path_sum (List.map (fun (r, r_opt) -> instantiate_path env r, r_opt) rs)
  | Path_test p -> Path_test (instantiate env p)
  | Path_star (r', r_opt) -> Path_star (instantiate_path env r', r_opt)
  | _ -> r
 *)

(* split : p -> [env1, q1; env2, q2; ...] *)

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
  if nbit = 0 then [] else
  split_rec p alist nbit [] 0

and split_rec p alist nbit rslt i =
  (* generate (env(i), (instaniate env(i) p)) and add it to rslt *)
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
  split_rec p alist nbit (rslt @ [env, q]) (i + 1)

(* gen_bits nbit n returns [b(0); b(1); ...; b(nbit - 1)] = bit-representation of n
 *)
and gen_bits nbit (n : int) =
  gen_bits_rec nbit n [] 0

and gen_bits_rec nbit (n : int) rslt i =
  if i = nbit then
    rslt
  else
    gen_bits_rec nbit n (rslt @ [n land (1 lsl i) <> 0]) (i + 1)

(* propositionalize f *)

let rec propositionalize ?(keep_terms = false) f =
  match f with
  | Prop_atomic _ -> f
  | Prop_equal (e1, e2) ->
      propositionalize_eq ~keep_terms e1 e2
  | Prop_neg f' -> Prop_neg (propositionalize ~keep_terms f')

  | Prop_conj fs -> Prop_conj (List.map (propositionalize ~keep_terms) fs)
  | Prop_disj fs -> Prop_disj (List.map (propositionalize ~keep_terms) fs)

  | Prop_modal (m, p, (f, opt)) ->
      let p' = propositionalize_lpath ~keep_terms p
      and f' = propositionalize ~keep_terms f
      in Prop_modal (m, p', (f', opt))

  | _ -> failwith "[propositionalize]"

and propositionalize_eq ?(keep_terms = false) (e1 : int term) (e2 : int term) =
  match e1, e2 with
  | Tm_const (c1, Ty_nat _), Tm_const (c2, Ty_nat _) ->
      (* c1 = c2 *)
      Prop_atomic (string_of_bool (c1 = c2))

  | Tm_var (x1, Ty_nat n1), Tm_const (c2, Ty_nat _) when n1 <= c2 ->
      (* x1 = c2 *)
      assert (n1 <= c2);
      Prop_atomic "false"
  | Tm_var (x1, Ty_nat n1), Tm_const (c2, Ty_nat n2) when keep_terms ->
      Prop_equal (e1, e2)
  | Tm_var (x1, Ty_nat n1), Tm_const (c2, Ty_nat n2) ->
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
  | Tm_const _, Tm_var _ ->
      (* c1 = x2 *)
      propositionalize_eq e2 e1

  | Tm_var (x1, Ty_nat n1), Tm_var (x2, Ty_nat n2) when x1 = x2 ->
      (* x1 = x2 *)
      Prop_atomic "true"
 (*
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
  *)

  | _ ->
      (* general case *)
      let alist = split (Prop_equal (e1, e2))
      in let qs =
	List.map
	  (fun (env, q) ->
	    (* for each case (env, q) in alist *)
	    let binds, in_range =
	      List.fold_left
		(fun (rslt, in_range) (x, (Ty_nat n, n')) ->
		  (* (x, (ty, n') -> x = n' *)
		  let eq = Prop_equal (Tm_var (x, Ty_nat n), Tm_const (n', Ty_nat n'))
		  in
		  rslt @ [if keep_terms then eq else propositionalize eq],
		  in_range && n > n')
		([], true) env
	    in let q' =
	      if in_range
	      then propositionalize ~keep_terms (simp q)
	      else Prop_atomic "false"
	    in Prop_disj [Prop_neg (Prop_conj binds); q'])
	  alist
      in Prop_conj qs

and propositionalize_lpath ?(keep_terms = false) (p, l_opt) =
  (propositionalize_path ~keep_terms p), l_opt

and propositionalize_path ?(keep_terms = false) p =
  match p with
  | Path_prop f -> Path_prop (propositionalize ~keep_terms f)
  | Path_seq ps -> Path_seq (List.map (propositionalize_lpath ~keep_terms) ps)
  | Path_sum ps -> Path_sum (List.map (propositionalize_lpath ~keep_terms) ps)
  | Path_test f -> Path_test (propositionalize ~keep_terms f)
  | Path_star p' -> Path_star (propositionalize_lpath ~keep_terms p')
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

  | Prop_equal (Tm_op ("<", [e1; e2]), Tm_const (b, Ty_nat _)) ->
      (* comparison *)
      assert (b = 0 || b = 1);
      print_term out e1;
      out (if b = 1 then " < " else " >= ");
      print_term out e2
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
  (*| Prop_label _ -> 0*)

and print_labelled_path out (r, l_opt) =
  match l_opt with
  | None -> print_path out r
  | Some l ->
      out l; out ":";
      let _ =
	match r with
	| Path_prop _ ->
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

  | _ -> failwith ("[print_path] " ^ show_path r)

(* precedence: grouping (()) < *, ? < concat (;) < choice (+) *)
and path_prec = function
  | Path_prop _ -> 0
  | Path_seq rs -> 100
  | Path_sum rs -> 200
  | Path_test f -> 50
  | Path_star r -> 30
(*| Path_label _ -> 0*)

(** pretty-printing (string conversion) *)

let string_of_property p =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  print_property concat p;
  !str

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
