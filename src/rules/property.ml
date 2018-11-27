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
  | Tm_val : 'a * 'a term_t -> 'a term
  | Tm_var : string * 'a term_t -> 'a term
  | Tm_app : ('a -> 'b) term * 'a term -> 'b term
  | Tm_abs : ('a term -> 'b term) -> ('a -> 'b) term
  | Tm_bop : string -> ('a -> 'a -> 'a) term
  | Tm_eq : 'a term * 'a term -> bool term

and _ term_t =
  | Ty_prop : bool term_t
  | Ty_nat : int -> int term_t
  | Ty_fun : 'a term_t * 'b term_t -> ('a -> 'b) term_t

(* ppx does not support gadt *)

(* term ops *)

let equal_term tm1 tm2 = failwith "[equal_term]"

let eval_term env tm =
  failwith "[eval_term]"

let term_to_propositions = function
  | Tm_var (x, Ty_nat n) ->
      let len = float_of_int n in
      let nbit = int_of_float @@ ceil (log len /. log 2.0) in
      List.init nbit (fun i -> "_" ^ x ^ "_" ^ string_of_int i)
  | _ -> invalid_arg "[term_to_propositions]"

(** pretty-printing *)

let rec print_term (out : string -> unit) (e : int term) =
  match e with
  | Tm_val (n, Ty_nat _) ->
      out (string_of_int n)
  | Tm_var (x, _) ->
      out x
  | Tm_app (Tm_app (Tm_bop bop, e1), e2) ->
      out "(";
      print_term out e1;
      out (" " ^ bop ^ " ");
      print_term out e2;
      out ")";
  | _ -> failwith "[print_term]"

(** pretty-printing -- ppx-compliant *)

let rec pp_term (pp : Format.formatter -> int -> unit) (fmt : Format.formatter) (tm : int term) =
  match tm with
  | Tm_val (n, Ty_nat _) -> Format.pp_print_int fmt n
  | Tm_var (x, Ty_nat _) -> Format.pp_print_string fmt x
  | Tm_app (Tm_app (Tm_bop bop, e1), e2) ->
      Format.pp_print_string fmt "(";
      pp_term pp fmt e1;
      Format.pp_print_string fmt (" " ^ bop ^ " ");
      pp_term pp fmt e2;
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

let rec propositionalize f =
  match f with
  | Prop_atomic _ -> f
  | Prop_equal (e1, e2) ->
      propositionalize_eq e1 e2
  | Prop_neg f' -> Prop_neg (propositionalize f')
  | Prop_conj fs -> Prop_conj (List.map propositionalize fs)
  | Prop_disj fs -> Prop_disj (List.map propositionalize fs)
  | Prop_modal (m, p, (f, opt)) ->
      let p' = propositionalize_lpath p and f' = propositionalize f
      in Prop_modal (m, p', (f', opt))
  | _ -> failwith "[propositionalize]"

and propositionalize_eq e1 e2 =
  match e1, e2 with
  | Tm_val _, Tm_val _ | Tm_var _, Tm_val _ | Tm_val _, Tm_var _ | Tm_var _, Tm_var _ ->
      propositionalize_eq_rec e1 e2
  | _ -> failwith "[propositionalize_eq]"

and propositionalize_eq_rec e1 e2 =
  match e1, e2 with
  | Tm_val (v1, Ty_nat n1), Tm_val (v2, Ty_nat n2) ->
      (* v1 = v2 *)
      Prop_atomic (string_of_bool (v1 = v2))

  | Tm_var (x1, Ty_nat n1), Tm_val (v2, Ty_nat n2) when n1 > v2 ->
      (* x1 = v2 *)
      let xs : string list = term_to_propositions e1 in
      let nbit = int_of_float @@ ceil (log (float_of_int n1) /. log 2.0) in
      let bits : bool list = gen_bits nbit v2 in
      let props, _  =
	List.fold_left
	  (fun (rslt, i) x ->
	    let f =
	      if List.nth bits i then Prop_atomic x else Prop_neg (Prop_atomic x)
	    in (rslt @ [f]), i + 1)
	  ([], 0) xs
      in Prop_conj props
  | Tm_var (x1, Ty_nat n1), Tm_val (v2, Ty_nat n2) ->
      assert (n1 <= v2);
      Prop_atomic "false"
  | Tm_val _, Tm_var _ ->
      (* v1 = x2 *)
      propositionalize_eq_rec e2 e1

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
  | Tm_var _, Tm_var _ ->
      propositionalize_eq_rec e2 e1

  | Tm_var (x1, _), Tm_app (Tm_app (Tm_bop "+", Tm_var (x2, _)), Tm_val (v3, Ty_nat _)) when x1 = x2 ->
      (* x = x + v *)
      Prop_atomic (string_of_bool (v3 = 0))
      
  | Tm_var (x1, Ty_nat n1), Tm_app (Tm_app (Tm_bop "+", Tm_var (x2, Ty_nat n2)), Tm_val (v3, Ty_nat n3)) when n1 > v3 ->
      failwith "[propositionalize_eq_rec] not yet supported"
  | Tm_var (_, Ty_nat n1), Tm_app (Tm_app (Tm_bop "+", Tm_var _), Tm_val (v3, _)) ->
      assert (n1 <= v3);
      Prop_atomic "false"

  | _ -> failwith "[propositionalize_eq_rec]"

and gen_bits nbit (n : int) =
  gen_bits_rec nbit n [] 0

and gen_bits_rec nbit (n : int) rslt i =
  if i = nbit then
    rslt
  else
    gen_bits_rec nbit n (rslt @ [n land (1 lsl i) <> 0]) (i + 1)

and propositionalize_lpath (p, l_opt) =
  (propositionalize_path p), l_opt

and propositionalize_path p =
  match p with
  | Path_prop f -> Path_prop (propositionalize f)
  | Path_seq ps -> Path_seq (List.map propositionalize_lpath ps)
  | Path_sum ps -> Path_sum (List.map propositionalize_lpath ps)
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
