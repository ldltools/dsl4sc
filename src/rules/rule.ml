(* $Id: rule.ml,v 1.3 2018/01/09 07:54:44 sato Exp $ *)
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
type _ term_t =
  | Ty_int : int term_t
  | Ty_Arrow : 'a term_t * 'b term_t -> ('a -> 'b) term_t

type _ term =
  | Tm_val : 'a -> 'a term
  | Tm_var : string * 'a term_t -> 'a term
  | Tm_abs : string * 'a term_t * 'b term -> ('a -> 'b) term
  | Tm_app : ('a -> 'b) term * 'a term -> 'b term

let equal_term tm1 tm2 = failwith "equal_term"

(** pretty-printing *)

let pp_term fmt tm =
  failwith "pp_term"

let term_of_yojson json =
  failwith "term_of_yojson"

let term_to_yojson tm =
  failwith "term_to_yojson"

(** protocol *)
type protocol =
  | Proto_prop of protocol_prop
  | Proto_seq of protocol list
  | Proto_sum of protocol list
  | Proto_test of protocol
  | Proto_star of protocol

and protocol_prop =
  | PProp_event of string
  | PProp_event_elt of string * int term list
  | PProp_neg of protocol_prop

[@@deriving show, yojson, eq]

(** property *)
type property =
  | Prop_atomic of string
  | Prop_atomic_elt of string * int term list
  | Prop_neg of property
  | Prop_conj of property list
  | Prop_disj of property list
  | Prop_modal of modality * labelled_path * labelled_property
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
  | Path_label of string

and labelled_path =
    path * label option

and label =
    string

[@@deriving show, yojson, eq]

(** rule *)
type rule =
    { event : event * string option;
      condition : labelled_property * string option;
      action : action * string option;
      path : labelled_path option;
    }

(** event *)
and event =
  | Ev_name of string
  | Ev_name_seq of string list
  | Ev_name_seq_compl of string list

(** action *)
and action =
    path option * action_unit list

and action_unit =
  | Act_ensure of property
  | Act_raise of string list (* nondeterministic choice *)
  | Act_preserve of string list

[@@deriving show, yojson, eq]

type t = rule

let rec propositional = function
  | Prop_atomic _ -> true
  | Prop_neg p -> propositional p
  | Prop_conj ps | Prop_disj ps ->
      List.fold_left (fun rslt p -> rslt && propositional p) true ps
  | Prop_modal _ -> false
  | Prop_label _ -> failwith "propositional"

let event_name ev =
  match ev with
  | Ev_name e -> e
  | Ev_name_seq [e] -> e
  | _ -> failwith ("event_name: " ^ show_event ev)

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

  | _ -> failwith ("print_property_rec: " ^ show_property f)

(* precedence: neg < and < lor < implies *)
and prec = function
  | Prop_atomic _ -> 0
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

  | _ -> failwith ("print_path: " ^ show_path r)

(* precedence: grouping (()) < *, ? < concat (;) < choice (+) *)
and path_prec = function
  | Path_prop _ -> 0
  | Path_seq rs -> 100
  | Path_sum rs -> 200
  | Path_test f -> 50
  | Path_star r -> 30
  | Path_label _ -> 0

let rec print_protocol out ?(fancy=false) (p : protocol) =
  print_protocol_rec out ~fancy p

and print_protocol_rec out ?(fancy=false) (p : protocol) =
  match p with
  | Proto_prop f -> print_protocol_prop out ~fancy f

  (* seq *)
  | Proto_seq [p'] when proto_prec p' <= proto_prec p ->
      print_protocol out p'
  | Proto_seq [p'] ->
      out "("; print_protocol out p'; out ")"
  | Proto_seq (p' :: ps) when proto_prec p' <= proto_prec p ->
      print_protocol out p';
      out "; ";
      print_protocol out (Proto_seq ps)
  | Proto_seq (p' :: ps) ->
      out "("; print_protocol out p'; out ")";
      out "; ";
      print_protocol out (Proto_seq ps)

  (* sum *)
  | Proto_sum [p'] when proto_prec p' <= proto_prec p ->
      print_protocol out p'
  | Proto_sum [p'] ->
      out "("; print_protocol out p'; out ")"
  | Proto_sum (p' :: ps) when proto_prec p' <= proto_prec p ->
      print_protocol out p';
      out " + ";
      print_protocol out (Proto_sum ps)
  | Proto_sum (p' :: ps) ->
      out "("; print_protocol out p'; out ")";
      out " + ";
      print_protocol out (Proto_sum ps)

  (* test *)
  | Proto_test p' when proto_prec p' <= proto_prec p ->
      print_protocol out p'; out "?";
  | Proto_test p' ->
      out "("; print_protocol out p'; out ")?";

  (* star *)
  | Proto_star p' when proto_prec p' <= proto_prec p ->
      print_protocol out ~fancy p'; out "*"
  | Proto_star p' ->
      out "("; print_protocol out ~fancy p'; out ")*"

  | _ ->
      failwith "print_protocol_rec"

and print_protocol_prop out ?(fancy=false) (f : protocol_prop) =
  match f with
  | PProp_event ev -> out ev
  | PProp_neg f' -> out "!"; print_protocol_prop out ~fancy f'
  | _ ->
      failwith "print_protocol_prop"

(* precedence: grouping (()) < *, ? < concat (;) < choice (+) *)
and proto_prec = function
  | Proto_prop _ -> 0
  | Proto_seq _  -> 100
  | Proto_sum _  -> 200
  | Proto_test _ -> 50
  | Proto_star _ -> 30

let rec print_rule out ?(fancy=false) (r : rule) =
  out "on ";
  let ev : event = fst r.event in
  let _ =
    match ev with
    | Ev_name e -> out e;
    | Ev_name_seq (e :: rest) ->
	out e; List.iter (fun e -> out ", "; out e) rest
    | Ev_name_seq_compl (e :: rest) ->
	out "^(";
	out e; List.iter (fun e -> out ", "; out e) rest;
	out ")"
    | _ -> failwith "print_rule"
  in ();
  out " ";

  out "when ";
  let p, p_opt = r.condition in
  print_labelled_property out p;
  let _ =
    match p_opt with Some str -> out " {"; out str; out "}" | _ -> ()
  in
  out " ";

  out "do ";
  let a, a_opt = r.action in
  print_action out a;
  let _ =
    match a_opt with Some str -> out " {"; out str; out "}" | _ -> ()
  in

  ()

and print_action out = function
  | None, [] ->
      print_action1 out (Act_ensure (Prop_atomic "true"))
  | None, [act] ->
      print_action1 out act
  | None, acts when List.length acts > 1 ->
      out "(";
      print_action1 out (List.hd acts);
      List.iter	(fun act -> out ", "; print_action1 out act) (List.tl acts);
      out ")"
  | Some _, acts ->
      failwith "not yet implemented";
      print_action out (None, acts)
  | _ ->
      failwith "print_action"

and print_action1 out = function
  | Act_ensure p ->
      out "ensure "; print_property out p
  | Act_raise (e :: es) ->
      out "raise "; out e;
      List.iter (fun e' -> out @@ " + " ^ e') es
  | Act_preserve ps ->
      out "preserve ";
      assert (List.length ps > 0);
      out (List.hd ps);
      List.iter (fun p -> out (", " ^ p)) (List.tl ps)
  | _ ->
      failwith "print_action1"

(** pretty-printing (string conversion) *)

let to_string (prn : (string -> unit) -> 'a -> unit) (x : 'a) =
  let str = ref "" in
  let concat str' = str := !str ^ str' in
  prn concat x;
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

let string_of_action =
  to_string print_action

let string_of_protocol =
  to_string (print_protocol ~fancy:false)
