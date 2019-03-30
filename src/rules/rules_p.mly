// $Id: rules_p.mly,v 1.4 2018/05/09 00:45:25 sato Exp sato $
//
// (C) Copyright IBM Corp. 2018.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

%{
open Protocol
open Property
open Rule
open Printf

type rule_elt =
  | Elt_event of event
  | Elt_event_opt of string
  | Elt_condition of labelled_property
  | Elt_condition_opt of string
  | Elt_action of action
(*| Elt_action_opt of string*)

let rec genrule (elts : rule_elt list) =
  let e : event = Ev_name ""
  and c : labelled_property = Prop_atomic "true", None
  and a : action = [] in
  let e', c', a' = genrule_rec ((e, None), (c, None), a) elts
  in { event = e'; condition = c'; action = a'; }

and genrule_rec ((e, ex), (c, cx), a) elts =
  if elts = [] then ((e, ex), (c, cx), a) else
  let rslt =
    match List.hd elts with
    | Elt_event e'          -> (e', ex), (c, cx), a
    | Elt_event_opt str     -> (e, Some str), (c, cx), a
    | Elt_condition c'      -> (e, ex), (c', cx), a
    | Elt_condition_opt str -> (e, ex), (c, Some str), a
    | Elt_action a'         -> (e, ex), (c, cx), a'
    (*| Elt_action_opt str    -> (e, ex), (c, cx), (a, Some str), r*)
    | _ -> failwith "genrule_rec"
  in genrule_rec rslt (List.tl elts)

(* deprecated *)
let rec expand_prop_spec prefix ranges =
  expand_prop_spec_rec [prefix ^ "_"] ranges

and expand_prop_spec_rec rslt = function
  | [] -> rslt
  | n :: rest ->
      assert (0 < n);
      let rec expand1 prefix n rslt i =
	if i = n
	then rslt
	else expand1 prefix n (rslt @ [prefix ^ "_" ^ string_of_int i]) (i + 1) in
      let rslt' = List.fold_left (fun rslt prefix -> expand1 prefix n rslt 0) [] rslt
      in
      expand_prop_spec_rec rslt' rest

let report_error (msg : string) =
  failwith @@ "[Rules_p] parse error: " ^ msg

%}

%start	rules decl_seq
%type<Rules.rules> rules
%type<Rules.decl list> decl_seq

%token	EVENT
%token	PROTOCOL
%token	VARIABLE
%token	PROPERTY
%token	SCRIPT

%token	RULE
%token	ON WHEN DO
%token	RAISE ENSURE
%token	PRESERVE EXCEPT

// modified
%token	<int> CONST
%token	<string> NAME
%token	<string> STRING
//%token	<string * string list> NAME_WITH_ARGS

%left	IMPLIES
%left	GT LT GE LE
%right	NOT EXCLAM
%left	STAR
%left	QUESTION
%left	OR
%left	AND

%token	NOT
%token	OR
%token	AND
%token	IMPLIES

%token	EQUAL
%token	NE GT LT GE LE

%token	TILDE
%token 	EXCLAM HAT
%token	PLUS MINUS
%token	QUESTION
%token	STAR SLASH

// extra tokens
%token	LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token	SEMI COLON COMMA AT
%token	SEMISEMI
%token	EOF

%%

// ================================================================================
// rules (top-level)
// ================================================================================
rules	: decl_seq
	  { Rules.decls_to_rules $1 }
	;

decl_seq
	: decl_seq1 EOF
	  { $1 }
	;

decl_seq1
	: decl
	  { $1 }
	| decl_seq1 decl
	  { $1 @ $2 }
	;

decl	: EVENT event_spec_seq
	  { List.map (fun s -> Rules.Decl_event s) $2 }
	| PROTOCOL protocol_spec_seq
	  // protocols enclosed by braces
	  { List.map (fun s -> Rules.Decl_protocol s) $2 }
//	| PROTOCOL protocol_seq
//	  // naked protocols
//	  { List.map (fun p -> Rules.Decl_protocol (None, p)) $2 }

	| VARIABLE var_spec_seq
	  { List.map
	      (fun ((name, t), code) ->
		match t with
		| Rules.VT_prop -> Rules.Decl_variable ((name, Rules.VT_prop), code)
		| _ -> Rules.Decl_variable ((name, t), code))
	      $2 }
	| PROPERTY property_spec_seq
	  // properties enclosed by braces
	  { List.map (fun s -> Rules.Decl_property s) $2 }
//	| PROPERTY property_seq
//	  // naked properties
//	  { List.map (fun p -> Rules.Decl_property (None, p)) $2 }

	| RULE rule_spec_seq
	  // rules enclosed by braces
	  { List.map (fun s -> Rules.Decl_rule s) $2 }

	| SCRIPT LBRACE STRING RBRACE
	  { [Rules.Decl_extra $3] }

// **conflict
//	| error
//	  { report_error "decl" }
	;

// ================================================================================
// event
// ================================================================================

event_spec_seq
	: event_spec_seq1
	  { $1 }
	| event_spec_seq SEMI event_spec_seq1
	  { $1 @ $3 }
	| event_spec_seq SEMI
	  { $1 }
	;

event_spec_seq1
	: event_spec
	  { $1 }
	| event_spec_seq1 COMMA event_spec
	  { $1 @ $3 }
	;

event_spec
	: NAME
	  { [$1, None] }
	| NAME LBRACE STRING RBRACE
	  { [$1, Some $3] }
	;

// ================================================================================
// protocol
// ================================================================================
protocol_spec_seq
	: protocol_spec
	  { [$1] }
	| protocol_spec_seq protocol_spec
	  { $1 @ [$2] }
	;

protocol_spec
	: protocol_or_pcall SEMISEMI
	  { $1 }

//	// deprecated
//	| NAME LPAREN param_seq RPAREN LBRACE protocol RBRACE
//	  { let args =
//	      List.map (function Tm_var (x, _) -> x | _ -> report_error "protocol_spec") $3
//	    in Some ($1, args), $6 }
//	| LBRACE protocol_or_pcall RBRACE
//	  { $2 }
	;

protocol_or_pcall
	: protocol
	  { $1 }

	// deprecated
	| NAME LPAREN param_seq RPAREN
	  { report_error "protocol_or_pcall" }
	;

/*
// naked protocols
protocol_seq
	: protocol_or_pcall
	  { [$1] }
	| protocol_seq SEMISEMI protocol_or_pcall 
	  { $1 @ [$3] }
	// ** the following rule causes a shift/reduce conflict (harmless)
	| protocol_seq SEMISEMI
	  { $1 }
	;
*/

// --------------------------------------------------------------------------------
// protocol
// --------------------------------------------------------------------------------
// precedence: grouping (()) < *, ? < concat (;) < choice (+)

protocol
	: protocol1
	  { $1 }
	| protocol PLUS protocol1
	  { Proto_sum [$1; $3] }
	| error
	  { report_error "protocol" }
	;

protocol1
	: protocol2
	  { $1 }
	| protocol1 SEMI protocol2
	  { match $1, $3 with
	    | Proto_seq s, _ -> Proto_seq (s @ [$3])
	    | _, Proto_seq s -> Proto_seq ($1 :: s)
	    | _ -> Proto_seq [$1; $3]
	  }
	;

protocol2
	: protocol3
	  { $1 }
	| protocol3 QUESTION
	  { Proto_sum [$1; Proto_event "_epsilon"] }
//	| protocol3 QUESTION protocol3
//	  { Proto_seq [Proto_sum [$1; Proto_event "_epsilon"]; $3] }
	| protocol3 STAR
	  { Proto_star $1 }
	;

protocol3
	: NAME
	  { match $1 with "_empty" -> Proto_empty | _ -> Proto_event $1 }
//	| neg NAME
//	  { Proto_prop (PProp_neg (PProp_event $2)) }
//	| neg protocol3
//	  { assert (match $2 with Proto_event _ -> true | _ -> false);
//	    Proto_neg $2 }
	| LPAREN protocol RPAREN
	  { $2 }
	;	

/*
protocol_prop
	: NAME
	  { PProp_event $1 }
	| neg protocol_prop
	  { PProp_neg $2 }
	;	
*/

// ================================================================================
// variable
// ================================================================================
var_spec_seq
	: var_spec_seq1
	  { $1 }
	| var_spec_seq var_spec_seq1
	  { $1 @ $2 }
//	| var_spec_seq SEMI
//	  { $1 }
	| error
	  { report_error "variable_spec" }
	;

// variables of the same type
// x1, x2, ...
// x1, x2, ... : t
var_spec_seq1
	: var_spec_seq2 SEMI
	  { List.map (fun (name, code) -> (name, Rules.VT_prop), code) $1 }
	| var_spec_seq2 COLON var_type SEMI
	  { List.map (fun (name, code) -> (name, $3), code) $1 }
	;

var_spec_seq2
	: var_spec
	  { $1 }
	| var_spec_seq2 COMMA var_spec
	  { $1 @ $3 }
	;

// single variable specification
var_spec
	: NAME
	  { [$1, None] }
//	| NAME range_seq
//	  { List.map (fun x -> x, None) (expand_prop_spec $1 $2) }
	| NAME LBRACE STRING RBRACE
	  { [$1, Some $3] }
	;

var_type
	: NAME
	  { match $1 with
	    | "prop"   -> Rules.VT_prop
	    | "bool"   -> Rules.VT_term (Ty_nat 2)
	    | "bit"    -> Rules.VT_term (Ty_nat 2)
	    | "nibble" -> Rules.VT_term (Ty_nat 16)
	    | "byte"   -> Rules.VT_term (Ty_nat 256)
	    | "nat"    -> Rules.VT_term (Ty_nat 16)
	    | _ -> failwith ("[parsing] unknown type: " ^ $1)
	  }
	| NAME LPAREN CONST RPAREN
	  // range type
	  { match $1 with
	    | "nat" when 0 < $3 && $3 <= 256 -> Rules.VT_term (Ty_nat $3)
	    | "nat" -> invalid_arg (sprintf "nat %d : out of range" $3)
	    | _ -> failwith ("[parsing] unknown type: " ^ $1)
	  }
	;

// ================================================================================
// property
// ================================================================================

// --------------------------------------------------------------------------------
// property_spec
// --------------------------------------------------------------------------------
property_spec_seq
	: property_spec
	  { [$1] }
	| property_spec_seq property_spec
	  { $1 @ [$2] }
	;

property_spec
	: property_or_pcall SEMI
	  { $1 }

//	// deprecated
//	| NAME LPAREN param_seq RPAREN LBRACE labelled_property RBRACE
//	  { let args =
//	      List.map (function Tm_var (x, _) -> x | _ -> report_error "protocol_spec") $3
//	    in Some ($1, args), $6 }
//	| LBRACE property_or_pcall RBRACE
//	  { $2 }
	;

property_or_pcall
	: property
	  { $1 }
//	| NAME LPAREN param_seq RPAREN
//	  { report_error "property_or_pcall" }
	;

/*
// naked properties
property_seq
	: property_or_pcall
	  { [$1] }
	| property_seq SEMI labelled_property
	  { $1 @ [$3] }
	// ** the following rule causes a shift/reduce conflict (harmless)
	| property_seq SEMI
	  { $1 }
	;
*/

param_seq
	:
	  { [] }
	| param_seq1
	  { $1 }
	;

param_seq1
	: term
	  { [$1] }
	| param_seq1 COMMA CONST
	  { $1 @ [Tm_const ($3, Ty_nat 255)] }
	;

/*
args	:
	  { [] }
	| args1
	  { $1 }
	;

args1	: NAME
	  { [$1] }
	| args1 COMMA NAME
	  { $1 @ [$3] }
	;

intparam_seq
	:
	  { [] }
	| intparam_seq1
	  { $1 }
	;

intparam_seq1
	: CONST
	  { [$1] }
	| intparam_seq1 COMMA CONST
	  { $1 @ [$3] }
	;
*/

// --------------------------------------------------------------------------------
// property
// --------------------------------------------------------------------------------
// precedence: neg < and < lor < implies

labelled_property
	: property
	  { $1, None }
	| label_def property3
	  { $2, Some $1 }
	;

property
	: property0
	  { $1 }
	| property0 IMPLIES property
	  { Prop_disj [Prop_neg $1; $3] }
	| error
	  { report_error "property" }
	;

property0
	: property1
	  { $1 }
	| property0 OR property1
	  { match $1 with Prop_disj s -> Prop_disj (s @ [$3]) | _ -> Prop_disj [$1; $3] }
	;

property1
	: property2
	  { $1 }
	| property1 AND property2
	  { match $1 with Prop_conj s -> Prop_conj (s @ [$3]) | _ -> Prop_conj [$1; $3] }
//	| property1 AMPERSAND property2
//	  { match $1 with Prop_conj s -> Prop_conj (s @ [$3]) | _ -> Prop_conj [$1; $3] }
	;

property2
	: property3
	  { $1 }
	| modal_property
	  { $1 }
	;

property3
	: NAME
	  { Prop_atomic $1 }
	| neg property3
	  { Prop_neg $2 }
	| LPAREN property RPAREN
	  { $2 }
	| property3_equal
	  { $1 }
	;

property3_equal
	: term EQUAL term
	  { Prop_equal ($1, $3) }
	| term NE term
	  { Prop_neg (Prop_equal ($1, $3)) }
	| term LT term
	  // e1 < e2
	  { Prop_equal (Tm_op ("<", [$1; $3]), Tm_const (1, Ty_nat 2)) }
	| term GT term
	  // e1 > e2 => e2 < e1
	  { Prop_equal (Tm_op ("<", [$3; $1]), Tm_const (1, Ty_nat 2)) }
	| term LE term
	  // e1 <= e2 => !(e2 < e1)
	  { Prop_equal (Tm_op ("<", [$3; $1]), Tm_const (0, Ty_nat 2)) }
	| term GE term
	  // e1 >= e2 => !(e1 < e2)
	  { Prop_equal (Tm_op ("<", [$1; $3]), Tm_const (0, Ty_nat 2)) }
	;

property3_labelled
	: property3
	  { $1, None }
	| label_def property3
	  { $2, Some $1 }
	;

// Prop_modal of modality * labelled_path * labelled_property
modal_property
	: modal_path property3_labelled
	  { Prop_modal (fst $1, snd $1, $2) }
	| modal_path modal_property
	  { Prop_modal (fst $1, snd $1, ($2, None)) }
//	| modal_path LPAREN property RPAREN
//	  { Prop_modal (fst $1, snd $1, $3) }
	;

modal_path
	: LT labelled_ldl_path GT
	  { (Mod_ex, $2) }
	| LBRACK labelled_ldl_path RBRACK
	  { (Mod_all, $2) }

// special cases: <> = <{true}*>, [] = [{true}*]
	| LT GT
	  { Mod_ex, (Path_star (Path_prop (Prop_atomic "true"), None), None) }
	| LBRACK RBRACK
	  { Mod_all, (Path_star (Path_prop (Prop_atomic "true"), None), None) }
	;

// negation operator
neg	: NOT
	    {}
	| EXCLAM
	    {}
	| TILDE
	    {}
	;

/*
index_seq
	: index
	  { [$1] }
	| index_seq index
	  { $1 @ [$2] }
	;

index	: LBRACK term RBRACK
	  { $2 }
	;
*/

// --------------------------------------------------------------------------------
// term
// --------------------------------------------------------------------------------

term	: sum_term
	  { $1 }
	| error
	  { report_error "term" }
	;

sum_term
	: product_term
	  { $1 }
	| sum_term PLUS product_term
	  { Tm_op ("+", [$1; $3]) }
	| sum_term MINUS product_term
	  { Tm_op ("-", [$1; $3]) }
	;

product_term
	: factor
	  { $1 }
	| product_term STAR factor
	  { Tm_op ("*", [$1; $3]) }
	| product_term SLASH factor
	  { Tm_op ("/", [$1; $3]) }
	;

factor	: atomic_term
	  { $1 }
	| LPAREN sum_term RPAREN
	  { $2 }
	;

atomic_term
	: CONST
	  { if $1 < 0 || $1 > 255 then failwith ("[parsing] out of range: " ^ (string_of_int $1));
	    Tm_const ($1, Ty_nat 256) }
	| NAME
	  { Tm_var ($1, Ty_nat 16) }
	;

// --------------------------------------------------------------------------------
// ldl_path
// --------------------------------------------------------------------------------
// precedence: grouping (()) < *, ? < concat (;) < choice (+)

// labelled_path = path * label option
labelled_ldl_path
	: path0_or_1_or_2
	  { $1, None }
	| label_def path3
	  { $2, Some $1 }
	| path3 label_def_suffix
	  { $1, Some $2 }
	;

path0_or_1_or_2
	: path0
	  { $1 }
	| path1
	  { $1 }
	| path2
	  { $1 }
	;

// PLUS
path0	: labelled_path1_or_2 PLUS labelled_path1_or_2
	  { Path_sum [$1; $3] }
	| path0 PLUS labelled_path1_or_2
	  { Path_sum (((function Path_sum ps -> ps) $1) @ [$3]) }
	| error
	  { report_error "path0" }
	;

path1_or_2
	: path1
	  { $1 }
	| path2
	  { $1 }
	;

// SEMI
path1	: labelled_path2 SEMI labelled_path2
	  { Path_seq [$1; $3] }
	| path1 SEMI labelled_path2
	  { match $1 with Path_seq s -> Path_seq (s @ [$3]) }
	;

// TEST/STAR
path2	: path3
	  { $1 }
	| LBRACE property RBRACE QUESTION
	  { Path_test $2 }
	| LBRACE property RBRACE QUESTION labelled_path3
	  { Path_seq [Path_test $2, None; $5] }
	| labelled_path3 STAR
	  { Path_star $1  }
	;

// proposition
path3	: LBRACE property RBRACE
	  { Path_prop $2 }
	| LPAREN path0_or_1_or_2 RPAREN
	  { $2 }

//	// label
//	| label_use
//	  { Path_label $1 }
	;	

labelled_path1_or_2
	: path1_or_2
	  { $1, None }
	| label_def path3
	  { $2, Some $1 }
	| path3 label_def_suffix
	  { $1, Some $2 }
	;

labelled_path2
	: path2
	  { $1, None }
	| label_def path3
	  { $2, Some $1 }
	| path3 label_def_suffix
	  { $1, Some $2 }
	;

labelled_path3
	: path3
	  { $1, None }
	| label_def path3
	  { $2, Some $1 }
	| path3 label_def_suffix
	  { $1, Some $2 }
	;

label_def
	: NAME COLON
	  { $1 }
	;

label_def_suffix
	: AT NAME
	  { $2 }
	| HAT NAME
	  { $2 }
	;

/*
label_use
	: DOLLAR NAME
	  { $2 }
	| AT NAME
	  { $2 }
	;
*/

// ================================================================================
// rule
// ================================================================================
rule_spec_seq
	: rule_spec
	  { [$1] }
	| rule_spec_seq rule_spec
	  { $1 @ [$2] }
	;

rule_spec
	: rule
	  { genrule $1, None }
	| rule_spec SEMI
	  { $1 }

	// deprecated
/*
	| LBRACE rule RBRACE
	  { None, genrule $2 }
	| NAME LPAREN param_seq RPAREN LBRACE rule RBRACE
	  { let args =
	      List.map (function Tm_var (x, _) -> x | _ -> report_error "rule_spec") $3
	    in Some ($1, args), genrule $6 }
*/
	;

/*
rule_or_rcall
	: rule
	  { $1 }
	| NAME LPAREN param_seq RPAREN
	  { report_error "rule_or_rcall" }
	;
*/

/*
// naked rules
rule_seq
	: rule
	  { [genrule $1] }
	| rule_seq SEMI rule
	  { $1 @ [genrule $3] }
	| rule_seq SEMI
	  { $1 }
	;
*/

// --------------------------------------------------------------------------------
// eca rule
// --------------------------------------------------------------------------------
rule	: ON rule_e WHEN rule_c rule_a
	  { $2 @ $4 @ $5 }
	| ON rule_e rule_a
	  // condition = true
	  { $2 @ $3 }

// special case
	| preserve_rule_except
	  { $1 }
	;

// event/condition/action
rule_e	: args
	  { List.map (fun arg -> Elt_event (Ev_name arg)) $1 }
	| args LBRACE STRING RBRACE
	  { (List.map (fun arg -> Elt_event (Ev_name arg)) $1) @ [Elt_event_opt $3] }
	;

args	: NAME
	  { [$1] }
	| args COMMA NAME
	  { $1 @ [$3] }
	;

rule_c	: labelled_property
	  { [Elt_condition $1] }
	| labelled_property LBRACE STRING RBRACE
	  { [Elt_condition $1; Elt_condition_opt $3] }
	| LBRACE STRING RBRACE
	  { [Elt_condition_opt $2] }
	;

rule_a	: action_seq
	  { [Elt_action $1] }
	| preserve_rule_a
	  { $1 }
	;

// ------
// action
// ------
	    
action_seq
	: action
	  { [$1] }
	| action_seq action
	  { $1 @ [$2] }
	;
	    
action	: action1
	  { $1, None }
	| action1 LBRACE STRING RBRACE
	  { $1, Some $3 }
	| DO LBRACE STRING RBRACE
	  { Act_do, Some $3 }
	;

action1	: action_ensure
	  { $1 }
	| action_raise
	  { $1 }
//	| DO action1
//	  { $2 }
	;

// -------------
// action_ensure
// -------------
action_ensure
	: ENSURE property
	  { Act_ensure $2 }
	;

/*
action_ensure
	: ENSURE state
	  { Act_ensure $2 }
	;

// post-state proposition
state	: state0
	  { $1 }
	| state0 IMPLIES state
	  { Prop_disj [Prop_neg $1; $3] }
	| error
	  { report_error "state" }
	;

state0	: state1
	  { $1 }
	| state0 OR state1
	  { match $1 with Prop_disj s -> Prop_disj (s @ [$3]) | _ -> Prop_disj [$1; $3] }
	;

state1	: state2
	  { $1 }
	| state1 AND state2
	  { match $1 with Prop_conj s -> Prop_conj (s @ [$3]) | _ -> Prop_conj [$1; $3] }
	;

state2	: NAME
	  { Prop_atomic $1 }
	| term EQUAL term
	  { Prop_equal ($1, $3) }
	| neg state2
	  { Prop_neg $2 }
	| LPAREN state RPAREN
	  { $2 }
//	| label_use
//	  { Prop_label $1 }
	;
*/

// ------------
// action_raise
// ------------
action_raise
	: RAISE event_sum
	  { Act_raise $2 }
	;

event_sum
	: event_sum1
	  { $1 }
	| event_sum PLUS event_sum1
	  { $1 @ $3 }
	;

event_sum1
	: NAME
	  { [$1] }
	| LPAREN event_sum RPAREN
	  { $2 }
	;

// ----------------------
// special case: preserve
// ----------------------
preserve_rule_except
	: preserve_rule_e preserve_rule_c preserve_rule_a
	  { $1 @ $2 @ $3 }
	;

preserve_rule_e
//	: ON args
//	  { List.map (fun arg -> Elt_event (Ev_name arg)) $1 }
//	| ON LPAREN args RPAREN
//	  { [Elt_event (Ev_name_seq $3)] }
//	| ON HAT NAME
//	  { [Elt_event (Ev_name_seq_compl [$3])] }
//	| ON HAT LPAREN args RPAREN
//	  { [Elt_event (Ev_name_seq_compl $4)] }
	: EXCEPT ON args
	  { [Elt_event (Ev_name_seq_compl $3)] }
	| EXCEPT ON LPAREN args RPAREN
	  { [Elt_event (Ev_name_seq_compl $4)] }
	;

preserve_rule_c
	:
	  { [] }
	| WHEN labelled_property
	  { [Elt_condition $2] }
	;

preserve_rule_a
	: preserve
	  { $1 }
//	| DO preserve
//	  { $2 }
//	| SLASH labelled_ldl_path SLASH
//	  { [Elt_path $2] }
	;

preserve
	: PRESERVE property
	  { [Elt_action [(Act_preserve [$2]), None]] }
	| PRESERVE preserve_args
	  { [Elt_action [(Act_preserve $2), None]] }
//	| PRESERVE preserve_args
//	  { [Elt_action [(Act_preserve $2), None]] }
// 	| PRESERVE LPAREN args RPAREN
//	  { [Elt_action [(Act_preserve $3), None]] }
	;

preserve_args
	: property COMMA property
	  { [$1; $3] }
	| preserve_args COMMA property
	  { $1 @ [$3] }
	;

//preserve_args
//	: NAME COMMA NAME
//	  { [Prop_atomic $1; Prop_atomic $3] }
//	| preserve_args COMMA NAME
//	  { $1 @ [Prop_atomic $3] }
//	;
 
%%
