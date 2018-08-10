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
open Rule
open Printf

type rule_elt =
  | Elt_path of labelled_path
  | Elt_event of event
  | Elt_event_opt of string
  | Elt_condition of labelled_property
  | Elt_condition_opt of string
  | Elt_action of action
  | Elt_action_opt of string

let rec genrule (elts : rule_elt list) =
  let e : event = Ev_name ""
  and c : labelled_property = Prop_atomic "true", None
  and a : action = None, [] in
  let e', c', a', r' = genrule_rec ((e, None), (c, None), (a, None), None) elts
  in { event = e'; condition = c'; action = a'; path = r'; }

and genrule_rec ((e, ex), (c, cx), (a, ax), r) elts =
  if elts = [] then ((e, ex), (c, cx), (a, ax), r) else
  let rslt =
    match List.hd elts with
    | Elt_event e'          -> (e', ex), (c, cx), (a, ax), r
    | Elt_event_opt str     -> (e, Some str), (c, cx), (a, ax), r
    | Elt_condition c'      -> (e, ex), (c', cx), (a, ax), r
    | Elt_condition_opt str -> (e, ex), (c, Some str), (a, ax), r
    | Elt_action a'         -> (e, ex), (c, cx), (a', ax), r
    | Elt_action_opt str    -> (e, ex), (c, cx), (a, Some str), r
    (*| Elt_path r'           -> (e, ex), (c, cx), (a, ax), Some r'*)
    | _ -> failwith "genrule_rec"
  in genrule_rec rslt (List.tl elts)

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
%}

%start	rules decl_seq
%type<Rules.rules> rules
%type<Rules.decl list> decl_seq

%token	EVENT
%token	PROTOCOL
%token	PROPOSITION VARIABLE
%token	PROPERTY

%token	RULE
%token	ON IF WHEN DO THEN END
%token	RAISE ENSURE
%token	PRESERVE EXCEPT

// modified
%token	<int> CONST
%token	<string> TYPE XU
%token	<string> NAME UNAME PNAME INAME
%token	<string> STRING
%token	<string * string list> NAME_WITH_ARGS

%left	IMPLIES
%right	EQUAL
%left	EQ NE
%left	GT LT GE LE
%right	UMIN NEG NOT EXCLAM
%left	DOT
%left	STAR
%left	QUESTION
%left	OR
%left	AND

// added
%token	OR
%token	AND
%token	IMPLIES EQUAL

%token	EQ NE
%token	GT LT GE LE
%token	TILDE NEG NOT EXCLAM DOT DOLLAR HAT
%token	PLUS MINUS SLASH PERCENT
%token	QUESTION
%token	STAR

// extra tokens
%token	LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token	SEMI COLON COMMA AT
%token	SEMISEMI
%token	SKIP
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

	| PROPOSITION proposition_spec_seq
	  { List.map (fun s -> Rules.Decl_proposition s) $2 }
	| VARIABLE var_spec_seq
	  { List.map
	      (fun ((name, t), code) ->
		match t with
		| Rules.VT_bool -> Rules.Decl_proposition (name, code)
		| _ -> Rules.Decl_variable ((name, t), code))
	      $2 }

	| PROPERTY property_spec_seq
	  // properties enclosed by braces
	  { List.map (fun s -> Rules.Decl_property s) $2 }
//	| PROPERTY property_seq
//	  // naked properties
//	  { List.map (fun p -> Rules.Decl_property (None, p)) $2 }
//	| PATH path_spec_seq
//	  { List.map (fun s -> Rules.Decl_path s) $2 }

	| RULE rule_spec_seq
	  // rules enclosed by braces
	  { List.map (fun s -> Rules.Decl_rule s) $2 }

	| error
	  { raise @@ Rules_l.ParseError "decl" }
	;

// ================================================================================
// proposition
// ================================================================================
proposition_spec_seq
	: proposition_spec_seq1
	  { $1 }
	| proposition_spec_seq SEMI proposition_spec_seq1
	  { $1 @ $3 }
	| proposition_spec_seq SEMI
	  { $1 }
	;

proposition_spec_seq1
	: proposition_spec
	  { $1 }
	| proposition_spec_seq1 proposition_spec
	  { $1 @ $2 }
	| proposition_spec_seq1 COMMA proposition_spec
	  { $1 @ $3 }
	;

proposition_spec
	: NAME
	  { [$1, None] }
	| NAME range_seq
	  { List.map (fun x -> x, None) (expand_prop_spec $1 $2) }
	| NAME LBRACE STRING RBRACE
	  { [$1, Some $3] }
	;

range_seq
	: range
	  { [$1] }
	| range_seq range
	  { $1 @ [$2] }
	;

range	: LBRACK CONST RBRACK
	  { $2 }
	;

// ================================================================================
// variable
// ================================================================================
var_spec_seq
	: var_spec_seq1
	  { $1 }
	| var_spec_seq SEMI var_spec_seq1
	  { $1 @ $3 }
	| var_spec_seq SEMI
	  { $1 }
	;

var_spec_seq1
	: var_spec_seq2
	  { List.map (fun (name, code) -> (name, Rules.VT_bool), code) $1 }
	| var_spec_seq2 COLON var_type
	  { List.map (fun (name, code) -> (name, $3), code) $1 }
	;

var_type
	: NAME
	  { assert ($1 = "bool"); Rules.VT_bool }
	| NAME LPAREN CONST COMMA CONST RPAREN
	  { assert ($1 = "range");
	    Rules.VT_range ($3, $5) }
	;

var_spec_seq2
	: var_spec
	  { $1 }
	| var_spec_seq2 COMMA var_spec
	  { $1 @ $3 }
	;

var_spec
	: NAME
	  { [$1, None] }
	| NAME range_seq
	  { List.map (fun x -> x, None) (expand_prop_spec $1 $2) }
	| NAME LBRACE STRING RBRACE
	  { [$1, Some $3] }
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
	: NAME LPAREN param_seq RPAREN LBRACE labelled_property RBRACE
	  { let args =
	      List.map (function Tm_var (x, _) -> x | _ -> raise @@ Rules_l.ParseError "protocol_spec") $3
	    in Some ($1, args), $6 }
	| LBRACE property_or_pcall RBRACE
	  { None, $2 }
	| property_or_pcall SEMI
	  { None, $1 }
	;

property_or_pcall
	: labelled_property
	  { $1 }
	| NAME LPAREN param_seq RPAREN
	  { raise @@ Rules_l.ParseError "property_or_pcall" }
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
	  { $1 @ [Tm_val $3] }
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
	  { raise @@ Rules_l.ParseError "property" }
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
	| NAME index_seq
	  { Prop_atomic_elt ($1, $2) }
//	| modal_path property3
//	  { Ldl_modal (fst $1, snd $1, $2) }
	| neg property3
	  { Prop_neg $2 }
	| LPAREN property RPAREN
	  { $2 }
//	| label_use
//	  { Prop_label $1 }
	;

labelled_property3
	: property3
	  { $1, None }
	| label_def property3
	  { $2, Some $1 }
	;

// Prop_modal of modality * labelled_path * labelled_property
modal_property
	: modal_path labelled_property3
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

index_seq
	: index
	  { [$1] }
	| index_seq index
	  { $1 @ [$2] }
	;

index	: LBRACK term RBRACK
	  { $2 }
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
	  { raise @@ Rules_l.ParseError "path0" }
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

	// label
	| label_use
	  { Path_label $1 }
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

label_use
	: DOLLAR NAME
	  { $2 }
	| AT NAME
	  { $2 }
	;

// ================================================================================
// event
// ================================================================================

event_spec_seq
	: proposition_spec_seq
	  { $1 }
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
	: NAME LPAREN param_seq RPAREN LBRACE protocol RBRACE
	  { let args =
	      List.map (function Tm_var (x, _) -> x | _ -> raise @@ Rules_l.ParseError "protocol_spec") $3
	    in Some ($1, args), $6 }
	| LBRACE protocol_or_pcall RBRACE
	  { None, $2 }
	| protocol_or_pcall SEMISEMI
	  { None, $1 }
	;

protocol_or_pcall
	: protocol
	  { $1 }
	| NAME LPAREN param_seq RPAREN
	  { raise @@ Rules_l.ParseError "protocol_or_pcall" }
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
	  { raise @@ Rules_l.ParseError "protocol" }
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
	  { Proto_test $1 }
	| protocol3 QUESTION protocol3
	  { Proto_seq [Proto_test $1; $3] }
	| protocol3 STAR
	  { Proto_star $1 }
	;

protocol3
	: NAME
	  { Proto_prop (PProp_event $1) }
	| neg NAME
	  { Proto_prop (PProp_neg (PProp_event $2)) }
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
// rule
// ================================================================================
rule_spec_seq
	: rule_spec
	  { [$1] }
	| rule_spec_seq rule_spec
	  { $1 @ [$2] }
	;

rule_spec
	: NAME LPAREN param_seq RPAREN LBRACE rule RBRACE
	  { let args =
	      List.map (function Tm_var (x, _) -> x | _ -> raise @@ Rules_l.ParseError "rule_spec") $3
	    in Some ($1, args), genrule $6 }
	| LBRACE rule_or_rcall RBRACE
	  { None, genrule $2 }
	| rule_or_rcall SEMI
	  { None, genrule $1 }
	;

rule_or_rcall
	: rule
	  { $1 }
	| NAME LPAREN param_seq RPAREN
	  { raise @@ Rules_l.ParseError "rule_or_rcall" }
	;

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
rule	: ON rule_e WHEN rule_c do_action
	  { $2 @ $4 @ $5 }
//	| ON rule_e WHEN rule_c
//	  { $2 @ $4 }
	| ON rule_e do_action
	  { $2 @ $3 }

// special case
	| preserve_rule
	  { $1 }
	;

do_action
	: DO rule_a
	  { $2 }
	| action1
	  { [Elt_action (None, [$1])] }
	| action1 LBRACE STRING RBRACE
	  { [Elt_action (None, [$1]); Elt_action_opt $3] }
	;

// event/condition/action
rule_e	: args
	  { List.map (fun arg -> Elt_event (Ev_name arg)) $1 }
	| args LBRACE STRING RBRACE
	  { (List.map (fun arg -> Elt_event (Ev_name arg)) $1) @ [Elt_event_opt $3] }
	;

rule_c	: labelled_property
	  { [Elt_condition $1] }
	| labelled_property LBRACE STRING RBRACE
	  { [Elt_condition $1; Elt_condition_opt $3] }
	| LBRACE STRING RBRACE
	  { [Elt_condition_opt $2] }
	;

rule_a	: action
	  { [Elt_action $1] }
	| action LBRACE STRING RBRACE
	  { [Elt_action $1; Elt_action_opt $3] }
	| LBRACE STRING RBRACE
	  { [Elt_action_opt $2] }
	;

// special case
preserve_rule
	: ON rule_e preserve_rule_a
	  { $2 @ $3 }
	| preserve_rule_e preserve_rule_a
	  { $1 @ $2 }
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

preserve_rule_a
	: DO preserve
	  { $2 }
	| preserve
	  { $1 }

//	| SLASH labelled_ldl_path SLASH
//	  { [Elt_path $2] }
	;

preserve
	: PRESERVE args
	  { [Elt_action (None, [Act_preserve $2])] }
 	| PRESERVE LPAREN args RPAREN
	  { [Elt_action (None, [Act_preserve $3])] }
	;

args	: NAME
	  { [$1] }
	| args COMMA NAME
	  { $1 @ [$3] }
	;
 
// --------------------------------------------------------------------------------
// action ::= state | modal_path? action1_seq
// --------------------------------------------------------------------------------
action	: action1
	  { None, [$1] }
	| action1_seq
	  { None, $1 }
/*
	| modal_path state2
	  { assert (fst $1 = Mod_ex); Some (fst (snd $1)), [Act_ensure $2] }
	| modal_path action1
	  { assert (fst $1 = Mod_ex); Some (fst (snd $1)), [$2] }
	| modal_path LPAREN action1_seq RPAREN
	  { assert (fst $1 = Mod_ex); Some (fst (snd $1)), $3 }
*/
	;

action1_seq
	: action1 COMMA action1
	  { [$1; $3] }
	| action1_seq COMMA action1
	  { $1 @ [$3] }
	| LPAREN action1_seq RPAREN
	  { $2 }
	;

action0	: ENSURE state
	  { Act_ensure $2 }
	| RAISE event_sum
	  { Act_raise $2 }
	| LPAREN action0 RPAREN
	  { $2 }
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

action1	: action0
	  { $1 }
	| state
	  { Act_ensure $1 }
	;

// post-state proposition
state	: state0
	  { $1 }
	| state0 IMPLIES state
	  { Prop_disj [Prop_neg $1; $3] }
	| error
	  { raise @@ Rules_l.ParseError "state" }
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
	| neg state2
	  { Prop_neg $2 }
	| LPAREN state RPAREN
	  { $2 }
	| label_use
	  { Prop_label $1 }
	;

// --------------------------------------------------------------------------------
// term
// --------------------------------------------------------------------------------

term	: term1
	  { $1 }
	| term PLUS term1
	  { $1 }
	| term MINUS term1
	  { $1 }
	;

term1	: term2
	  { $1 }
	| term1 STAR term2
	  { $1 }
	;

term2	: CONST
	  { Tm_val $1 }
	| NAME
	  { Tm_var ($1, Ty_int) }
	| LPAREN term RPAREN
	  { $2 }
	;

%%
