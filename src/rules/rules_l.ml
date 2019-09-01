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

open Rules_p
open Printf

type lexbuf =
   {
     _buf: Sedlexing.lexbuf;
     mutable _pos: Lexing.position;
   }

exception Error of lexbuf * string
exception ParseError of string

(** common patterns *)

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]

(*let special_initial = [ '$' '*' '_' ]*)
let special_initial = [%sedlex.regexp? '_']
let initial = [%sedlex.regexp? letter | special_initial]
(*let initial = letter*)
let special_subsequent = [%sedlex.regexp? '_' | '.']
let subseqent = [%sedlex.regexp? letter | digit | special_subsequent]
(*let subseqent = letter | digit*)

let identifier = [%sedlex.regexp? initial, Star subseqent]

(*let sign = [%sedlex.regexp? "" | '+' | '-'*)
let sign = [%sedlex.regexp? "" | '+' | '-']
(*let integer = sign digit+*)
let integer = [%sedlex.regexp? Plus digit]
let suffix = [%sedlex.regexp? "" | ('e', sign, Plus digit)]
let real = [%sedlex.regexp? (Plus digit), '.', (Plus digit), suffix | '.', (Plus digit), suffix]
let number = [%sedlex.regexp? integer | real]
let boolean = [%sedlex.regexp? "false" | "true"]

let string_element = [%sedlex.regexp? "''" | Compl ('\"' | '\\') | "\\\"" | '\\']
let string = [%sedlex.regexp? '\"', Star string_element, '\"']

let newline = [%sedlex.regexp? '\n' | '\r' | "\n\r"]
let whitespace = [%sedlex.regexp? ' ' | '\t' | newline]
(*let atmosphere = whitespace | comment*)
let atmosphere = [%sedlex.regexp? whitespace]
let intertoken_space = [%sedlex.regexp? Plus atmosphere]

let any_string = [%sedlex.regexp? Star (Compl ('\n' | '\r'))]

(** lexbuf operations *)

let create_lexbuf ?(file = "") (buf : Sedlexing.lexbuf) =
  let p : Lexing.position =
    { pos_fname = file;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 1;
    }
  in {_buf = buf; _pos = p}

let update_cnum (buf : lexbuf) =
  let p :int = Sedlexing.lexeme_start buf._buf in
  buf._pos <- {buf._pos with Lexing.pos_cnum = p}

let update_lnum (buf : lexbuf) =
  (*Sedlexing.new_line buf._buf;*)
  let lnum = buf._pos.pos_lnum + 1
  and bol = Sedlexing.lexeme_start buf._buf
  in buf._pos <- {buf._pos with Lexing.pos_lnum = lnum; Lexing.pos_bol = bol}

let lexeme {_buf} = Sedlexing.Utf8.lexeme _buf

let mode = ref 0
    (* token invokes different operations depending on the mode value
       00: top level
       10: event
       20: protocol
       30: variable
       40: property
       51: event part of a rule
       52: when part
       53/54/55: ensure/raise/preserve part
       56: do part
       90: script
     *)

(** enclosed_string *)

(* token : lexbuf -> Rules_p.token *)
let token_queue = Queue.create ()
    (* enqueue in enclosed_string (token = RBRACE or STRING)
       dequeue in token
     *)

(* property info *)
type pinfo_t =
    { tokens: (Rules_p.token * string) Queue.t;
      mutable depth_paren: int;
      mutable depth_box: int;
      mutable depth_dia: int;
    }
let pinfo : pinfo_t =
  { tokens = Queue.create ();
    depth_paren = 0;
    depth_box = 0;
    depth_dia = 0;
  }

let pinfo_reset () =
  Queue.clear pinfo.tokens;
  pinfo.depth_paren <- 0;
  pinfo.depth_box <- 0;
  pinfo.depth_dia <- 0;
  ()

let pdepth = ref 0
    (* '(' and ')' incr/decr pdeth, respectively *)

(* checks if '{' in the current lexing position starts an enclosed string '{<STRING>}' or not.
  -- tricky and incomplete. needs to be updated.
 *)
let enclosed_string_p (buf : lexbuf) =
  if lexeme buf <> "{" then false else
  let _ = () in

  (* case: not in the 'when' or 'ensure' block *)
  if not (List.mem !mode [52; 53]) then true else

  (* case: in '(..)' or '[..]' *)
  if pinfo.depth_paren <> 0 || pinfo.depth_box <> 0 then false else
  let _ = () in

  (* case: no '<' precedes '{' *)
  let lt_precedes =
    try Queue.iter (function LT, _ -> raise Exit | _ -> ()) pinfo.tokens; false with Exit -> true
  in
  if not lt_precedes then true else

  (* otherwise -- incorrect
     'pinfo.depth_dia <> 0' does not always indicate that '{' appears outside the formula part.
     for instance, consider 'ensure x < y { ... }', for which 'pinfo.depth_dia = 1' holds.
   *)
  if pinfo.depth_dia <> 0 then false else true

(** lexing *)

let rec token (buf : lexbuf) =
  if not @@ Queue.is_empty token_queue
  then
    Queue.take token_queue
  else
    let _ = () in
    update_cnum buf;
    (*eprintf "(%s)" @@ lexeme buf; flush_all ();*)
    match !mode with
    | 0 -> toplevel buf
    | 10 -> event buf
    | 20 -> protocol buf
    | 30 -> variable buf
    | 40 -> property buf

    | 51 -> rule_event buf
    | 52 -> rule_when buf
    | 53 -> rule_ensure buf
    | 54 -> rule_raise buf
    | 55 -> rule_preserve buf
    | 56 -> rule_do buf

    | 90 -> rule_do buf

    | _ -> failwith ("[token] unrecognized mode: " ^ string_of_int !mode)

and toplevel (buf : lexbuf) =
  assert (!mode = 0);
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | "event"		-> mode := 10; EVENT
  | "protocol"		-> mode := 20; PROTOCOL
  | "variable"		-> mode := 30; VARIABLE
  | "property"		-> mode := 40; PROPERTY

  | "rule"		-> RULE
  | "except"		-> EXCEPT
  | "on"		-> mode := 51; ON
  | "when"		-> mode := 52; pinfo_reset (); WHEN
  | "ensure"		-> mode := 53; pinfo_reset (); ENSURE
  | "raise"		-> mode := 54; RAISE
  | "preserve"		-> mode := 55; PRESERVE
  | "do"		-> mode := 56; DO

  | "script"		-> mode := 90; SCRIPT

  | newline		-> update_lnum buf; toplevel buf
  | whitespace		-> toplevel buf
  | eof			-> (*eprintf "<eof>\n"; flush_all ();*) EOF
  | "//", any_string, (newline | eof)
			-> update_lnum buf; toplevel buf
  | "/*"		-> comment buf; toplevel buf
  | _			-> failwith ("[toplevel] unexpected character: " ^ lexeme buf)

and common (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | "//", any_string, (newline | eof)
			-> update_lnum buf; token buf
  | "/*"		-> comment buf; token buf

  | "event"
  | "protocol"
  | "variable"
  | "property"		-> mode := 0; Sedlexing.rollback _buf; token buf

  | "rule"
  | "on"
  | "except"
  | "when"
  | "ensure"
  | "raise"
  | "preserve"
  | "do"		-> mode := 0; Sedlexing.rollback _buf; token buf

  | "script"		-> mode := 0; Sedlexing.rollback _buf; token buf

  (* NAME *)
  | identifier		-> let sym = lexeme buf in
			   (*eprintf "(%s)" sym; flush_all ();*)
			   NAME sym
  (* LITERAL *)
  | integer		->
      let str = lexeme buf in
      let n =
	try int_of_string str
	with Failure "int_of_string" ->
	  Printf.eprintf
	    "invalid term: %S (out of [%d,%d]) --> 0\n"
	    str min_int max_int;
	  0
      in
      (*Logger.printf "<INT(%d)>" n;*)
      CONST n
  (*
  | real		->
      let n = float_of_string (lexeme buf) in
      (*Printf.eprintf "<REAL(%.2f)>" n;*)
      REALNUMBER n
  | string		->
      let s = lexeme buf in
      (*eprintf "<STR(%s)>" s; flush_all ();*)
      STRING (String.sub s 1 (String.length s - 2))
   *)

  | newline		-> update_lnum buf; token buf
  | whitespace		-> token buf
  | eof			-> (*eprintf "<eof>\n"; flush_all ();*) EOF
  | _			-> failwith "[common] nothing to scan"

and event (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | ','			-> COMMA
  | ';'			-> SEMI
  | _			-> common buf

and protocol (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | '('			-> LPAREN
  | ')'			-> RPAREN
  | '+'			-> PLUS
  | '*'			-> STAR
  | '?'			-> QUESTION
  | ";;"		-> SEMISEMI
  | ';'			-> SEMI

  | '{'			-> enclosed_string buf
  | '}'			-> RBRACE

  | _			-> common buf

and variable (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | ','			-> COMMA
  | ':'			-> COLON
  | '('			-> LPAREN
  | ')'			-> RPAREN
  | ';'			-> SEMI

  | '{'			-> enclosed_string buf
  | '}'			-> RBRACE

  | _			-> common buf

and property (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | "//", any_string, (newline | eof)
			-> update_lnum buf; token buf
  | "/*"		-> comment buf; token buf

  (* terms *)
  | '='			-> EQUAL
  | "!=" | "~="		-> NE
  | "<="		-> LE
  | ">="		-> GE
  | '-'			-> MINUS
  | '/'			-> SLASH

  (* logical connectives *)
  | '!' | '~'		-> NOT
  | "&&" | '&'		-> AND
  | "||" | '|'		-> OR
  | "->" | "=>"		-> IMPLIES

  (* modal *)
  | 'X' | "()"		-> LTL_UOP 'X'
  | 'F' | "<>"		-> LTL_UOP 'F'
  | 'G' | "[]"		-> LTL_UOP 'G'
  | 'U'			-> LTL_BOP 'U'

  | '['			-> LBRACK
  | ']'			-> RBRACK
  | '?'			-> QUESTION

  (* common *)
  | '+'			-> PLUS
  | '*'			-> STAR
  | ';'			-> SEMI
  | '<'			-> LT
  | '>'			-> GT

  | '('			-> LPAREN
  | ')'			-> RPAREN

  | '{'			-> LBRACE
  | '}'			-> RBRACE

  | _			-> common buf

and rule_event (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | ','			-> COMMA
  | '('			-> LPAREN
  | ')'			-> RPAREN

  | '{'			-> enclosed_string buf
  | '}'			-> RBRACE

  | _			-> common buf

and rule_when (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | ';'			-> SEMI

  | "()"		-> LTL_UOP 'X'
  | "<>"		-> LTL_UOP 'F'

  | '('			-> pinfo.depth_paren <- pinfo.depth_paren + 1; LPAREN
  | ')'			-> pinfo.depth_paren <- pinfo.depth_paren - 1; RPAREN
  | '[' | ']'		-> raise Rules_p.Error
  | '<'			-> pinfo.depth_dia <- pinfo.depth_dia + 1; LT
  | '>'			-> pinfo.depth_dia <- pinfo.depth_dia - 1; GT

  | '{'			-> if enclosed_string_p buf
			   (* {<STRING>} *)
			   then enclosed_string buf
			   else LBRACE
  | '}'			-> RBRACE

  | _			-> property buf

and rule_ensure (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | ';'			-> SEMI

  | "()"		-> LTL_UOP 'X'
  | "<>"		-> LTL_UOP 'F'
  | "[]"		-> LTL_UOP 'G'

  | '('			-> pinfo.depth_paren <- pinfo.depth_paren + 1; LPAREN
  | ')'			-> pinfo.depth_paren <- pinfo.depth_paren - 1; RPAREN
  | '['			-> pinfo.depth_box <- pinfo.depth_box + 1; LBRACK
  | ']'			-> pinfo.depth_box <- pinfo.depth_box - 1; RBRACK
  | '<'			-> pinfo.depth_dia <- pinfo.depth_dia + 1; LT
  | '>'			-> pinfo.depth_dia <- pinfo.depth_dia - 1; GT

  | '{'			-> if enclosed_string_p buf
			   (* {<STRING>} *)
			   then enclosed_string buf
			   else LBRACE
  | '}'			-> RBRACE

  | _			-> property buf

and rule_raise (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | '+'			-> PLUS
  | '('			-> LPAREN
  | ')'			-> RPAREN
  | ';'			-> SEMI

  | '{'			-> enclosed_string buf
  | '}'			-> RBRACE

  | _			-> common buf

and rule_preserve (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | ','			-> COMMA
  | ';'			-> SEMI
  | _			-> property buf

and rule_do (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | ';'			-> SEMI

  | '{'			-> enclosed_string buf
  | '}'			-> RBRACE

  | _			-> common buf

and enclosed_string (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  Sedlexing.rollback _buf;
  match%sedlex _buf with
  | '{'			-> Queue.push LBRACE token_queue;
			   enclosed_string_rec 1 [] buf
  | _			-> failwith "[read_string_rec]"

and enclosed_string_rec n chars (buf : lexbuf) =
  let _buf = buf._buf in
  match%sedlex _buf with
  | '{'	->
      (*eprintf "{"; flush_all ();*)
      enclosed_string_rec (n + 1) (chars @ ['{']) buf
  | '}'	->
      (*eprintf "}"; flush_all ();*)
      if n > 1
      then
	enclosed_string_rec (n - 1) (chars @ ['}']) buf
      else (* n = 1 -- end of string *)
	let str = String.init (List.length chars) (List.nth chars) in
	(*eprintf "(%S)" str;*)
	Queue.push (STRING str) token_queue;
	Queue.push RBRACE token_queue;
	token buf
  | any	->
      let str = lexeme buf in
      (*eprintf "<%s>" str; flush_all ();*)
      enclosed_string_rec n (chars @ [str.[0]]) buf
  | _	-> failwith "[enclosed_string_rec]"

and comment (buf : lexbuf) =
  comment_rec 1 buf

and comment_rec n (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | "/*"		-> comment_rec (n + 1) buf
  | "*/"		-> if n > 1 then comment_rec (n - 1) buf
  | newline		-> update_lnum buf; comment_rec n buf
  | any			-> comment_rec n buf
  | _			-> failwith ("[comment_rec] unexpected character: " ^ lexeme buf)

(** parsing *)

let parse (p : (Rules_p.token, 'ast) MenhirLib.Convert.traditional) (buf : lexbuf) =
  (* lexer *)
  (* note
     - each time 'sedlex%match _buf' is called
       first, the lexer potition is marked (Sedlexing.mark) for later backtracking,
       and then it is advanced by 1 (Sedlexing.next)
     - if there exists a match (except '_'), the advanced positions are fixed.
     - if it matches with '_', the marked positions are restored (Sedlexing.backgrack).
   *)
  let l : unit -> Rules_p.token * Lexing.position * Lexing.position = fun () ->
    let tok = token buf and pos1, pos2 = Sedlexing.lexing_positions buf._buf in
    (*eprintf "(pos1=%d,pos2=%d,%S)" pos1.pos_cnum pos2.pos_cnum (lexeme buf); flush stderr;*)
    Queue.push (tok, lexeme buf) pinfo.tokens;
    tok, pos1, pos2
  (* parser *)
  and p' : (unit -> Rules_p.token * Lexing.position * Lexing.position) -> 'ast =
    MenhirLib.Convert.Simplified.traditional2revised p
  in
  try
    p' l
  with
  | Invalid_argument msg ->
      let p, _ = Sedlexing.lexing_positions buf._buf in
      eprintf "[Rules_l.parse] invalid arg: lnum=%d, bol=%d, cnum=%d\n" p.pos_lnum p.pos_bol p.pos_cnum;
      invalid_arg msg
  | ParseError msg ->
      let p, _ = Sedlexing.lexing_positions buf._buf in
      eprintf "[Rules_l.parse] ** ParseError: %s(<%s>)"
	(if p.pos_fname = "" then "" else "\"" ^ p.pos_fname ^ "\" ") msg;
      let lnum, bol, col, pos = p.pos_lnum, p.pos_bol, p.pos_cnum - p.pos_bol, p.pos_cnum in
      eprintf " at line:%d, column:%d, position:%d\n" lnum col pos;
      flush_all ();
      exit 1
  | Rules_p.Error ->
      let p, _ = Sedlexing.lexing_positions buf._buf in
      eprintf "[Rules_l.parse] Error: lnum=%d, bol=%d, cnum=%d\n" p.pos_lnum p.pos_bol p.pos_cnum;
      exit 1
