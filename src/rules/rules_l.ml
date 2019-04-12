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

(** lexing *)

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

(* token : lexbuf -> Rules_p.token *)
let token_queue = Queue.create ()
let mode = ref 0
let pdepth = ref 0

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
    | 52 -> property buf
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
  | "on"		-> mode := 51; ON
  | "except"		-> EXCEPT
  | "when"		-> mode := 52; pdepth := 0; WHEN
  | "ensure"		-> mode := 53; pdepth := 0; ENSURE
  | "raise"		-> mode := 54; RAISE
  | "preserve"		-> mode := 55; PRESERVE
  | "do"		-> mode := 56; DO

  | "implementation"
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
  | "implementation"
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
  | '{'			-> enclosed_string buf
  | '}'			-> RBRACE
  | ';'			-> SEMI
  | _			-> common buf

and property (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | "//", any_string, (newline | eof)
			-> update_lnum buf; token buf
  | "/*"		-> comment buf; token buf

  (* terms *)
  | '='			-> EQUAL
  | "!="		-> NE
  | "~="		-> NE
  | "<="		-> LE
  | ">="		-> GE
  | '-'			-> MINUS
  | '/'			-> SLASH

  (* logical connectives *)
  | '!'			-> NOT
  | '~'			-> NOT
  | "&&"		-> AND
  | '&'			-> AND
  | "||"		-> OR
  | '|'			-> OR
  | "->"		-> IMPLIES
  | "=>"		-> IMPLIES

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

  | '('			-> incr pdepth; LPAREN
  | ')'			-> decr pdepth; RPAREN
  | '{'			-> if !pdepth = 0 && List.mem !mode [52; 53]
			   then enclosed_string buf
			   else LBRACE
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

and rule_ensure (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | ';'			-> SEMI
  | _			-> property buf

and rule_raise (buf : lexbuf) =
  let _buf : Sedlexing.lexbuf = buf._buf in
  match%sedlex _buf with
  | '+'			-> PLUS
  | '('			-> LPAREN
  | ')'			-> RPAREN
  | '{'			-> enclosed_string buf
  | '}'			-> RBRACE
  | ';'			-> SEMI
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
  | '{'			-> enclosed_string buf
  | '}'			-> RBRACE
  | ';'			-> SEMI
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
  let l : unit -> Rules_p.token * Lexing.position * Lexing.position = fun () ->
    let pos1 = buf._pos in
    let tk = token buf in
    let pos2 = buf._pos in
    tk, pos1, pos2 in
  let p' : (unit -> Rules_p.token * Lexing.position * Lexing.position) -> 'ast =
    MenhirLib.Convert.Simplified.traditional2revised p in
  try
    p' l
  with
  | Invalid_argument msg ->
      let p = buf._pos in
      eprintf ";; invalid arg: lnum=%d, bol=%d, cnum=%d\n" p.pos_lnum p.pos_bol p.pos_cnum;
      invalid_arg msg
  | ParseError msg ->
      let p = buf._pos in
      eprintf "** Error on parsing %s(<%s>)"
	(if p.pos_fname = "" then "" else "\"" ^ p.pos_fname ^ "\" ") msg;
      let lnum, bol, col, pos = p.pos_lnum, p.pos_bol, p.pos_cnum - p.pos_bol, p.pos_cnum in
      eprintf " at line:%d, column:%d, position:%d\n" lnum col pos;
      flush_all ();
      exit 1
  | Rules_p.Error ->
      let p = buf._pos in
      eprintf ";; Rules_p.Error: lnum=%d, bol=%d, cnum=%d\n" p.pos_lnum p.pos_bol p.pos_cnum;
      exit 1
