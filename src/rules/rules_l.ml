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

let token_queue = Queue.create ()
let mode = ref 0
(* 0: normal
   1: in proposition/event. switch to mode=2 when encountering '{'
   2: in read_string. return to mode=1 when encountering '}'
   10: in rule.
   11: in on/when/do.
       - switch to mode=12 when encountering '{' when depth_angle/brack = 0
       - switch to mode=10 when encountering '}' when depth_angle/brack = 0
   12: in read_string. return to mode=11 when encountering '}'
 *)
let depth_angle = ref 0
let depth_brack = ref 0

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
let rec token (buf : lexbuf) =
  if Queue.is_empty token_queue
  then
    if !mode <> 2 && !mode <> 12
    then token_rec buf
    else read_string 1 [] buf
  else
    Queue.take token_queue

and token_rec (buf : lexbuf) =
  let _buf = buf._buf in
  update_cnum buf;
  (*eprintf "(%s)" @@ lexeme buf; flush_all ();*)
  match%sedlex _buf with
  | "/*"		-> comment 1 buf; token_rec buf
  | "//", any_string, (newline | eof)
			-> update_lnum buf; token_rec buf

  | '{'			->
      assert (List.mem !mode [0; 1; 10; 11]);
      (*eprintf "{"; flush_all ();*)
      if !depth_brack = 0 && !depth_angle = 0 then
	mode := (match !mode with 1 -> 2 | 11 -> 12 | _ -> !mode);
      LBRACE
  | '}'			->
      assert (List.mem !mode [0; 1; 10; 11]);
      (*eprintf "}"; flush_all ();*)
      if !depth_brack = 0 && !depth_angle = 0 then
	mode := (match !mode with 11 -> 10 | _ -> !mode);
      RBRACE

  (* event/protocol *)
  | "event"		-> mode := 1; EVENT
  | "protocol"		-> mode := 0; PROTOCOL

  (* proposition/property *)
  | "proposition"	-> mode := 1; PROPOSITION
  | "variable"
  | "var"		-> mode := 0; VARIABLE
  | "property"		-> mode := 0; PROPERTY

  (* logical connectives *)
  | "not"		-> NOT
  | '!'			-> NOT
  | "and"		-> AND
  | "&&"		-> AND
  | '&'			-> AND
  | "or"		-> OR
  | "||"		-> OR
  | '|'			-> OR
  | "->"		-> IMPLIES
  | "=>"		-> IMPLIES

  (* rule *)
  | "rule"		-> mode := 10; RULE
  | "on"		-> if !mode = 10 then mode := 11; ON
  | "when"		-> if !mode = 10 then mode := 11; WHEN
  | "if"		-> if !mode = 10 then mode := 11; WHEN
  | "do"		-> if !mode = 10 then mode := 11; DO
  | "then"		-> if !mode = 10 then mode := 11; DO
  | "ensure"		-> if !mode = 10 then mode := 11; ENSURE
  | "raise"		-> if !mode = 10 then mode := 11; RAISE
  | "preserve"		-> if !mode = 10 then mode := 11; PRESERVE
  | "except"		-> EXCEPT

  | ";;"		-> SEMISEMI
  | "=="		-> EQ
  | "!="		-> NE
  | "<="		-> LE
  | ">="		-> GE

  | '~'			-> TILDE
  | '<'			-> incr depth_angle; LT
  | '>'			-> decr depth_angle; GT
  | '('			-> LPAREN
  | ')'			-> RPAREN
  | '['			-> incr depth_brack; LBRACK
  | ']'			-> decr depth_brack; RBRACK
  | ':'			-> COLON
  | ';'			-> SEMI
  | ','			-> COMMA
  | '.'			-> DOT
  | '@'			-> AT
  | '='			-> EQUAL
  | '+'			-> PLUS
  | '-'			-> MINUS
  | '*'			-> STAR
  | '/'			-> SLASH
  | '%'			-> PERCENT
  | '?'			-> QUESTION
  | '^'			-> HAT
  | '$'			-> DOLLAR

  (* SYMBOL *)
  | identifier		->
      let s = lexeme buf in
      (*eprintf "<SYM(%s)>" s; flush_all ();*)
      NAME s

  (* LITERAL *)
  | string		->
      let s = lexeme buf in
      (*eprintf "<STR(%s)>" s; flush_all ();*)
      STRING (String.sub s 1 (String.length s - 2))
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
   *)

  | newline		-> update_lnum buf; token_rec buf
  | whitespace		-> token_rec buf
  | eof			-> (*eprintf "<eof>\n"; flush_all ();*) EOF

  | _			 -> failwith ("Unexpected character: " ^ lexeme buf)

and comment n (buf : lexbuf) =
  let _buf = buf._buf in
  match%sedlex _buf with
  | "/*"		-> comment (n + 1) buf
  | "*/"		-> if n > 1 then comment (n - 1) buf
  | newline		-> update_lnum buf; comment n buf
  | any			-> comment n buf
  | _			-> failwith ("Unexpected character: " ^ lexeme buf)

and read_string n chars (buf : lexbuf) =
  let _buf = buf._buf in
  match%sedlex _buf with
  | '{'	->
      (*eprintf "{"; flush_all ();*)
      read_string (n + 1) (chars @ ['{']) buf
  | '}'	->
      (*eprintf "}"; flush_all ();*)
      if n > 1
      then read_string (n - 1) (chars @ ['}']) buf
      else
	(assert (List.mem !mode [2; 12]);
	 mode := (match !mode with 2 -> 1 | 12 -> 11 | _ -> !mode);
	 Queue.push RBRACE token_queue;
	 STRING (String.init (List.length chars) (List.nth chars)))
  | any	->
      let str = lexeme buf in
      (*Printf.eprintf "<%s>" str; flush_all ();*)
      read_string n (chars @ [str.[0]]) buf

  | _	-> failwith ("Unexpected character: " ^ lexeme buf)

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
	
