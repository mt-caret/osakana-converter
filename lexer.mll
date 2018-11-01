{
open Core_kernel
open Parser

exception SyntaxError of string

let next_line (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
;;
}

let whitespace = ' ' | '\t'
let newline = '\r' | '\n' | "\r\n"

let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | whitespace+ { read lexbuf }
  | newline     { next_line lexbuf; read lexbuf }
  | '#'         { read_comment lexbuf; read lexbuf }
  | ':'         { COLON }
  | ','         { COMMA }
  | 'w'         { WAIT }
  | 'g'         { GO }
  | id          { ID (Lexing.lexeme lexbuf) }
  | _           { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof         { EOF }
and read_comment =
  parse
  | newline { () }
  | _       { read_comment lexbuf }
