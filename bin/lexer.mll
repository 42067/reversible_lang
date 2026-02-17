{
  open Lexing
  open Parser

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let nat = ['0'-'9']+
let comment = "(*" ([^'*'] | '*' [^')'])* "*)"
let ident_tvar = '\'' ['a'-'z'] ['a'-'z' '0'-'9' '_']*
let ident_var = ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
let ident_ctor = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule read = parse
  | eof { EOF }
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | comment { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "*" { TIMES }
  | "|" { PIPE }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "::" { CONS }
  | "->" { ARROW }
  | "<->" { BIARROW }
  | "=" { EQUAL }
  | "unit" { UNIT }
  | "let" { LET }
  | "in" { IN }
  | "iso" { ISO }
  | "fix" { FIX }
  | "type" { TYPE }
  | "inv" { INVERT }
  | "rec" { REC }
  | "of" { OF }
  | "fun" { FUN }
  | "case" { CASE }
  | "match" { MATCH }
  | "with" { WITH }
  | nat { NAT (lexeme lexbuf |> int_of_string) }
  | ident_tvar { TVAR (lexeme lexbuf) }
  | ident_var { VAR (lexeme lexbuf) }
  | ident_ctor { CTOR (lexeme lexbuf) }

{
  let string_of_lb lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "line %d, character %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

  let parse_res lexbuf =
    try Ok (program read lexbuf) with
    | Error -> Error (string_of_lb lexbuf)

  let parse str = from_string str |> parse_res
}
