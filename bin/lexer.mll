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
let valid_chars = [' '-'!' '#'-'&' '('-'[' ']'-'~']
let char = "'" valid_chars "'"
let string = "\"" valid_chars* "\""
let comment = "(*" ([^'*'] | '*' [^')'])* "*)"
let ticked = '\'' ['a'-'z'] ['a'-'z' '0'-'9' '_']*
let lower = ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
let upper = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule read = parse
  | eof { EOF }
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | comment { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "." { DOT }
  | "*" { TIMES }
  | "+" { PLUS }
  | "|" { PIPE }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "::" { CONS }
  | "|>" { TRIANGLE }
  | "->" { ARROW }
  | "<->" { BIARROW }
  | "=" { EQUAL }
  | "begin" { LPAREN }
  | "end" { RPAREN }
  | "unit" { UNIT }
  | "char" { CHARTYPE }
  | "let" { LET }
  | "iso" { ISO }
  | "in" { IN }
  | "fix" { FIX }
  | "type" { TYPE }
  | "rec" { REC }
  | "of" { OF }
  | "fun" { FUN }
  | "case" { CASE }
  | "match" { MATCH }
  | "with" { WITH }
  | nat { NAT (lexeme lexbuf |> int_of_string) }
  | char { CHAR (String.get (lexeme lexbuf) 1) }
  | ticked { TICKED (lexeme lexbuf) }
  | lower { LOWER (lexeme lexbuf) }
  | upper { UPPER (lexeme lexbuf) }
  | string
    { let buf = lexeme lexbuf in
      STRING (String.sub buf 1 (String.length buf - 2) |> Util.char_list_of_string) }

{
  let string_of_lb lexbuf =
    let pos = lexbuf.lex_curr_p in
    Format.sprintf "parse error at line %d, character %d"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

  let parse_res lexbuf =
    try Ok (program read lexbuf) with
    | Error -> Error (string_of_lb lexbuf)

  let parse str = from_string str |> parse_res
}
