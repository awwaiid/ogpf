
(*
 * lexer.mll - Does the lexical analysis
 *
 * Copyright (c) 2001 Brock Wilcox
 * Released under the terms of the GNU GPL
 * See copying.txt for details
 *)

{
  open Parser
  let debug msg =
    print_string msg;
    print_newline()
  let debug msg = ()
}

rule token = parse
  (* Skip white space and comments *)
  | ([' ' '\t' '\n']+) | ('#' [^ '\n']*)
    { token lexbuf }


  (* End-of-files are needed *)
  | eof { EOF }
  | ';' { debug "semi" ; SEMI }


  (* Looping constructs *)
  | "while" { debug "while" ; WHILE }
  | '{' { debug "left curley" ; LEFT_CURLEY }
  | '}' { debug "right curley" ; RIGHT_CURLEY }
(*  | "do" { DO }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "foreach" { FOREACH }
  | "goto" { GOTO }
  | ':' { COLON } *)


  (* Binops *)
  | '=' { BINARY_OP("=") }
  | ',' { BINARY_OP(",") }
  | '+' { BINARY_OP("+") }
  | '-' { BINARY_OP("-") }
  | '*' { BINARY_OP("*") }
  | '/' { BINARY_OP("/") }
(*  | '%' { BINARY_OP("%") }
  | '>' { BINARY_OP(">") }
  | '<' { BINARY_OP("<") }
  | "&&" { BINARY_OP("&&") }
  | "||" { BINARY_OP("||") }
  | "!" { BINARY_OP("!") }
  | "!=" { BINARY_OP("!=") }
  | "ne" { BINARY_OP("ne") }
  | "eq" { BINARY_OP("eq") }
  | "==" { BINARY_OP("==") } *)
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }

  (* Constants *)
  | ['0'-'9']+ ('.' ['0'-'9']+)?
    { CONST(Lexing.lexeme lexbuf) }
  | '"' [^ '"']* '"'
    {
      let s = Lexing.lexeme lexbuf in
      let substr = String.sub s 1 ((String.length s) - 2) in
      CONST(substr)
    }
  | '\'' [^ '\'']* '\''
    {
      let s = Lexing.lexeme lexbuf in
      let substr = String.sub s 1 ((String.length s) - 2) in
      CONST(substr)
    }

  (* Indentifier indicators *)
  | '$' { SCALAR }
  | '@' { LIST }

  (* Identifiers *)
  | (['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*) | '?'
    { IDENT(Lexing.lexeme lexbuf) }
  
