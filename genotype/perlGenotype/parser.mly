
/*
 * This is the yacc grammar for basic2c
 *
 * Copyright (c) 2001 Brock Wilcox
 * Released under the terms of the GNU GPL
 * See copying.txt for details
 */

%{
  let debug msg =
    print_string msg;
    print_newline()
  let debug msg = ()
%}


/* File structure */
%token EOF SEMI

/* Variable identifiers */
%token SCALAR LIST

/* Looping constructs */
%token WHILE
/*
%token IF ELSE
%token FOR FOREACH
%token GOTO LABEL COLON
%token SUB
*/

/* Commands (things like print) and identifiers */
%token <string> IDENT

/* Math */
%token <string> BINARY_OP
%token LEFT_PAREN RIGHT_PAREN

/* Other stuff */
%token LEFT_CURLEY RIGHT_CURLEY
%token LEFT_BRACKET RIGHT_BRACKET

/* Constants */
%token <string> CONST

/* Directive for where to start parsing */
%start block

%type <Pnode.block> block

%%

block:
  | SEMI block { $2 }
  | statement SEMI block
    { debug "statement-semi-block" ; $1::$3 }
  | statement SEMI
    { debug "statement" ; $1::[] }
  | loop block
    { debug "loop" ; $1::$2 }
  | EOF
    { [] };

code_block:
  | LEFT_CURLEY block RIGHT_CURLEY
    { debug "Code Block" ; $2 };

statement:
  | expression
    { debug "expression" ; $1 };

expression:
  | LEFT_PAREN expression RIGHT_PAREN
    { $2 }
  | function_call
    { debug "function call" ; $1 }
  | expression BINARY_OP expression
    { debug ("BinOp " ^ $2) ; Pnode.Binop($2,$1,$3) }
  | CONST
    { Pnode.Const($1) }
  | identifier
    { $1 };

loop:
  | while_loop
    { $1 };

while_loop:
  | WHILE LEFT_PAREN expression RIGHT_PAREN code_block
    { debug "while" ; Pnode.Loop("while",[$3],$5,[]) };

function_call:
  | IDENT LEFT_PAREN expression RIGHT_PAREN
    { Pnode.Apply($1,$3) }
  | IDENT LEFT_PAREN RIGHT_PAREN
    { Pnode.Apply($1,Pnode.Nothing) };

identifier:
  | SCALAR IDENT subscript
    { debug (Printf.sprintf "Pnode.ListAt(%s,)" $2 );
      Pnode.ListAt($2,$3) }
  | SCALAR IDENT
    { debug ("Scalar " ^ $2) ; Pnode.ScalarVar($2) }
  | LIST IDENT
    { debug ("List " ^ $2) ; Pnode.ListVar($2) };

subscript:
  | LEFT_BRACKET expression RIGHT_BRACKET subscript
    { $2::$4 }
  | LEFT_BRACKET expression RIGHT_BRACKET
    { $2::[] }


