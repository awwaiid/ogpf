
(* pnode.ml -- define AST for mperl
 *
 * Copyright (c) 2001 Brock Wilcox
 * Released under the terms of the GNU GPL
 * See copying.txt for details
 *)

open Printf

exception Error of string

type pnode =
  | Binop     of string * pnode * pnode
              (* op   left_param right_param *)
  | Apply     of string * pnode
              (* func name, parameters *)
  | ScalarVar of string (* scalar name *)
  | ListVar   of string (* list name *)
  | Const     of string (* actual constant *)
  | ListAt    of string * block (* array name, locs *)
  | Loop      of string * block * block * block
              (* name     parms   iftrue  iffalse *)
  | Nothing

and block = pnode list

let max a b = if a > b then a else b

let rec max_depth n =
  List.fold_left (fun x y -> max x (depth y)) 10000 n

and depth = function
  | Binop(op,left,right) -> 1 + max (depth left) (depth right)
  | Apply(name,params) -> 1 + depth params
  | ScalarVar(name) -> 1
  | ListVar(name) -> 1
  | Const(str) -> 1
  | ListAt(name,loc) -> 1 + (max_depth loc)
  | Loop(name,params,iftrue,iffalse) ->
    1 + (max (max_depth params) (max (max_depth iftrue) (max_depth iffalse)))
  | Nothing -> 1

let rec block_num_nodes = function
  | [] -> 0
  | h::t -> (num_nodes h) + (block_num_nodes t)

and num_nodes = function
  | Binop(op,left,right) -> 1 + (num_nodes left) + (num_nodes right)
  | Apply(name,params) -> 1 + (num_nodes params)
  | ScalarVar(name) -> 1
  | ListVar(name) -> 1
  | Const(str) -> 1
  | ListAt(name,loc) -> 1 + (block_num_nodes loc)
  | Loop(name,params,iftrue,iffalse) ->
    1 + (block_num_nodes params)
      + (block_num_nodes iftrue)
      + (block_num_nodes iffalse)
  | Nothing -> 1

let rec pp_string_block pad = function
  | [] -> ""
  | h::t -> pad ^ (pp_string h pad) ^ ";\n" ^ (pp_string_block pad t)

and pp_string_binop op left right pad =
  if op = "," then
    sprintf "(%s %s %s)"
      (pp_string left pad) op (pp_string right pad)
  else 
    sprintf "%s %s %s"
      (pp_string left pad) op (pp_string right pad)

and pp_string_apply f params pad =
  sprintf "%s(%s)"
    f (pp_string params pad)

and pp_string_arrayloc pad = function
  | [] -> ""
  | h::t -> sprintf "[%s]%s" (pp_string h pad) (pp_string_arrayloc pad t)

and pp_string node pad =
  let npad = pad ^ "  " in
  match node with
  | Loop(ltype,condition,bodyA,bodyB) ->
    sprintf "%s(%s) {\n%s\n%s}"
      ltype (pp_string (List.hd condition) pad) 
      (pp_string_block npad bodyA) pad
  | Binop(op,left,right) -> pp_string_binop op left right pad
  | Apply(name,params) -> pp_string_apply name params pad
  | ScalarVar(name) -> sprintf "$%s" name (* "$VARIABLE" *)
  | ListVar(name) -> sprintf "@%s" name (* "$VARIABLE" *)
  | Const(s) -> sprintf "\"%s\"" s
  | ListAt(name, loc) -> sprintf "$%s%s" name
    (pp_string_arrayloc pad loc)
    (* (pp_string_arrayloc pad loc) *)
  | Nothing -> ""

let pp_string_program prog =
  sprintf "\n# Converted with mperl\n\n\n%s\n\n"
    (pp_string_block "" prog)

