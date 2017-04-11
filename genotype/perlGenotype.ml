
open Parameters
open Pnode
open Printf

exception Error of string

let debug = sprintf

type t = block

(*  val to_string: t -> string *)
let to_string g = pp_string_block "" g

let rec get_subtree_list n = function
  | [] -> raise (Error "Hrm. Brokenness")
  | h::t ->
    let c = num_nodes h in
    if n <= c then get_subtree h n
    else get_subtree_list (n - c) t

and get_subtree g n =
  if n = 0 then g
  else if n < 0 then raise (Error "Invalid argument to get_subtree")
  else begin
    debug "get_subtree: g='%s'\nn='%s'\n" (to_string [g]) (string_of_int n);
    match g with
      | Binop(op,left,right) ->
        let left_size = num_nodes left in
        if n > left_size then get_subtree right (n - left_size - 1)
        else get_subtree left (n - 1)
      | Apply(name,params) -> get_subtree params (n - 1)
      | ListAt(name,loc) -> get_subtree_list (n - 1) loc
      | Loop(name,params,iftrue,iffalse) ->
        let params_size = block_num_nodes params in
        if n <= params_size then get_subtree_list (n - 1) params
        else
          let iftrue_size = block_num_nodes iftrue in
          if n <= (params_size + iftrue_size) then
            get_subtree_list (n - 1 - params_size) iftrue
          else get_subtree_list (n - 1 - params_size - iftrue_size) iffalse
      | _ -> g
  end

let rec set_subtree_list h n = function
  | [] -> [h] (* raise (Error "hmew.") *)
  | g::t ->
    let g_size = num_nodes g in
    if n >= g_size then (set_subtree g h n)::t
    else g::(set_subtree_list h (n - g_size) t)

and set_subtree g h = function
  | 0 -> h
  | _ as n ->
      debug "set_subtree: g='%s'\nh='%s'\nn='%s'\n\n" (to_string [g]) (to_string [h]) (string_of_int n);
      match g with
      | Binop(op,left,right) ->
        let left_size = num_nodes left in
        if n > left_size then
          Binop(op, left, set_subtree right h (n - left_size - 1))
        else Binop(op, set_subtree left h (n - 1), right)
      | Apply(name,params) -> Apply(name, set_subtree params h (n - 1))
      | ListAt(name,loc) -> ListAt(name, set_subtree_list h (n - 1) loc)
      | Loop(name,params,iftrue,iffalse) ->
        let params_size = block_num_nodes params in
        if n <= params_size then
          Loop(name, set_subtree_list h (n - 1) params, iftrue, iffalse)
        else
          let iftrue_size = block_num_nodes iftrue in
          if n <= (params_size + iftrue_size) then
            Loop(
              name, params, set_subtree_list h (n - 1 - params_size) iftrue, iffalse)
          else Loop(name, params, iffalse,
            set_subtree_list h (n - 1 - params_size - iftrue_size) iffalse)
      | _ -> h
      (* raise (Error "Error in set_subtree") *)

(*  val combine: t -> t -> t *)
let combine a b =
  debug "combine: a='%s'\nb='%s'\n" (to_string a) (to_string b);
  let nodes_a = block_num_nodes a in
  let chosen_node_a = Random.int nodes_a in
  let nodes_b = block_num_nodes b in
  let chosen_node_b = Random.int nodes_b in
  debug "a count=%d\nb count=%d\n" nodes_a nodes_b;
  let subtree_a = get_subtree_list chosen_node_a a in
  debug "Chosen subtree: '%s'\n" (to_string [subtree_a]);
  set_subtree_list subtree_a chosen_node_b b

let randOp() =
  let r = Random.int 5 in
  match r with
  | 0 -> "+"
  | 1 -> "-"
  | 2 -> "*"
  | 3 -> "/"
  | 4 -> "."
  | _ -> "%"

let randVar() =
  let r = Random.int 50 in
  "v" ^ (string_of_int r)
  
let randFunc() =
  let r = Random.int 50 in
  "f" ^ (string_of_int r)

let string_of_char c =
   String.make 1 c

let randConst() =
  let r = Random.int 2 in
  match r with
  | 0 -> (* number *)
    string_of_int(Random.int 100)
  | 1 -> (* string *)
    string_of_char (char_of_int (Random.int 255)) 
  | _ -> "bleh"

(*  val randInstance: int -> t *)
let rec randInstance_node n =
  if n = 0 then Const(randConst())
  else begin
    let n = n - 1 in
    let r = Random.int 8 in
    match r with
    | 0 -> Binop(randOp(), randInstance_node n, randInstance_node n)
    (*| 1 -> Apply(randFunc(), randInstance_node n) *)
    | 2 -> ScalarVar(randVar())
(*    | 3 -> ListVar(randVar()) *)
    | 4 -> Const(randConst())
(*    | 5 -> ListAt(randVar(), [randInstance_node n]) *)
    (* | 6 -> Loop(...) *)
(*    | 7 -> Lambda(randFunc(), randInstance 3) *)
    | _ -> Nothing
  end

and randInstance n =
  if n = 0 then []
  else (randInstance_node 3)::(randInstance (n-1))


(*  val print: t -> unit *)
let print g = print_string (to_string g)

(*  val of_string: string -> t *)
(* dummy implementation *)
let of_string s =
  let lexbuf = Lexing.from_string s in
  let result = Parser.block Lexer.token lexbuf in
  result

let perl_call prog =
  try
  let (inc, outc) = Unix.open_process "perl" in
  output_string outc prog;
  close_out outc;
  let result = ref "" in
  (try while true do
    let s = input_line inc in
(*    printf "Got: '%s'" s;
    print_newline(); *)
    result := !result ^ s ^ "\n"
  done with _ -> ());
  !result
  with _ -> "ERROR"

let eval x g =
  let prog = (sprintf "$v0 = %f; %s; print $v0\n" x (to_string g)) in
(*  print_string ("doing - '" ^ prog ^ "'\n");
  print_newline(); *)
  let result = perl_call prog in
  try
    let r = float_of_string result in
    (* printf "Result: %f\n" r; *)
    r
  with Failure(m) -> -100.0

