
open RunParameters
open Pnode
open Printf

exception Error of string

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
    Printf.printf "get_subtree: g='%s'\nn='%s'\n" (to_string [g]) (string_of_int n);
    print_newline();
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
  | 1 -> h
  | _ as n ->
      Printf.printf "set_subtree: g='%s'\nh='%s'\nn='%s'\n\n" (to_string [g]) (to_string [h]) (string_of_int n);
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
      | _ -> raise (Error "Error in set_subtree")

(*  val combine: t -> t -> t *)
let combine a b =
  printf "combine: a='%s'\nb='%s'\n" (to_string a) (to_string b);
  let nodes_a = block_num_nodes a in
  let chosen_node_a = Random.int nodes_a in
  let nodes_b = block_num_nodes b in
  let chosen_node_b = Random.int nodes_b in
  printf "a count=%d\nb count=%d\n" nodes_a nodes_b;
  let subtree_a = get_subtree_list chosen_node_a a in
  printf "Chosen subtree: '%s'\n" (to_string [subtree_a]);
  set_subtree_list subtree_a chosen_node_b b

let randOp() =
  let r = Random.int 4 in
  match r with
  | 0 -> "+"
  | 1 -> "-"
  | 2 -> "*"
  | 3 -> "/"
  | _ -> "."

let randName() =
  let r = Random.int 50 in
  "x" ^ (string_of_int r)

let randConst() =
  let r = Random.int 2 in
  match r with
  | 0 -> (* number *)
    string_of_int(Random.int 100)
  | 1 -> (* string *)
    "hrm"
  | _ -> "bleh"

(*  val randInstance: int -> t *)
let rec randInstance_node n =
  if n = 0 then Const(randConst())
  else begin
    let n = n - 1 in
    let r = Random.int 8 in
    match r with
    | 0 -> Binop(randOp(), randInstance_node n, randInstance_node n)
    | 1 -> Apply(randName(), randInstance_node n)
    | 2 -> ScalarVar(randName())
    | 3 -> ListVar(randName())
    | 4 -> Const(randConst())
    | 5 -> ListAt(randName(), [randInstance_node n])
    (* | 6 -> Loop(...) *)
    | _ -> Nothing
  end

let rec randInstance n =
  if n = 0 then []
  else (randInstance_node 10)::(randInstance (n-1))


(*  val print: t -> unit *)
let print g = print_string (to_string g)

(*  val of_string: string -> t *)
(* dummy implementation *)
let of_string s =
  let lexbuf = Lexing.from_string s in
  let result = Parser.block Lexer.token lexbuf in
  result

let sys_call cmd =
  let inc = Unix.open_process_in cmd in
  let buf = Buffer.create 16 in
  (try while true do Buffer.add_channel buf inc 1 done with _ -> ());
  close_in inc;
  Buffer.contents buf

let eval x g = 1.0

