
open RunParameters
open Lambda
open Functions

exception Error of string

type t = exp

let rec get_subtree g n =
  if n = 0 then g
  else
    (* if (depth g) >= n then begin *)
    if 7 >= n then begin
      match g with
      | Lambda(name,ex) -> get_subtree ex (n - 1)
      | Apply(e1,e2) ->
        let n1 = depth e1 in
        if n <= n1 then get_subtree e1 (n - 1)
        else get_subtree e2 (n - n1)
      | _ -> raise (Error "Error in get_subtree")
    end else raise (Error "Invalid argument to get_subtree")

let rec set_subtree g h = function
  | 0 -> h
  | _ as n ->
    if depth g >= n then begin
      match g with
      | Lambda(name,ex) -> set_subtree ex h (n - 1)
      | Apply(e1,e2) ->
        let n1 = depth e1 in
        if n <= n1 then set_subtree e1 h (n - 1)
        else set_subtree e2 h (n - n1)
      | _ -> raise (Error "Error in set_subtree")
    end else raise (Error "Invalid argument to get_subtree")

(*  val combine: t -> t -> t *)
let combine a b =
  let nodes_a = num_nodes a in
  let chosen_node_a = Random.int nodes_a in
  let nodes_b = num_nodes b in
  let chosen_node_b = Random.int nodes_b in
  let subtree_a = get_subtree a chosen_node_a in
  set_subtree b subtree_a chosen_node_b



(*  val randInstance: int -> t *)
let randInstance n = rand n

(*  val print: t -> unit *)
(* done in lambda *)
let print = Lambda.print

(*  val to_string: t -> string *)

let rec to_string = function
  | Num(num) ->
    string_of_int num
  | Var(name,num) ->
    if (name = "") then begin
      "#" ^ (string_of_int num)
    end else
      name
  | Lambda(name,ex) ->
    "\\" ^
    (if not (name = "") then begin
      name ^ "."
    end else "" )
    ^ (to_string ex)
  | Apply(e1,e2) ->
    "(" ^ (to_string e1) ^ " " ^ (to_string e2) ^ ")"
  | Prim(name,f) ->
    "!" ^ name

(*  val of_string: string -> t *)
(* dummy implementation *)
let of_string s = Num(0)

let eval n g =
  install ();
  add_var "x" (Lambda.Num n);
    let gexp = bruijnize (expand g) in
      try
        match Lambda.timed_reduce gexp 1 with
        | Num(n) -> n
        | _ -> -1
      with Lambda.Timeout ->
        print_string "TIMEOUT\n";
        0
let _ =
  install ()
