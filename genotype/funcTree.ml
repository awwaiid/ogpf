
(* ftreeGenotype
 *
 * This genotype is designed to be compatable with Koza-style lisp genomes.
 * There are several ways to implement this sort of thing in OCaml, so this may
 * be one implementation among many.
 * 
 * History / Notes
 *   2003.12.19
 *     - Created
 *)


(* The core type, this must be named "t" *)
type t =
    NonTerminal of string * (t list)
  | Terminal of string

let terminal_set = [
    "1";
    "x";
  ]

let nonterminal_set = [
    ("+",2);
    ("-",2);
    ("*",2);
    ("%",2);
    ("sin",1);
  ]

(* shortcut -- get the lengths of these things *)
let nt_set_size = List.length nonterminal_set
let t_set_size = List.length terminal_set

let rec to_string = function
    Terminal(v) -> v
  | NonTerminal(v,l) ->
    (* flatten the parameter list, add spaces between each *)
    let p = List.fold_left (fun x y -> x ^ " " ^ (to_string y)) "" l in
    "(" ^ v ^ p ^ ")"

let print g = print_string (to_string g)

(* TODO: replace this with a real one! *)
let of_string (s:string) = Terminal(List.nth terminal_set 0)

let rec depth = function
  | Terminal(v) -> 1
  | NonTerminal(v, l) ->
    let d = List.fold_left (
      fun x y ->
        let d = depth y in
        if (x > d) then x else d
      ) 0 l in
    d

let rec num_nodes = function
  | Terminal(v) -> 1
  | NonTerminal(v, l) ->
    let children = List.fold_left (
      fun x y ->
        let n = num_nodes y in
        x + n
    ) 0 l in
    children

let rec replace_nth' w n = function
  | [] -> (n, w)
  | h::t ->
    if n = 0 then (0, w) else
    match h with
    | Terminal(v) -> (n - 1, Terminal "")
    | NonTerminal(v, l) ->
      let (n, tree) = replace_nth' w (n - 1) l in
      replace_nth' w (n - 1) t

let replace_nth w tree n =
  let (n, tree) = replace_nth' w n [tree] in
  tree

let rec nth' n = function
  | [] -> (n, Terminal "")
  | h::t ->
    if n = 0 then (0, h) else
    match h with
    | Terminal(v) -> (n - 1, Terminal "")
    | NonTerminal(v, l) ->
      let (n, tree) = nth' (n - 1) l in
      if n = 0 then (0, tree)
      else nth' (n - 1) t

let nth tree n =
  let (n, tree) = nth' n [tree] in
  tree

(* grab a random subset of b and stick it somewhere in a *)
let combine a b =
  let a_size = num_nodes a in
  let b_size = num_nodes b in
  let a_rand = Random.int a_size in
  let b_rand = Random.int b_size in
  let b_tree = nth b b_rand in
  let a_tree = replace_nth b a a_rand in
  a_tree

let rec make_list f = function
  | 0 -> []
  | n -> f()::(make_list f (n - 1))

(* This takes one variable, the depth *)
and randInstance = function

  (* no depth, so just give back a terminal *)
  | 0 -> Terminal(List.nth terminal_set (Random.int t_set_size))

  (* Otherwise lets build some stuff *)
  | _ as n ->
    let x = Random.int (nt_set_size + t_set_size) in
    if x >= nt_set_size then
      Terminal(List.nth terminal_set (x - nt_set_size))
    else
      let (nonterm, num_params) = List.nth nonterminal_set x in
      let l = make_list (fun () -> randInstance (n - 1)) num_params in
      NonTerminal(nonterm, l)

