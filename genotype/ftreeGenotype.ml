
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
let of_string s = List.nth terminal_set 0

(* TODO: implement actuall crossover *)
let combine a b = a

let rec make_list f = function
  | 0 -> [f()]
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

