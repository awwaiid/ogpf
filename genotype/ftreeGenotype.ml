
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
    "1"; "2"; "3"; "4";
    "x";
  ]

let nonterminal_set = [
    ("+",2);
    ("*",2);
    ("if",3);
  ]

let rec print = function
    Terminal(v) -> print_string v
  | NonTerminal(v,l) ->
    print_string "(";
    print_string v;
    
(* TODO: implement actuall crossover *)
let combine a b = a

let rec rand' depth =
  if depth == 0 then
    let max = List.length terminal_set in
    List.nth terminal_set (Random.int max)
  else
    let max = List.length nonterminal_set in
    let n = Random.int max in
    let (t, p) = list.nth n nonterminal_set in

let rec randInstance' n nt_set_size t_set_size =
  if n > 1 then
    let (nonterm, np) = List.nth (Random.int nt_set_size) nonterminal_set in
    let randTree n nt_set_size t_set_size = function
      | 0 -> []
      | _ as x -> List.nth (Random.int 
      
    let child_list = 



let randInstance' n nt_set_size t_set_size =

  let rand_terminal() =
    Terminal( List.nth (Random.int t_set_size))
  in
    
  let rec rNode = function
    | 0 -> rand_terminal()
    | _ as n ->
      let x = Random.int (nt_set_size + t_set_size) in
      if x >= nt_set_size then
        rand_terminal()
      else
        let (nonterm, num_params) = List.nth nt_set_size nonterminal_set in
        

let randInstance n =
  let nt_set_size = List.length nonterminal_set in
  let t_set_size = List.length terminal_set in



