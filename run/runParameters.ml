
(* Edit this file. Set up your run parameters here. *)

(* Here is the type definition for the run parameters. This is here as
   self-documenting code, so that you can see what exactly can be specified. *)
type ptype = {
  (* Population size *)
  max_population: int;
  min_population: int;
  generations: int;
}


(* Now comes your chance to tune the actual content of these parameters *)
let parameter = {
  max_population = 200;
  min_population = 100;
  generations = 200 * 500;
}

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
(*    ("if",3);
    (">",2);
    ("<",2);
    ("=",2); *)
  ]
