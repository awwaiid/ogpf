
(* Edit this file. Set up your run parameters here. *)

let TrigIdent = module Make ( Genotype: Fgenotype.Sig ) = struct

  type t = Genotype.t
  open Genotype
  exception Error of string

  let getFitness (g:t) =
    (* print_string "Evaluating...\n";
    Genotype.print g;
    print_newline(); *)
    let total = ref 0.0 in
    let pi = 4.0 *. atan 1.0 in
    let n = 100 in
    for i = 0 to n do
      let x = Random.float (2.0 *. pi) in
      let target = cos (2.0 *. x) in 
      (*let target = (x *. 3.0) +. x in *)
      let result = Genotype.eval x g in
      let diff = abs_float (target -. result) in
      total := !total +. diff;
    done;
    let subtotal = int_of_float (!total *. 100.0) in
    subtotal + String.length (Genotype.to_string g)
    (* print_string "TOTAL: ";
    print_int !total;
    print_string "\t";
    print_int (truncate !total);
    print_newline();
    print_newline();

    truncate !total *)

end


(* Here is the type definition for the run parameters. This is here as
   self-documenting code, so that you can see what exactly can be specified. *)
type ptype = {
  max_population: int;
  min_population: int;
  generations: int;
}


(* Now comes your chance to tune the actual content of these parameters *)
let parameter = {
  max_population = 100;
  min_population = 150;
  generations = 100 * 500;
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

(* Edit this file. Set up your run components here. *)

(* We keep the parameters for the run in a separate file for dependencies *)
(* open RunParameters *)

(* This is where you specify the model of evolution. The two main structures to
   worry about are the genotype and the selection mechanism. These are not very
   tightly coupled to each other, so you should have a pretty wide range of
   possible combinations. *)

module SelectionMethod = ContinuousTournament

(* module Genotype = StringGenotype *)
module Genotype = LambdaGenome
(* module Genotype = PerlGenotype *)

(* Last but not least you get to specify the problem itself. This will be
   closely tied to the genotype. *)
module FitnessTest = TrigIdent



