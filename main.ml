
(* Main - entrypoint for a GP program based on the OGPF.
 *
 * History / Notes
 *   2004.01.13
 *     - Cleaning a bit
 *     - Moved components into subdirectories. Not noticable here
 *   2003.12.18
 *     - Compiles and runs the "Roses are red" problem using new framework! YAY
 *   2003.12.16
 *     - Moving to new framework structure
 *     - This file should be minimal, deligating work elsewhere
 *)

(* First we open the RunSpecification, importing everything. This will tell us
  what genotype and selection-method to use, and what problem we will be
  solving *)
open RunSpecification

(* Function to display the intro *)
let intro () = print_string (
    "OGPF - OCaml Genetic Programming Framework\n"
  ^ "\n"
  ^ "See gpl.txt for copyright information.\n\n")

(* module Genotype = CGenotype.Make (Genotype) (FitnessTest) *)

(* First we'll create a population module for the desired genotype *)
module Population = Pop.Make (Genotype)

(* Now we set up the fitness test *)
module FitnessTest = FitnessTest.Make (Genotype)

(* Set up the run module, which needs the population holder and fitness test *)
module Run = SelectionMethod.Make (Genotype) (Population) (FitnessTest)

let _ =
  (* Print out the intro *)
  intro();
  (* Initialize the random number generator *)
  Random.self_init();
  (* Initialize the run *)
  let r = Run.run() in
  (*print_int parameter.max_population; *)
  print_newline()

