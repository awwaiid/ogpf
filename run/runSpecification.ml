
(* Edit this file. Set up your run here. *)

(* Here is the type definition for the run parameters. This is here as
   self-documenting code, so that you can see what exactly can be specified. *)
type ptype = {

  (* Population size *)
  max_population: int;
  min_population: int;
}


(* Now comes your chance to tune the actual content of these parameters *)
let parameter = {
  max_population = 200;
  min_population = 100;
}

(* This is where you specify the model of evolution. The two main structures to
   worry about are the genotype and the selection mechanism. These are not very
   tightly coupled to each other, so you should have a pretty wide range of
   possible combinations. *)
(* module Genotype = StringGenotype *)
module Genotype = FuncTree
module SelectionMethod = ContinuousTournament

(* Last but not least you get to specify the problem itself. This will be
   closely tied to the genotype. *)
(*module FitnessTest = RosesProblem*)
module FitnessTest = TrigIdent

