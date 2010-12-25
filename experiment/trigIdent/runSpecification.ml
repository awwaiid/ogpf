
(* Edit this file. Set up your run components here. *)

(* We keep the parameters for the run in a separate file for dependencies *)
open RunParameters

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
(* module FitnessTest = PerlFactorial *)
module FitnessTest = TrigIdent

