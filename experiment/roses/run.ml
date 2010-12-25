

(* Override an parameters that we want *)
open Parameters
let _ =
  parameter.max_population <- 100;
  parameter.min_population <- 100;
  parameter.generations    <- 100 * 500000

module SelectionMethod = ContinuousTournament
module Genotype        = StringGenotype
module FitnessTest     = RosesProblem

