

module Make ( Genotype: Genotype.Sig ) = struct

  type t = Genotype.t
  open Genotype
  exception Error of string

  let getFitness (g:t) =
    

end
