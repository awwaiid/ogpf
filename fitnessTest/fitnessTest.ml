
module type Sig = sig
  type t
  val getFitness: t -> int
end

