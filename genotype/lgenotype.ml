
module type Sig = sig
  type t
  val print: t -> unit
  val to_string: t -> string
  val eval: int -> t -> int
end

