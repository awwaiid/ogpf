
module type Sig = sig
  type t
  val combine: t -> t -> t
  val randInstance: int -> t
  val print: t -> unit
  val to_string: t -> string
  val length: t -> int
  val at: t -> int -> char
  val of_string: string -> t
  val to_string: t -> string
end


