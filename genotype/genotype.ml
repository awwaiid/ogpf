
module type Sig = sig
  type t
  val combine: t -> t -> t
  val randInstance: int -> t
  val print: t -> unit
  val to_string: t -> string
  val of_string: string -> t

  (* wonder what this was for? *)
  (* val at: t -> int -> char *)
end


