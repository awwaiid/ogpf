
module type Sig = sig
  type t (* = NonTerminal of string * t list | Terminal of string *)
  val print: t -> unit
  val to_string: t -> string
  val eval: float -> t -> float
end

