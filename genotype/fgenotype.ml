
module type Sig = sig
  type t = NonTerminal of string * t list | Terminal of string
  val print: t -> unit
end

