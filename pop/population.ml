
module type Sig = sig
  type m
  type 'a t
  val empty : m t
  val populate : int -> m t
  val size : m t -> int
  val pull_rand_member : m t -> m * m t
  val add_member : m t -> m -> m t
  val print : m t -> unit
  (*val empty : t
  val populate : int -> t
  val size : t -> int
  val pull_rand_member : t -> m * t
  val add_member : t -> m -> t
  val print : t -> unit *)
(*  val m_to_raw : m -> 'a
  val m_of_raw : 'a -> m *)
end


