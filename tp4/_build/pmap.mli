exception EmptySet

module type Ordoned =
sig
  type t
  val compare: t -> t -> int
end

module type S =
sig
  type key
  type 'a pmap
  val empty : 'a pmap
  val is_empty : 'a pmap -> bool
  val add : key -> 'a -> 'a pmap -> 'a pmap
  val find : key -> 'a pmap -> 'a
  val mem : key -> 'a pmap -> bool
  val remove : key -> 'a pmap -> 'a pmap
  val fold : (key -> 'b -> 'b) -> 'a pmap -> 'b -> 'b
end

module AVLMap (X: Ordoned) : S with type key=X.t
