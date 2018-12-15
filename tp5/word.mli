
module type OrderedType =
sig
  type t
  val compare: t -> t -> int
end

module type Word =
sig
  module Letter : OrderedType
  type word
  val empty: word
  val is_empty: word ->bool
  val add_end: word -> Letter.t -> word
  val pop : word ->Letter.t*word
end

module MakeWordString: Word with type word=String.t
