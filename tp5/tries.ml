module type Trie =
sig
  type elt
  type letter
  module LetterMap : Map.S with type key = letter
  type t
  val empty: t
  val is_empty: t -> bool
  val cardinal : t -> int
  val nbletter: t -> int
  val maxlong: t ->int
  val add : elt -> t -> t
  val union : t -> t -> t
  val terminaison: t -> elt -> elt list
  val liste : t -> elt list
end

module MakeTrie(W:Word.Word) = struct
  type elt= W.word
  type letter= W.Letter
  module LetterMap=Map.Make


end
