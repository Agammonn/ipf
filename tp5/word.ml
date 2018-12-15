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


module MakeWordString = struct
  module Letter= Char
  type word= String.t
  let empty = ""
  let is_empty s= s=empty
  let add_end s c = s^ Letter.escaped c
  let pop s = (String.get s 0),(String.sub s 1 ((String.length s)-1))
end
