exception EmptySet
(** exception levÃ©e si l'on tente de deconstruire un ensemble vide *)

module type Ordoned =
sig
  type t
  val compare: t -> t -> int
end

module type Ensemble =
  sig
    type elt
    type set
    val is_empty : set -> bool
    val empty : set
    val mem : elt -> set -> bool
    val add : elt -> set -> set
    val fold : (elt -> 'a -> 'a) -> set -> 'a -> 'a
    val get_min : set -> elt
    val equal: set -> set -> bool
    val remove: elt -> set -> set
    val union: set -> set -> set
end

(** def d'un foncteur
module F (X : X_type) : Y_type
*)

module MakeList (X: Ordoned) =
  struct
    type elt = X.t
    type set = X.t list
    let empty = []

    let is_empty s = s = empty

    let rec mem a s = match s with
        [] -> false
       |t::q -> let c = X.compare t a in
                if c=0 then true
                else if c > 0 then false
                else mem a q;;

    let rec add e s = match s with
        []->[e]
       |t::q -> let c = X.compare t e in
                if c=0 then t::q
                else if c > 0 then e::s
                else t::(add e q);;


    let fold f s a = List.fold_left (fun acc  e -> f e acc) a s


    let get_min s= match s with
        [] -> raise EmptySet
       |t::q -> t

    let rec equal s1 s2 = match s1,s2 with
        [], [] -> true
       |t::q, a::b -> ((X.compare t a) = 0) && equal q b
       |_ -> false;;

    let rec remove e s = match s with
        []-> raise EmptySet
       |t::q -> let c = X.compare t e in
                if c = 0 then q
                else if c > 0  then s
                else t::(remove e q);;

    let rec union s1 s2 = match s1 with
      [] -> s2
      |t::q -> union q (add t s2);;

  end




module MakeAvl(X: Ordoned) =
struct

(** ensemble  avec les arbres*)

type elt = X.t
type set = Vide | Noeud of (elt*set*set*int)

let empty = Vide;;

let is_empty s = s=empty;;

let rec mem a s = match s with
    Vide -> false
   |Noeud(n,fg,fd,_) -> let c = X.compare n a in
            if c=0 then true
            else if c<0 then mem a fd
            else mem a fg;;


exception AddIn

let height a = match a with
  | Vide -> 0
  | Noeud(_,_,_,h)-> h

let node fg v fd =
  Noeud(v,fg,fd, 1+max (height fg) (height fd))

let balance g v d =
  let hg = height g in
  let hd = height d in
 if hg > hd +1
  then match g with
    | Vide -> assert false
    | Noeud(vg,gg,gd,hg)->
      let hgg = height gg in
      let hgd = height gd in
      if hgg > hgd then
        node  gg vg  (node gd v d)
      else (*hgg < hgd *)match gd with
          |Vide -> assert false
          |Noeud(vgd,gdg,gdd,_)->
          node (node gg vg gdg) vgd (node gdd v d)
  else if hd > hg +1 then match d with
    | Vide -> assert false
    | Noeud(vd,dg,dd,_)->
      let hdg = height dg in
      let hdd = height dd in
      if hdd > hdg then
        node  (node g v dg) vd  dd
      else (*hgg < hgd *)match dg with
          |Vide -> assert false
          |Noeud(vdd,dgg,dgd,_)->
          node (node g v dgg) vdd (node dgd vd dd)
  else (*if hg=hd then*)
      node g v d


let rec add x a = match a with
    | Vide -> node empty x empty
    | Noeud(v,fg,fd,_) -> let c = X.compare x v in
            if c=0 then (*x= v*)
                a
            else if c<0 then (* x < v*)
                balance (add x fg) v fd
            else (* x > v*)
                balance  fg v (add x fd)

let rec fold f s v0 = match s with
  |Vide -> v0
  |Noeud(n,fg,fd,_)-> let v1= f n (fold f fg v0) in
                    fold f fd v1;;


let rec get_min s = match s with
  |Vide -> raise EmptySet
  |Noeud(n,Vide,_,_) -> n
  |Noeud(n,fg,_,_) -> get_min fg;;

let rec equal_aux s1 s2 = match s1 with
  |Vide -> true
  |Noeud(n,fg,fd,_) -> mem n s2 && equal_aux fg s2 && equal_aux fd s2;;

let equal s1 s2 =
  equal_aux s1 s2 && equal_aux s2 s1;;

exception NotFound
let rec remove e s = match s with
  |Vide -> raise NotFound
  |Noeud(n,Vide,Vide,_)-> let c = X.compare n e in
                        if c=0 then Vide
                        else raise NotFound
  |Noeud(n,fg,fd,_) -> let c = X.compare n e in
                    if c > 0 then
                       let fg2= remove e fg in
                       balance fg2 n fd
                     else if c < 0 then
                       let fd2= remove e fd in
                      balance  fg n fd2
                     else (* n=e c=0*) let newn = get_min fd in
                                    let nfd = remove newn fd in
                                    balance fg newn nfd;;

let rec union s1 s2 = match s1 with
  |Vide -> s2
  |Noeud(n,fg,fd,_) -> union fd (union fg (add n s2));;

  end

  (*
  *****************************************************************************
  *)

  module MakeAbr (X: Ordoned) =
  struct

  (** ensemble  avec les arbres*)

  type elt = X.t
  type set = Vide | Noeud of (elt*set*set)

  let empty = Vide;;

  let is_empty s = s=empty;;

  let rec mem a s = match s with
      Vide -> false
     |Noeud(n,fg,fd) -> let c = X.compare n a in
              if c=0 then true
              else if c<0 then mem a fd
              else mem a fg;;


  exception AddIn

  let rec add_aux e s = match s with
      Vide->Noeud(e,Vide,Vide)
     |Noeud(n,fg,fd) -> let c = X.compare n e in
                        if c = 0 then
                          raise AddIn
                        else if c>0 then
                         Noeud(n, add_aux e fg, fd)
                        else (* n<e c<0 *)
                         Noeud(n, fg, add_aux e fd);;

  let add e s =
    try add_aux e s
    with AddIn -> s

  let rec fold f s v0 = match s with
    |Vide -> v0
    |Noeud(n,fg,fd)-> let v1= f n (fold f fg v0) in
                      fold f fd v1;;


  let rec get_min s = match s with
    |Vide -> raise EmptySet
    |Noeud(n,Vide,_) -> n
    |Noeud(n,fg,_) -> get_min fg;;

  let rec equal_aux s1 s2 = match s1 with
    |Vide -> true
    |Noeud(n,fg,fd) -> mem n s2 && equal_aux fg s2 && equal_aux fd s2;;

  let equal s1 s2 =
    equal_aux s1 s2 && equal_aux s2 s1;;

  exception NotFound
  let rec remove e s = match s with
    |Vide -> raise NotFound
    |Noeud(n,Vide,Vide)-> let c = X.compare n e in
                          if c=0 then Vide
                          else raise NotFound
    |Noeud(n,fg,fd) -> let c = X.compare n e in
                      if c > 0 then
                         let fg2= remove e fg in
                         Noeud(n,fg2,fd)
                       else if c < 0 then
                         let fd2= remove e fd in
                         Noeud(n,fg,fd2)
                       else (* n=e c=0*) let newn = get_min fd in
                                      let nfd = remove newn fd
                                      in Noeud(newn,fg,nfd);;

  let rec union s1 s2 = match s1 with
    |Vide -> s2
    |Noeud(n,fg,fd) -> union fd (union fg (add n s2));;

    end
