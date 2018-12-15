exception EmptySet
(** exception levÃ©e si l'on tente de deconstruire un ensemble vide *)
module type IntSet =
  sig
    type int_set
(** type ABSTRAIT (ne pas modifier cette ligne !) des ensemble d'entiers *)

val is_empty : int_set -> bool
(** [is_empty s] teste si  [s] est vide *)                     
  

val empty : int_set
(** L'ensemble vide. 

    Rq: 
     is_empty empty = true
 *)

val mem : int -> int_set -> bool
(**
   [mem e s] teste si [e] est un élément de [s]

   Rq: 
    1- \forall n s, is_empty s -> mem n s = false. 
    2- \forall n, mem n empty = false
 *)

  
val add : int -> int_set -> int_set
(** [add e s] retourne l'ensemble [s] augmenté de l'élément [e]
    
    Rq : 
     1- \forall n s, is_empty (add n s) = false
     2- \forall n s, mem n (add n s) = true 
     3- \forall n m s, mem m s = true -> mem m (add n s) = true
 *)


val fold : (int -> 'a -> 'a) -> int_set -> 'a -> 'a
(** [fold f s v0] calcule [(f xN ... (f x2 (f x1 v0)))] où [x1 ... xN] sont les éléments de [s] par ordre croissant

 *)

                              
(** À PARTIR DE CE POINT VOUS DEVEZ DONNER LES PROFILS DES FONCTIONS *)

val get_min : int_set -> int
(** [get_min s] retourne le plus petit élément de [s]

    Lève [EmptySet] si [s] est vide
 *)
                          
val equal: int_set -> int_set -> bool
(** [equal s t] test si les ensembles [s] et [t] sont égaux (i.e. contiennent EXACTEMENT les mêmes éléments)
 *)

val remove: int -> int_set -> int_set
(** [remove e s] retourne l'ensemble [s] dont on a retiré l'élément [e]

Rq: 
1- \forall n m s, mem m (remove n s) = true -> mem m s =true /\ m <> n
2- \forall n s, mem n s = false -> equal s (remove n s)

*)

val union: int_set -> int_set ->int_set
(** [union s t] retourne l'union des ensemble [s] et [t] 
    
    Rq: 
1- \forall n s t, mem n (union s t) = true <-> (mem n s=true \/ me n t = true)
2- \forall s, equal (union empty s) s /\ equal (union s empty) s
 *)
                              

  end 

module IntSetList =
  struct

(** ensemble d'entier avec les lites *)

type int_set = int list;;

let empty = [];;

let is_empty s = s=empty;;

let rec mem a s = match s with
    [] -> false
   |t::q -> if t=a then true
            else if t>a then false
            else mem a q;;

let rec add e s = match s with
    []->[e]
   |t::q -> if t = e then t::q
            else if t>e then e::s
            else t::(add e q);;


let fold f s a = List.fold_left (fun acc  e -> f e acc) a s
              
              
let get_min s= match s with
    [] -> raise EmptySet
   |t::q -> t

let rec equal s1 s2 = match s1,s2 with
    [], [] -> true
   |t::q, a::b -> a = t && equal q b
   |_ -> false;;

let rec remove e s = match s with
    []-> raise EmptySet
   |t::q -> if t = e then q
            else if t>e then s
            else t::(remove e q);;

let rec union s1 s2 = match s1 with
  [] -> s2
  |t::q -> union q (add t s2);;
  
  end

module IntSetAbr : IntSet =
  struct

(** ensemble d'entier avec les arbres*)
type int_set = Vide | Noeud of (int*int_set*int_set)

let empty = Vide;;

let is_empty s = s=empty;;

let rec mem a s = match s with
    Vide -> false
   |Noeud(n,fg,fd) -> if n=a then true
            else if n>a then mem a fd
            else mem a fg;;

  
exception AddIn
  
let rec add_aux e s = match s with
    Vide->Noeud(e,Vide,Vide)
   |Noeud(n,fg,fd) -> if n = e then
                        raise AddIn
                      else if n>e then
                       Noeud(n, add_aux e fg, fd)
                      else (* n<e *)
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
  |Noeud(n,Vide,Vide)-> if n=e then Vide
                        else raise NotFound 
  |Noeud(n,fg,fd) -> if n > e then
                       let fg2= remove e fg in
                       Noeud(n,fg2,fd)
                     else if n < e then
                       let fd2= remove e fd in
                       Noeud(n,fg,fd2)
                     else (* n=e *) let newn = get_min fd in
                                    let nfd = remove newn fd 
                                    in Noeud(newn,fg,nfd);;                        

let rec union s1 s2 = match s1 with
  |Vide -> s2  
  |Noeud(n,fg,fd) -> union fd (union fg (add n s2));;
 
  end
