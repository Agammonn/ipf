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
  val remove : key -> 'a pmap -> 'a pmap
  val fold : (key -> 'b -> 'b) -> 'a pmap -> 'b -> 'b
end

module AVLMap (X: Ordoned) : S with type key=X.t =
struct
type key = X.t
type 'a pmap =
  | Vide
  | Noeud of ( X.t * 'a * 'a pmap * 'a pmap * int)

  let empty = Vide;;

  let is_empty s = s=empty;;

  let rec find a s=  match s with
        Vide -> failwith("not in")
       |Noeud(n,t,fg,fd,_) -> let c = X.compare n a in
                if c=0 then t
                else if c<0 then find a fd
                else find a fg;;

  let rec mem a s = match s with
      Vide -> false
     |Noeud(n,_,fg,fd,_) -> let c = X.compare n a in
              if c=0 then true
              else if c<0 then mem a fd
              else mem a fg;;


  exception AddIn

  let height a = match a with
    | Vide -> 0
    | Noeud(_,_,_,_,h)-> h

  let node fg v t fd =
    Noeud(v,t,fg,fd, 1+max (height fg) (height fd))

  let balance g v t d =
    let hg = height g in
    let hd = height d in
   if hg > hd +1
    then match g with
      | Vide -> assert false
      | Noeud(vg,tg,gg,gd,hg)->
        let hgg = height gg in
        let hgd = height gd in
        if hgg > hgd then
          node  gg vg tg (node gd v t d)
        else (*hgg < hgd *)match gd with
            |Vide -> assert false
            |Noeud(vgd,tgd,gdg,gdd,_)->
            node (node gg vg tg gdg) vgd tgd (node gdd v t d)
    else if hd > hg +1 then match d with
      | Vide -> assert false
      | Noeud(vd,td,dg,dd,_)->
        let hdg = height dg in
        let hdd = height dd in
        if hdd > hdg then
          node  (node g v t dg) vd td dd
        else (*hgg < hgd *)match dg with
            |Vide -> assert false
            |Noeud(vdd,tdd,dgg,dgd,_)->
            node (node g v t dgg) vdd tdd (node dgd vd td dd)
    else (*if hg=hd then*)
        node g v t d


  let rec add x t a = match a with
      | Vide -> node empty x t empty
      | Noeud(v,t',fg,fd,_) -> let c = X.compare x v in
              if c=0 then (*x= v*)
                  node fg v t fd
              else if c<0 then (* x < v*)
                  balance (add x t fg) v t' fd
              else (* x > v*)
                  balance  fg v t' (add x t fd)

  let rec fold f s v0 = match s with
    |Vide -> v0
    |Noeud(n,t,fg,fd,_)-> let v1= f n (fold f fg v0) in
                      fold f fd v1;;


  let rec get_min s = match s with
    |Vide -> raise EmptySet
    |Noeud(n,t,Vide,_,_) -> n
    |Noeud(n,t,fg,_,_) -> get_min fg;;

  let rec equal_aux s1 s2 = match s1 with
    |Vide -> true
    |Noeud(n,t,fg,fd,_) -> mem n s2 && (find n s2) = t && equal_aux fg s2 && equal_aux fd s2;;

  let equal s1 s2 =
    equal_aux s1 s2 && equal_aux s2 s1;;

  exception NotFound
  let rec remove e s = match s with
    |Vide -> raise NotFound
    |Noeud(n,t,Vide,Vide,_)-> let c = X.compare n e in
                          if c=0 then Vide
                          else raise NotFound
    |Noeud(n,t,fg,fd,_) -> let c = X.compare n e in
                      if c > 0 then
                         let fg2= remove e fg in
                         node fg2 n t fd
                       else if c < 0 then
                         let fd2= remove e fd in
                        balance  fg n t fd2
                       else (* n=e c=0*) let newn = get_min fd in
                                        let newt = find newn fd in
                                      let nfd = remove newn fd in
                                      balance fg newn newt nfd;;

  let rec union s1 s2 = match s1 with
    |Vide -> s2
    |Noeud(n,t,fg,fd,_) -> union fd (union fg (add n t s2));;

end
