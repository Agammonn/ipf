type formule = Var of int | ET of (formule*formule) | OU of (formule*formule) | NON of (formule)

let rec fnc f = match f with
  Var(a) -> Var(a)
  |NON(Var(a)) -> NON(Var(a))
  |NON(ET(f1,f2)) -> fnc (OU(NON(f1),NON(f2)))
  |NON(OU(f1,f2)) -> ET(fnc (NON(f1)),fnc (NON(f2)))
  |NON(NON(f)) -> f
  |ET(f1,f2)->ET(fnc f1,fnc f2)
  |OU(f1,f2)->let f0'= fnc f1 in let f1'=fnc f1 in (match f0',f1' with
                ET(f3,f4), ET(f5,f6) -> ET(ET(OU(f3,f5),OU(f3,f6)),ET(OU(f4,f5),OU(f4,f6)))
                |ET(f3,f4),OU(f5,f6) ->ET(OU(f3,OU(f5,f6)),OU(f4,OU(f5,f6)))
                |OU(f5,f6),ET(f3,f4) ->ET(OU(f3,OU(f5,f6)),OU(f4,OU(f5,f6)))
                |OU(_),OU(_)         -> OU(f0',f1')
                |OU(f5,f6),Var(a)    ->OU(OU(f5,f6),Var(a))
                |Var(a),OU(f5,f6)    ->OU(OU(f5,f6),Var(a))
                |ET(f5,f6),Var(a)    ->ET(OU(f5,Var(a)),OU(f6,Var(a)))
                |Var(a),ET(f5,f6)    ->ET(OU(f5,Var(a)),OU(f6,Var(a)))
                |Var(a),Var(b)       -> OU(Var(a),Var(b)))


let ex1= OU(ET(Var(1),Var(2)),OU(Var(3),ET(Var(4),Var(5))));;

fnc ex1;;
