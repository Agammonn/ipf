
open Graph;;
let g = GraphAVL.empty;;

Printf.printf "Empty test execept true : %b \n" (GraphAVL.is_empty g);;

let g = GraphAVL.add_vertex "t" g;;

Printf.printf "Empty test execept false : %b \n" (GraphAVL.is_empty g);;
