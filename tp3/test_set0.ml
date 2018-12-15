(** Module de générique pour les emsembles d'entiers
 *)
open Ensembles

module Entier =
  struct
      type t = int
      let compare a b = a - b
end

module EnsEntier= MakeAvl(Entier)

let do_test (test_name,test_function,wanted_anwser,print_anwser) =
  try
    let res = test_function () in (* on effectue le test *)
    if res = wanted_anwser
    then (* Si le test est valide on affiche le nom du test suivit de "Ok" *)
      Format.printf "%s : Ok@." test_name
    else
      (* Sinon : *)
      Format.printf
        "%s : Error : wanted:=%a obtained:=%a@."
        test_name print_anwser wanted_anwser print_anwser res
  with e ->
    Format.printf "%s : Uncaught exception %s@." test_name (Printexc.to_string e)

(** [do_test (test_name,test_function,wanted_anwser,print_anwser)]
effectue un test, le vérifie et affiche le résultat
 *)


(** Quelques fonctions d'affichage *)

let fprintf_bool fmt b = Format.fprintf fmt "%b" b
(** affichage pour les résultats booléen *)

let fprintf_int fmt n = Format.fprintf fmt "%d" n
(** affichage pour les résultats booléen *)

let fprintf_set fmt s =
  Format.fprintf fmt "[%a]"
    (fun fmt s -> EnsEntier.fold (fun e _ -> Format.fprintf fmt "%d; " e) s ())
    s
(** affichage pour les résultats de type int_set *)

let fprintf_exn fmt e = Format.fprintf fmt "%s" (Printexc.to_string e)
(** affichage pour les résultats de type exn (exception) *)


exception Error of string * exn
(** Une exception pour les tests qui ne renvoient pas la bonne exception *)

    (** Les premiers tests *)
let tests_bool =
  [ "EnsEntier.empty_is_EnsEntier.empty", (fun () -> (EnsEntier.is_empty EnsEntier.empty)), true, fprintf_bool;
    "EnsEntier.empty_not_EnsEntier.mem", (fun () -> EnsEntier.mem 3 EnsEntier.empty),false,fprintf_bool;
    "EnsEntier.add_not_EnsEntier.empty",(fun () -> EnsEntier.is_empty (EnsEntier.add 3 EnsEntier.empty)),false,fprintf_bool;
    "EnsEntier.add_EnsEntier.mem_1",(fun () -> EnsEntier.mem 3 (EnsEntier.add 3 EnsEntier.empty)),true,fprintf_bool;
    "EnsEntier.add_EnsEntier.mem_2",(fun () -> EnsEntier.mem 3 (EnsEntier.add 3 (EnsEntier.add 2 EnsEntier.empty))),true,fprintf_bool;
    "EnsEntier.add_EnsEntier.mem_3",(fun () -> EnsEntier.mem 2 (EnsEntier.add 3 (EnsEntier.add 2 EnsEntier.empty))),true,fprintf_bool;
    "EnsEntier.remove_not_EnsEntier.mem",(fun () -> EnsEntier.mem 2 (EnsEntier.remove 2 (EnsEntier.add 2 (EnsEntier.add 2 EnsEntier.empty)))),false,fprintf_bool;
    "EnsEntier.union_EnsEntier.equal_1", (fun () -> EnsEntier.equal (EnsEntier.union (EnsEntier.add 1 (EnsEntier.add 3 EnsEntier.empty)) (EnsEntier.add 2 (EnsEntier.add 5 EnsEntier.empty))) (EnsEntier.add 1 (EnsEntier.add 2 (EnsEntier.add 3 (EnsEntier.add 5 EnsEntier.empty ))))), true, fprintf_bool;
  ]

let tests_int = [
     "fold", (fun () ->EnsEntier.fold (fun a b -> a + b) (EnsEntier.add 1 (EnsEntier.add 3 EnsEntier.empty)) 2), 6, fprintf_int;
    "get_min_1", (fun () -> EnsEntier.get_min (EnsEntier.add 1 (EnsEntier.add 3 EnsEntier.empty))), 1, fprintf_int;
    "get_min_2", (fun () -> EnsEntier.get_min (EnsEntier.add 4 (EnsEntier.add 3 EnsEntier.empty))), 3, fprintf_int;
  ]

let tests_exn =
  [
    "get_min_EnsEntier.empty",(fun () ->
      try
        ignore(EnsEntier.get_min EnsEntier.empty);
        raise (Error ("Should raise",EmptySet))
      with e -> e
    ),EmptySet,fprintf_exn;

    "EnsEntier.remove_in_empty",(fun()->
      try
        ignore(EnsEntier.remove 2 EnsEntier.empty);
        raise (Error ("should raise", EmptySet))
      with e -> e
    ),EmptySet,fprintf_exn;
  ]

let _ =
  List.iter do_test tests_bool;
  List.iter do_test tests_exn;
  List.iter do_test tests_int
