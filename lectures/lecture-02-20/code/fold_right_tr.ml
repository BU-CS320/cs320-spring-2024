let fold_right_tr op l base =
  List.fold_left
    (fun x y -> op y x)
    base
    (List.rev l)


(*
1 -r (2 -r (3 -r (4 -r 0))) =
1 -r (2 -r (3 -r (0 - 4)))  =
1 -r (2 -r ((0 - 4) - 3))   =
1 -r (((0 - 4) - 3) - 2)    =
(((0 - 4) - 3) - 2) - 1
*)