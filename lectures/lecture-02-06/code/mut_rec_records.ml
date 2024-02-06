(* Mutually recursive types *)

type linked_list = {head : int ; tail : maybetail}
and  maybetail = Nope | Yep of linked_list

let example = {
  head = 1 ;
  tail = Yep {
    head = 2 ;
    tail = Yep {
      head = 3;
      tail = Nope ;
    }
  }
}