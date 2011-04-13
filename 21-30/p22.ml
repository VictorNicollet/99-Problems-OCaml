(* Create a list containing all integers within a given range. *)

let (--) a b = 
  let rec aux a b = 
    if a > b then [] else a :: aux (a+1) b
  in

  if a > b then List.rev (aux b a) else aux a b
;;

assert (4 -- 9 = [4;5;6;7;8;9]) ;;
assert (9 -- 4 = [9;8;7;6;5;4]) ;;
