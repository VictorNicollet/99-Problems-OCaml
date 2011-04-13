(* Find the K'th element of a list *)

let rec at k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k-1) t
;;

(* If k is smaller than 1, it will return None after traversing the entire list. *)

assert (at 3 [ `a ; `b ; `c ; `d ; `e ] = Some `c) ;;
assert (at 3 [ `a ] = None) ;;
