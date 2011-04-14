(* Determine whether a given integer number is prime. *)

let divides d n = (n mod d) = 0 ;;

let is_prime n = 
  let n = max n (-n) in
  let rec aux d = 
    d * d > n || not (divides d n) && aux (d+1)
  in
  aux 2
;;

assert (is_prime 7) ;;
assert (not (is_prime 12)) ;;
