(* Goldbach's conjecture *)

let divides d n = (n mod d) = 0 ;;

let is_prime n = 
  let n = max n (-n) in
  let rec aux d = 
    d * d > n || not (divides d n) && aux (d+1)
  in
  aux 2
;;

let goldbach n = 
  let rec aux d = 
    if is_prime d && is_prime (n - d) then (d, n-d) else
      aux (d+1) 
  in aux 2
;;

let rec goldbach_list a b = 
  if a > b then [] else
    if a mod 2 = 1 then goldbach_list (a+1) b else
      (a, goldbach a) :: goldbach_list (a+2) b
;;

assert (goldbach_list 9 20 = 
    [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
     (20, (3, 17))]) ;;

let goldbach_limit a b lim = 
  List.filter (fun (_,(a,b)) -> a > lim && b > lim) (goldbach_list a b)
;;

assert (goldbach_limit 1 2000 50 = 
    [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
     (1928, (61, 1867))]) ;;
