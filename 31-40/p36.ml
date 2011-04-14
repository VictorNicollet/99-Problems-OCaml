(* Determine the prime factors of of a given positive integer. *)

let divides d n = (n mod d) = 0 ;;

let factors n = 
  let rec aux d n = 
    if n = 1 then [] else 
      if divides d n then 
	match aux d (n / d) with 
	  | (h,n) :: t when h = d -> (h,n+1) :: t
	  | l -> (d,1) :: l
      else aux (d+1) n
  in
  aux 2 n
;;

assert (factors 315 = [3,2 ; 5,1 ; 7,1]) ;;
  
