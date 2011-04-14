(* Compare the two euler computation methods. *)

let mod_count = ref 0 ;;

let divides d n = incr mod_count ; (n mod d) = 0 ;;
let rec gcd a b = if b = 0 then a else (incr mod_count ; gcd b (a mod b)) ;;

(* Naive method *)

let coprime a b = gcd a b = 1 ;;

let naive_phi n = 
  mod_count := 0 ;
  let rec aux acc d = 
    if d < n then
      aux (if coprime n d then acc + 1 else acc) (d + 1)
    else acc
  in
  let result = if n = 1 then 1 else aux 0 1 in
  Printf.printf "Naive: %d divides\n" !mod_count ;
  result
;;

(* Improved method *)

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

let rec pow n p = if p < 1 then 1 else n * pow n (p-1) ;;

let improved_phi n = 
  mod_count := 0 ;
  let rec aux acc = function
    | [] -> acc
    | (p,m) :: t -> aux ((p - 1) * pow p (m - 1) * acc) t
  in 
  let result = aux 1 (factors n) in 
  Printf.printf "Improved: %d divides\n" !mod_count ;
  result
;;

(* Comparison *)

let _ = naive_phi 10090 ;;    (* 77393 divides *)
let _ = improved_phi 10090 ;; (*  1010 divides *)
