(* Calculate Euler's totient function phi(m) *)

let rec gcd a b = if b = 0 then a else gcd b (a mod b) ;;
let coprime a b = gcd a b = 1 ;;

let phi n = 
  let rec aux acc d = 
    if d < n then
      aux (if coprime n d then acc + 1 else acc) (d + 1)
    else acc
  in
  if n = 1 then 1 else aux 0 1
;;

assert (phi 10 = 4) ;;
assert (phi 13 = 12) ;;
