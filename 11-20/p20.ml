(* Remove the K'th element from a list *)

let rec remove_at n = function
  | [] -> []
  | h :: t -> if n = 1 then t else h :: remove_at (n-1) t
;;

assert (remove_at 2 [`a;`b;`c;`d] = [`a;`c;`d]) ;;
