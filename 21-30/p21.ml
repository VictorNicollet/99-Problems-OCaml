(* Insert an element at a given position into a list *)

let rec insert_at x n = function
  | [] -> []
  | h :: t as l -> if n = 1 then x :: l else h :: insert_at x (n-1) t
;;

assert (insert_at `alfa 2 [`a;`b;`c;`d] = [`a;`alfa;`b;`c;`d]) ;;
