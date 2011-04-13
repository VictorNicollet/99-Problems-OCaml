(* Find the number of elements of a list *)

let length list =
  let rec aux n = function
    | [] -> n
    | _::t -> aux (n+1) t
  in aux 0 list
;;

(* This function is tail-recursive: it uses a constant amount of 
   stack memory regardless of list size. *)

assert (length [ `a ; `b ; `c] = 3) ;;
assert (length [] = 0) ;;
