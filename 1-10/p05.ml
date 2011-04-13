(* Reverse a list. *)

let rev list = 
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t
  in aux [] list
;;

assert (rev [`a ; `b ; `c] = [`c ; `b ; `a]) ;;
