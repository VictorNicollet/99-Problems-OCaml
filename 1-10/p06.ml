(* Find out whether a list is a palindrome. *)

(* HINT: a palindrome is its own reverse. Use either the rev function from
   problem 5, or the built-in List.rev *)

let is_palindrome list = 
  list = List.rev list 
;;

assert (is_palindrome [ `x ; `a ; `m ; `a ; `x ]) ;;
assert (not (is_palindrome [ `a ; `b ])) ;;
