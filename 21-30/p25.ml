(* Generate a random permutation of the elements of a list. *)

let random n = 1337 mod n ;;

let permutation list =
    rand_select list (List.length list)
;;

assert (permutation [`a;`b;`c;`d;`e;`f] = [`f; `c; `b; `e; `d; `a]) ;;


