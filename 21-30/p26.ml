(* Generate the combinations of K distinct objects chosen from the N elements of a list. *)

(* This is actually fairly easy in prolog (the original language for the problem), but it gets
   a little bit harder for OCaml. The code below can be understood as an imperative traversal,
   which uses a provided 'emit' function to emit all possible lists of size k. *)

let extract k list = 

  let rec aux k acc emit = function
    | [] -> acc
    | h :: t -> 
      if k = 1 then aux k (emit [h] acc) emit t else 
	let new_emit x = emit (h :: x) in
	aux k (aux (k-1) acc new_emit t) emit t
  in

  let emit x acc = x :: acc in

  aux k [] emit list 
;;

assert (extract 2 [`a;`b;`c;`d] = [[`c;`d]; [`b;`d]; [`b;`c]; [`a;`d]; [`a;`c]; [`a;`b]]) ;;
