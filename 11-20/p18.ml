(* Extract a slice from a list *)

let slice list b e = 

  let rec take n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n-1) t
  in

  let rec drop n = function
    | [] -> []
    | h :: t as l -> if n = 0 then l else drop (n-1) t
  in

  take (e - b + 1) (drop (b - 1) list) 
;;

assert (slice [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 7 = [`c;`d;`e;`f;`g]) ;;
