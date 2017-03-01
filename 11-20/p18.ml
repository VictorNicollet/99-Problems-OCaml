(* Extract a slice from a list *)

let slice list b e = 

  let rec aux drop take = function 
    | [] -> []
    | h :: t -> if take = 0 then 
                  [] 
                else if drop = 0 then 
                  h :: aux 0 (take - 1) t
                else
                  aux (drop - 1) take t
  in

  aux (b - 1) (e - b + 1) list
;;

assert (slice [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 7 = [`c;`d;`e;`f;`g]) ;;
