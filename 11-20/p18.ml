(* Extract a slice from a list *)

let slice list i k  =
	let rec aux current i k = function
    	| [] -> []
        | h :: t ->
        	if (i <= current && current <= k) then h :: aux (current + 1) i k t
            else  aux (current + 1) i k t in
     aux 0 i k list;;

assert (slice [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 7 = [`c;`d;`e;`f;`g]) ;;
