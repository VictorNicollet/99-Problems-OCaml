(* Extract a slice from a list *)

let slice list i k  =
	let rec aux current i k = function
    	| [] -> []
        | h :: t ->
        	if (i <= current && current <= k) then h :: aux (current + 1) i k t
            else  aux (current + 1) i k t in
     aux 0 i k list;;
