(* Extract a given number of randomly selected elements from a list *)

(* Deterministic, for testing. Replace with Random.int to get true random results *)
let random n = 1337 mod n ;;

let rec rand_select list n =
    if n > 0 then let i = random ((List.length list)) in
        if n < 0 || n > (List.length list) then raise Not_found else (List.nth list i) :: rand_select (remove_at i list) (n-1) (*rand_select from exercise 20*)
    else []
;;

assert (rand_select [`a;`b;`c;`d;`e;`f;`g;`h] 3 = [`b;`a;`h]);; 
