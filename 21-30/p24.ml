(* Lotto: draw N different random numbers from the set 1 -- M *)

(* From problem 22 ---------------- *)

let (--) a b = 
  let rec aux a b = 
    if a > b then [] else a :: aux (a+1) b
  in

  if a > b then List.rev (aux b a) else aux a b
;;

(* From problem 23 ---------------- *)

(* Deterministic, for testing. Replace with Random.int to get true random results *)
let random n = 1337 mod n ;;

let rec rand_select list n = 

  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then h, acc @ t else extract (h::acc) (n-1) t
  in

  let extract_rand list len = 
    extract [] (random len) list 
  in

  let rec aux n acc list len = 
    if n = 0 then acc else
      let picked, rest = extract_rand list len in 
      aux (n-1) (picked :: acc) rest (len-1)
  in

  let len = List.length list in

  aux (min n len) [] list len 
;;

(* The solution ---------------- *)

let lotto_select n m = rand_select (1 -- m) n ;;

assert (lotto_select 6 49 = [37;8;25;21;43;15]) ;;

