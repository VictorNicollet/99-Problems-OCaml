(* Run-length encoding of a list (direct solution) *)

(* Since Objective Caml cannot represent a list of both normal values and N,X values, one needs to 
   define a type for that. *)

type 'a rle = 
  | One of 'a 
  | Many of (int * 'a)
;;

let encode list = 

  let rle count x = if count = 0 then One x else Many (count + 1, x) in

  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> rle count x :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t else aux 0 (rle count a :: acc) t
  in

  List.rev (aux 0 [] list) 
;;

assert (encode [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = 
    [Many (4,`a) ; One `b ; Many (2,`c) ; Many (2,`a) ; One `d ; Many (4,`e)]) ;;

