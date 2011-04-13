(* Pack consecutive duplicates of list elements into sublists. *)

(* Since Objective Caml cannot represent a list of both normal values and N,X values, one needs to 
   define a type for that. *)

type 'a rle = 
  | One of 'a 
  | Many of (int * 'a)
;;

let pack list = 

  let rec aux current acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (a :: current) acc t else aux [] ((a :: current) :: acc) t
  in

  List.rev (aux [] [] list) 
;;

let encode list =
  let rec aux = function 
    | [] -> []
    | [] :: t -> aux t
    | [x] :: t -> One x :: aux t
    | (x :: l) :: t -> Many (1 + List.length l , x) :: aux t
  in

  aux (pack list) 
;;

assert (encode [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = 
    [Many (4,`a) ; One `b ; Many (2,`c) ; Many (2,`a) ; One `d ; Many (4,`e)]) ;;

