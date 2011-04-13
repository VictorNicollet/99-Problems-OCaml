(* Rotate a list N places to the left. *)

(* Rotation by N is the same as rotation by (list length) + N, so rotating by -2 is the
   same as rotating by (list length) - 2. *)

let split list n = 

  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l else aux (i-1) (h :: acc) t
  in

  aux n [] list
;;

let rotate list n = 
  
  let len = List.length list in 
  let n = if len = 0 then 0 else (n mod len + len) mod len in (* Compute a rotation value between 0 and len-1 *)
  if n = 0 then list else
    let a, b = split list n in  b @ a 

;;

assert (rotate [`a;`b;`c;`d;`e;`f;`g;`h] 3 = [`d;`e;`f;`g;`h;`a;`b;`c]) ;;
assert (rotate [`a;`b;`c;`d;`e;`f;`g;`h] (-2) = [`g;`h;`a;`b;`c;`d;`e;`f]) ;;
