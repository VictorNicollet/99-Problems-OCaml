(* Pack consecutive duplicates of list elements into sublists. *)

let pack l =
  
    let rec aux sublist ll = match sublist,ll with
      | _, [] -> [sublist]
      | [], h::t -> aux [h] t
      | x::yy, h::t ->  if x=h then aux (h::(x::yy)) t else (x::yy)::(aux [] (h::t))
    in
    
    aux [] l

assert (pack [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`d;`e;`e;`e;`e] = 
    [[`a;`a;`a;`a];[`b];[`c;`c];[`a;`a];[`d;`d];[`e;`e;`e;`e]]) ;;

