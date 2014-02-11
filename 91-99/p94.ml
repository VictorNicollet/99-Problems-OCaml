(*

Given a list of integer numbers, find a correct way of inserting arithmetic signs (operators) such that the result is a correct equation. 

Example: With the list of numbers [2;3;5;7;11] we can form the equations 2-3+5+7 = 11 or 2 = (3*5+7)/11 (and ten others!).

*)

type op = Add | Sub | Mul | Div

type expr = 
  | Num of int
  | App of expr * op * expr

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
      
let rec string_of_expr = function
  | Num x -> string_of_int x
  | App (e1,op,e2) -> Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_op op) (string_of_expr e2)

let rev_flatten l = 
  List.fold_left (fun acc x -> List.fold_left (fun acc y -> y::acc) acc x) [] l

let ops = [Add;Sub;Mul;Div]

let apply x op y =
  match op with
    | Add -> x + y
    | Sub -> x - y
    | Mul -> x * y
    | Div -> x / y

let rec value = function
  | Num x -> x
  | App (x, op, y) -> apply (value x) op (value y)

let legal x op y =
  match op with
    | Add -> true
    | Sub -> true
    | Mul -> true
    | Div -> y <> 0 && x mod y = 0

(*
  splits [2;3;5;7;11] = [([2; 3; 5; 7], [11]); ([2; 3; 5], [7; 11]); ([2; 3], [5; 7; 11]); ([2], [3; 5; 7; 11])] 
*)
let splits l = 
  let rec split front acc = function
    | [] | _::[] -> acc
    | hd::tl -> 
      let new_front = hd::front in split new_front ((List.rev new_front, tl)::acc) tl
  in 
  split [] [] l

(* generate list: [(e1+e2,v);(e1*e2,v)...] depend on legal *)
let combine (e1,v1) (e2,v2) =
  let rec comb acc = function
    | [] -> acc
    | op::tl when legal v1 op v2 -> comb ((App (e1,op,e2),apply v1 op v2)::acc) tl
    | op::tl -> comb acc tl
  in 
  comb [] ops

(* every element from l1 pairs of every element from l2 via f *)
let fuse f l1 l2 = 
  List.rev_map (
    fun x -> List.rev_map (fun y -> f x y) l2 |> rev_flatten) l1 |> rev_flatten

let rec make_expr = function
  | [] -> []
  | x::[] -> [(Num x, x)]
  | _ as l -> splits l |> List.rev_map (fun (a,b) -> fuse combine (make_expr a) (make_expr b)) |> rev_flatten

let eq (e1,v1) (e2,v2) = if v1 = v2 then [Some (e1,e2,v1)] else [None]

let find_eq l =
  splits l |> 
      List.rev_map (fun (a,b) -> fuse eq (make_expr a) (make_expr b) |> 
	  List.filter ((<>)None)) |> 
	  rev_flatten |>
	      List.fold_left (fun acc x -> match x with None -> acc | Some v -> v::acc) [] |>
		  List.iter (fun (e1,e2,v) -> Printf.printf "%s = %s\n" (string_of_expr e1) (string_of_expr e2)) 


let _ = find_eq [2;3;5;7;11]
