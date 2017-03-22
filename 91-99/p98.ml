(*

Nonograms. (hard)

Around 1994, a certain kind of puzzles was very popular in England. The "Sunday Telegraph" newspaper wrote: "Nonograms are puzzles from Japan and are currently published each week only in The Sunday Telegraph. Simply use your logic and skill to complete the grid and reveal a picture or diagram." As an OCaml programmer, you are in a better situation: you can have your computer do the work!

The puzzle goes like this: Essentially, each row and column of a rectangular bitmap is annotated with the respective lengths of its distinct strings of occupied cells. The person who solves the puzzle must complete the bitmap given only these lengths.

          Problem statement:          Solution:

          |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3
          |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1
          |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2
          |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2
          |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6
          |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5
          |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6
          |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1
          |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2
           1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3
           2 1 5 1                     2 1 5 1
For the example above, the problem can be stated as the two lists [[3];[2;1];[3;2];[2;2];[6];[1;5];[6];[1];[2]] and [[1;2];[3;1];[1;5];[7;1];[5];[3];[4];[3]] which give the "solid" lengths of the rows and columns, top-to-bottom and left-to-right, respectively. Published puzzles are larger than this example, e.g. 25×20, and apparently always have unique solutions.

(* example pending *);;

*)
let ints i n =
  let rec gen j acc =
    if j = n then acc
    else gen (j+1) (i::acc)
  in 
  gen 0 []

let zeros n = ints 0 n
let ones n = ints 1 n
let sum l = List.fold_left (+) 0 l

let prefix_zeros l n =
  if l = [] then [(zeros n, 0)] 
  else 
    let rec prefix i acc =
      if i > n then acc
      else prefix (i+1) (((List.rev_append (zeros i) l), n-i)::acc)
    in 
    prefix 1 [(l, n)]

let rev_flatten l =
  let rec flat acc = function
    | [] -> acc
    | hd::tl -> flat (List.rev_append hd acc) tl
  in 
  flat [] l

let seqs_from_list n l =
  let rec fuse tl (l,j) = List.rev_map (fun x -> l@x) (gen j tl) and 
      gen i = function
	| [] -> List.map fst (prefix_zeros [] i)
	| hd::[] -> prefix_zeros (ones hd) i |> List.rev_map (fuse []) |> rev_flatten
	| hd::tl -> prefix_zeros ((ones hd)@[0]) (i-1) |> List.map (fuse tl) |> rev_flatten
  in 
  gen (n-(sum l)) l

let seqs_to_list l = 
  let acc, s = List.fold_left (fun (acc, s) x -> if x = 1 then acc, (s+1) else if s = 0 then acc,0 else s::acc,0) ([],0) l in
  if s = 0 then List.rev acc
  else List.rev (s::acc)

let rec valid f l2 rows = 
  match l2 with
    | [] -> true
    | hd::tl -> 
      if f (List.map List.hd rows |> seqs_to_list) hd then List.map List.tl rows |> valid f tl
      else false

let solve l1 l2 =
  let n = List.length l2 in
  let row_seqs = List.map (seqs_from_list n) l1 in
  let rec run acc seqs seq = 
    match seq with
      | [] -> []
      | seq_hd::seq_tl ->
	let rows = acc@[seq_hd] in
	match seqs with
	  | [] -> if valid (=) l2 rows then rows else run acc [] seq_tl
	  | seqs_hd::seqs_tl ->
	    if valid (<=) l2 rows then 
	      let r = run rows seqs_tl seqs_hd in
	      if r = [] then run acc seqs seq_tl
	      else r
	    else run acc seqs seq_tl
  in 
  run [] (List.tl row_seqs) (List.hd row_seqs)

let l1, l2 = [[3];[2;1];[3;2];[2;2];[6];[1;5];[6];[1];[2]], [[1;2];[3;1];[1;5];[7;1];[5];[3];[4];[3]]
      
   
