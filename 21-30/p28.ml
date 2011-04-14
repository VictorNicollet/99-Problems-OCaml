(* Sorting a list of lists according to length of sublists. *)

(* We might not be allowed to use built-in List.sort, so here's an eight-line implementation
   of insertion sort - O(n²) time complexity. *)

let rec insert cmp e = function
  | [] -> [e]
  | h :: t as l -> if cmp e h <= 0 then e :: l else h :: insert cmp e t
;;

let rec sort cmp = function
  | [] -> []
  | h :: t -> insert cmp h (sort cmp t)
;;	     

(* Sorting according to length : prepend length, sort, remove length *)

let length_sort lists = 
  let lists = List.map (fun list -> List.length list, list) lists in
  let lists = sort (fun a b -> compare (fst a) (fst b)) lists in
  List.map snd lists
;;

assert (length_sort [ [`a;`b;`c]; [`d;`e]; [`f;`g;`h]; [`d;`e]; [`i;`j;`k;`l]; [`m;`n]; [`o] ] = 
    [[`o]; [`d; `e]; [`d; `e]; [`m; `n]; [`a; `b; `c]; [`f; `g; `h]; [`i; `j; `k; `l]]) ;;

(* Sorting according to length frequency : prepend frequency, sort, 
   remove frequency. Frequencies are extracted by sorting lengths and 
   applying RLE to count occurences of each length (see problem 10) *)

let rle list = 

  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) -> 
      if a = b then aux (count + 1) acc t 
      else aux 0 ((a, count + 1) :: acc) t
  in

  aux 0 [] list
;;

let rec assoc x = function
  | [] -> raise Not_found
  | (k,v) :: t -> if x = k then v else assoc x t
;;

let frequency_sort lists = 
  let lengths = List.map List.length lists in
  let freq = rle (sort compare lengths) in
  let by_freq = List.map (fun list -> assoc (List.length list) freq , list) lists in
  let sorted = sort (fun a b -> compare (fst a) (fst b)) by_freq in
  List.map snd sorted
;;
  
assert (frequency_sort [ [`a;`b;`c]; [`d;`e]; [`f;`g;`h]; [`d;`e]; [`i;`j;`k;`l]; [`m;`n]; [`o] ] =
    [[`i; `j; `k; `l]; [`o]; [`a; `b; `c]; [`f; `g; `h]; [`d; `e]; [`d; `e]; [`m; `n]]) ;;
