(*

Sudoku. (medium)

Sudoku puzzles go like this:

   Problem statement                 Solution

    .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7
            |         |                      |         |
    6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
            |         |                      |         |
    5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
    --------+---------+--------      --------+---------+--------
    3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
            |         |                      |         |
    .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
            |         |                      |         |
    .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
    --------+---------+--------      --------+---------+--------
    1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
            |         |                      |         |
    .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
            |         |                      |         |
    2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8

Every spot in the puzzle belongs to a (horizontal) row and a (vertical) column, as well as to one single 3x3 square (which we call "square" for short). At the beginning, some of the spots carry a single-digit number between 1 and 9. The problem is to fill the missing spots with digits in such a way that every number between 1 and 9 appears exactly once in each row, in each column, and in each square.

*)

let solve sudoku =

  (* Is there an n >= i >= m such that f(i) is true?  *)
  let rec find m n f = n >= m && (f n || find m (n-1) f) in

  (* Does value 'v' exist in any line, column or square
     containing cell (i,j) ? *)
  let exists v i j =
    let x  = i - (i mod 3) and y = j - (j mod 3) in
    find 0 8 (fun k ->
      sudoku.(i).(k) = v
      || sudoku.(k).(j) = v
      || sudoku.(x + (k mod 3)).(y + (k / 3)) = v)
  in

  (* Recursive solver. Not tail-recursive, but maximum stack depth is 81.
     Returns true if solution is found (sudoku then contains that solution). *)
  let rec solve i j =
    if i = 9 then
      j = 8 || solve 0 (j+1)
    else if sudoku.(i).(j) > 0 then
      solve (i+1) j
    else
      find 1 9 (fun v ->
        if exists v i j then false else
          ( sudoku.(i).(j) <- v ;
            if solve (i+1) j then true else
              (sudoku.(i).(j) <- 0 ; false )))
  in

  solve 0 0

(* test *)

let _ = 	  
  let b = 
    [|
      [|0;  0;  4;  8;  0;  0;  0;  1;  7|];
      [|6;  7;  0;  9;  0;  0;  0;  0;  0|];     
      [|5;  0;  8;  0;  3;  0;  0;  0;  4|];     
      [|3;  0;  0;  7;  4;  0;  1;  0;  0|];    
      [|0;  6;  9;  0;  0;  0;  7;  8;  0|];     
      [|0;  0;  1;  0;  6;  9;  0;  0;  5|];      
      [|1;  0;  0;  0;  8;  0;  3;  0;  6|];     
      [|0;  0;  0;  0;  0;  6;  0;  9;  1|];    
      [|2;  4;  0;  0;  0;  1;  5;  0;  0|];
    |]
  in
  if solve b then b else failwith "Cannot solve!"

  

