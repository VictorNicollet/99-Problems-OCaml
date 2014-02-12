(*

Syntax checker. (medium)

In a certain programming language (Ada) identifiers are defined by the syntax diagram >(railroad chart) opposite. Transform the syntax diagram into a system of syntax >diagrams which do not contain loops; i.e. which are purely recursive. Using these >modified diagrams, write a function identifier : string -> bool that can check whether or >not a given string is a legal identifier.

*)

let is_empty s = String.length s = 0

let is_letter = function 'a' .. 'z' | 'A' .. 'Z'  -> true | _ -> false

let is_alphanumeri = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

let is_fst_letter s = (not (is_empty s)) && (is_letter s.[0])

let identifier s =
  let n = String.length s in
  let rec check i =
    if i = n then true
    else if is_alphanumeri s.[i] then check (i+1)
    else if s.[i] = '-' && i <> n-1 && is_alphanumeri s.[i+1] then check (i+2)
    else false
  in 
  is_fst_letter s && check 0
      
let _ = List.map identifier ["this-is-a-long-identifier";"this-ends-in-";"two--hyphens";"a-;b";"9d";"-2";"strange%%"] 
