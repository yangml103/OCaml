(*
Honor code comes here:

First Name:
Last Name:
BU ID:

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)


(* 
Write an insertion sort function for lists of integers

for example,
sort [6;7;1] = [1;6;7]
*)

(* 
Hint: use the following helper function 

let rec insert (i: int) (list: int list): int list = failwith "unimplemented"

that takes a a number, an already sorted ls and returns a new sorted list with that number inserted
for example,
insert 5 [1;3;5;7] = [1;3;5;5;7]
*)

let rec insert (i: int) (list: int list): int list =
  match list with
    [] -> [i]
  |
    h::t -> if i < h then i::h::t else h::insert i t


let rec sort (ls: int list): int list = 
  match ls with
    [] -> []
  |
    h::t -> insert h (sort t)




(*
Write a zip function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, return None
your method should be tail recursive.

For example,
safe_zip_int [1;2;3;5] [6;7;8;9] = Some [(1,6);(2,7);(3,8);(5,9)]
safe_zip_int [1] [2;4;6;8] = None
safe_zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
*)


let rec safe_zip_int (ls1: int list) (ls2: int list) : ((int * int) list) option = 
  let rec aux (ls1: int list) (ls2: int list) (acc: (int*int) list) : ((int*int) list) option = 

    match (ls1,ls2) with

      ([],[]) -> Some acc
    |
      ([],_) -> None
    |
      (_,[]) -> None
    |

      (a0::at, b0::bt) -> aux at bt ((a0,b0)::acc) 

  in aux ls1 ls2 []


(*
Write a function that produces the ith Pell number:
https://en.wikipedia.org/wiki/Pell_number
https://oeis.org/A000129
your function should be tail recursive, but only needs to have the correct output up to integer overflow

pell 0 = 0
pell 1 = 1
pell 7 = 169
pell 1000000  does not stack overflow
*)


let rec pell (i: int) : int = 
  let rec aux (i:int) (pre1:int) (pre2:int) : int = 
    match i with
      0 -> pre2
    |
      _ -> aux (i-1) (2*pre1+pre2) (pre1)
  in aux i 1 0


(*
infinite precision natural numbers can be represented as lists of ints between 0 and 9

Write a function that takes an integer and represents it with a list of integers between 0 and 9 where the head ias the least signifigant digit.
If the input is negative return None

toDec 1234 = Some [4; 3; 2; 1]
toDec 0 = Some []
toDec (-1234) = None
*)

(* Hint use 
   mod 10
   / 10
*)

let rec toDec (i : int) : int list option = 
  let rec aux (i:int) (acc:int list) : int list = 

    if i < 10 then (i::acc) else aux (i/10) ((i mod 10)::acc)

  in if i < 0 then None else if i = 0 then Some [] else Some (List.rev (aux i []))



(*
Write a function that sums 2 natrual numbers as represented by a list of integers between 0 and 9 where the head ias the least signifigant digit.
Your function should be tail recursive

sum [4; 3; 2; 1] [1;0;1] = [5; 3; 3; 1]
sum [1] [9;9;9] = [0; 0; 0; 1]
sum [] [] = []
sum (nines 1000000) [1] does not stack overflow, when (nines 1000000) provides a list of 1000000 9s
*)


let rec sum (a : int list) (b : int list) : int list = 
  let rec aux (a:int list) (b: int list) (acc:int list) : int list = 
    match (a,b) with

      ([],[]) -> acc
    |
      ([],_) -> acc@b
    |
      (_,[]) -> acc@a
    |
      (a0::at,b0::bt) -> if a0 + b0 > 9 then 

        let d = a0 + b0 - 10 in 

        aux (aux at [1] []) bt (acc@[d]) else 

        aux at bt (acc@[a0+b0])

  in aux a b []

(* if a0 + b0 > 9, then put into a new list summ, break number into individual nums
   , reverse and cons to acc *)



(*
Write an infinite precision version of the pel function from before

pell2 0 = []
pell2 1 = [1]
pell2 7 = [9; 6; 1]
pell2 50 = [2; 2; 5; 3; 5; 1; 4; 2; 9; 2; 4; 6; 2; 5; 7; 6; 6; 8; 4]

*)


let rec pell2 (i: int) : int list = 
  let rec aux (i:int ) (pre1:int list) (pre2: int list): int list = 
    (* if i = 0 then [] else *)
    match i with
      0 -> pre2
    |
      _ -> aux (i-1) (sum (sum pre1 pre1) pre2) pre1
  in aux i [1] []
