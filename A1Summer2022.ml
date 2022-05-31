(*
Honor code comes here:

First Name: Man Lung
Last Name: Yang
BU ID: U73532271

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)



(* 
a print_list function useful for debugging.
*)

let rec print_list (ls: int list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | e::[] -> print_int e
    | e::l -> 
      let _ = print_int e 
      in let _ = print_string "; " 
      in aux l

  in let _ = print_string "[" 
  in let _ = aux ls
  in         print_string "]" 





(* Problems *)

(*
TODO: Write a function called between that lists the integers between two integers (inclusive)
If the first number is greater then the second return the empty list
the solution should be tail recursive

For example,
between 4 7 = [4; 5; 6; 7]
between 3 3 = [3]
between 10 2 = []
between 4 1000000 does not stack overflow
*)


let rec between (n:int) (e:int): int list = 
  let rec aux (n:int) (e:int) (acc:int list): int list = 
    if n > e then
      acc
    else
      aux (n+1) e (n::acc)

  in List.rev (aux n e [])





(*
TODO: Write a zip function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, combine as long as possible
your method should be tail recursive.

For example,
zip_int [1;2;3;5] [6;7;8;9] = [(1,6);(2,7);(3,8);(5,9)]
zip_int [1] [2;4;6;8] = [(1,2)]
zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
*)


let zip_int (a: int list) (b: int list): (int * int) list = 

  let rec aux (a:int list) (b:int list) (acc: (int * int) list): (int*int) list =


    match (a,b) with 
      ([], _) -> acc
    |
      (_, []) -> acc
    |
      (a0::at, b0::bt) -> aux at bt ((a0,b0)::acc)

  (*failwith ("a0 is " ^ string_of_int a0 ^ ", b0 is " ^ string_of_int b0) *)

  in List.rev (aux a b [])







(*
TODO: Write a dotProduct function for lists of integers,
If the two list are of unequal lengths then return 0

For example,
dotProduct [1;2;3;4] [6;7;8;9] = 80            (since 1*6+2*7+3*8+4*9 = 80)
dotProduct [1;2;3;4] [6] = 0
*)


let dotProduct (x: int list) (y: int list): int = 

  let rec aux (x:int list) (y: int list) (acc: int):int = 

    match (x, y) with 

      ([],[]) -> acc
    |
      (a0::at, b0::bt) -> aux at bt (acc + a0 * b0)
    |
      _ -> 0

  in aux x y 0

  