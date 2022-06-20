(* parsing util functions *)

let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" c

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)

let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let ( >>= ) = bind

let ( let* ) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then
      Some (x, ls)
    else
      None
  | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

let ws : unit parser = many whitespace >| ()

let ws1 : unit parser = many1 whitespace >| ()

let digit : char parser = satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match (cs, ls) with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c then
        loop cs xs
      else
        None
    | _ -> None
  in
  loop cs ls

let keyword (s : string) : unit parser = literal s >> ws >| ()

(* end of parser combinators *)

type const = 
    Int of int | Unit | Bool of string

type command = 
    Push of const
  |
    Pop of const
  |
    Trace of const
  |
    Add of const
  |
    Sub of const
  |
    Mul of const
  |
    Div of const




let pushInt = 
  literal "Push" >>= fun _ ->
  whitespace >>= fun _ ->
  natural >>= fun i ->
  pure (Push (Int i))

let pushFalse = 
  literal "Push" >>= fun _ ->

  whitespace >>= fun _ ->
  satisfy (fun x -> x = 'F') >>= fun _ ->
  satisfy (fun x -> x = 'a') >>= fun _ ->
  satisfy (fun x -> x = 'l') >>= fun _ ->
  satisfy (fun x -> x = 's') >>= fun _ ->
  satisfy (fun x -> x = 'e') >>= fun _ ->
  pure (Push (Bool "False"))

let pushTrue = 
  literal "Push" >>= fun _ ->
  whitespace >>= fun _ ->
  satisfy (fun x -> x = 'T') >>= fun _ ->
  satisfy (fun x -> x = 'r') >>= fun _ ->
  satisfy (fun x -> x = 'u') >>= fun _ ->
  satisfy (fun x -> x = 'e') >>= fun _ ->
  pure (Push (Bool "True"))

let pushUnit = 
  literal "Push" >>= fun _ ->
  whitespace >>= fun _ ->
  literal "()" >>= fun _ ->
  pure (Push Unit)

let pushBool = 
  pushTrue <|> pushFalse  

let pushCommand = 
  pushInt <|> pushBool <|> pushUnit

let popCommand = 
  literal "Pop" >>= fun _ ->
  whitespace >>= fun _ ->
  natural >>= fun i ->
  pure (Pop (Int i))

let traceCommand = 

  literal "Trace" >>= fun _ ->
  whitespace >>= fun _ ->
  natural >>= fun i ->
  pure (Trace (Int i))


let addCommand = 

  literal "Add" >>= fun _ -> 
  whitespace >>= fun _ ->
  natural >>= fun i ->
  pure (Add (Int i))


let subCommand = 
  literal "Sub" >>= fun _ -> 
  whitespace >>= fun _ ->
  natural >>= fun i ->
  pure (Sub (Int i))

  (*
let mulCommand = 
  failwith "Unimplemented"

let divCommand = 
  failwith "Unimplemented"
*)
let commandParser = 
  ws >>= fun _ -> 
  pushCommand <|> popCommand <|> traceCommand 
  <|> addCommand <|> subCommand 
  (*<|> mulCommand <|> divCommand *)

  >>= fun c -> ws >>= fun _ -> pure c 




(* parse (treeParser()) "(true^false)" *)

let rec string_of_const (x: const): string =
  match x with
    Int x -> string_of_int x 
  |
    Bool x -> x
  |
    Unit -> "()"


let rec drop (n:int) (l: 'a list): 'a list = 
  match (n,l) with 
    (0,_) -> l 
  |
    (n,_::t) -> (drop (n-1) t)
  |
    _ -> failwith "drop: unimplemented"

let rec take (n:int) (l: 'a list): 'a list = 
  match (n,l) with 
    (0,_) -> []
  |
    (n,h::t) -> (h::(take (n-1) t))
  |
    _ -> failwith "take: unimplemented"


let rec ints_of_Ints (big_ints:const list) : int list option = 
  match big_ints with 
    [] -> Some [] 
  |
    Int x::t -> 
    begin
      match ints_of_Ints t with 
        Some ints -> Some (x::ints)
      |
        None -> None 
    end
  |
    _ -> None 


let rec run (l: command list) (s: const list) (output: string list) =
  match l with
    [] -> output 
  |
    (Push x)::t -> run t (x::s) output
  |
    (Pop (Int n))::t -> 
    if List.length s >= n then

      run t (drop n s) output
    else
      ["Error"]
  |
    Trace (Int n)::t -> 
    if n == 1 && List.length s >= 1 then 
      run t (drop n s) (List.map string_of_const (take n s))@output
    else if List.length s >= n then 
      run t (drop n s) (List.rev (List.map string_of_const (take n s)))@output

    else 
      ["Error"]

  |

    (Add (Int n))::t -> 
    if n = 0 then
      run t (Int 0::s) output

    else if n <= List.length s then
      match ints_of_Ints (take n s) with 

        Some ints ->  
        let new_stack_tail = (drop n s) in 
        let new_stack_head = Int (List.fold_right (+) ints 0) in
        let new_stack = new_stack_head::new_stack_tail in 
        run t new_stack output  
      |
        None -> ["Error"]
    else 
      ["Error"]
  |

    (Sub (Int n))::t -> 
    if n = 0 then
      run t (Int 0::s) output

    else if n < 0 then 
      ["Error"]
    else if n <= List.length s then

      match s with 
        h::tail -> 
        let first = h in 
        let rest = (take (n-1) tail) in 
        let new_stack_tail = (drop n s) in 
        begin
          match first, ints_of_Ints rest with
            Int i, Some ints ->  
            let new_stack_head = 
              Int (List.fold_left (-) i ints) in 
            let new_stack = new_stack_head::new_stack_tail in 
            run t new_stack output

          |
            _ -> ["Error"]
        end
      |
        _ -> failwith "should not happen"
    else 
      ["Error"]

  |



    _ -> failwith "run: Unimplemented"

(* Need to add other cases for other commands e.g. pop,add, trace *)


(* TODO *)
let interp (src : string) : string list = 
  match parse (many1 commandParser) src with 
    Some (x, []) -> (run x [] [])
  |
    _ -> failwith "interp failed"


(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src




