type const =
  | Unit
  | Int of int
  | Bool of bool
  | Name of string

type cmd =
  | Push of const
  | Pop of int
  | Add of int
  | Sub of int
  | Mul of int
  | Div of int
  | Trace of int
  | And 
  | Or 
  | Not
  | Equal
  | Lte
  | Local
  | Global
  | Lookup
  | Begin 
  | End
  | If 
  | Else

and cmds = cmd list

type env = (string * value) list

and value =
  | VUnit
  | VInt of int
  | VBool of bool
  | VName of string

type stack = value list

type log = string list

let string_of_value v =
  match v with
  | VUnit -> "()"
  | VInt i -> string_of_int i
  | VBool b ->
    if b then
      "True"
    else
      "False"
  | VName x -> x

let debug v =
  match v with
  | VUnit -> "VUnit"
  | VInt i -> string_of_int i
  | VBool b ->
    if b then
      "V(True)"
    else
      "V(False)"

  | VName x -> x 

let rec addn n ls =
  if n < 0 then
    None
  else if n = 0 then
    Some (0, ls)
  else
    match ls with
    | VInt x :: ls -> (
        match addn (n - 1) ls with
        | Some (y, ls) -> Some (x + y, ls)
        | None -> None)
    | _ -> None

let subn n ls =
  if n < 0 then
    None
  else if n = 0 then
    Some (0, ls)
  else
    match ls with
    | VInt x :: ls -> (
        match addn (n - 1) ls with
        | Some (y, ls) -> Some (x - y, ls)
        | None -> None)
    | _ -> None

let rec muln n ls =
  if n < 0 then
    None
  else if n = 0 then
    Some (1, ls)
  else
    match ls with
    | VInt x :: ls -> (
        match muln (n - 1) ls with
        | Some (y, ls) -> Some (x * y, ls)
        | None -> None)
    | _ -> None

let rec divn n ls =
  if n < 0 then
    None
  else if n = 0 then
    Some (1, ls)
  else
    match ls with
    | VInt x :: ls -> (
        match muln (n - 1) ls with
        | Some (0, ls) -> None
        | Some (y, ls) -> Some (x / y, ls)
        | None -> None)
    | _ -> None

let rec popn n ls =
  if n < 0 then
    None
  else if n = 0 then
    Some ls
  else
    match ls with
    | _ :: ls -> (
        match popn (n - 1) ls with
        | Some ls -> Some ls
        | None -> None)
    | _ -> None

let rec tracen n ls =
  if n < 0 then
    None
  else if n = 0 then
    Some ([], ls)
  else
    match ls with
    | v :: ls -> (
        match tracen (n - 1) ls with
        | Some (log, ls) -> Some (string_of_value v :: log, ls)
        | None -> None)
    | _ -> None

let and2 ls = 
  match ls with 
    (VBool b1)::(VBool b2)::t -> Some ((b1 && b2),t)
  |
    _ -> None

let or2 ls = 
  match ls with 
    (VBool b1)::(VBool b2)::t -> Some ((b1 || b2),t)
  |
    _ -> None

let not2 ls = 
  match ls with 
    (VBool b1)::t -> Some ((not b1),t)
  |
    _ -> None

let equal2 ls =
  match ls with
    (VInt i1)::(VInt i2)::t -> Some ((i1 = i2),t)
  |
    _ -> None 

let lte2 ls = 
  match ls with 
    (VInt b1)::(VInt b2)::t -> Some ((b1 <= b2),t)
  |
    _ -> None

let local2 ls = 
  match ls with 
    (VName b1)::x::t -> Some ((b1,x),t)
  |
    _ -> None

let global2 ls = 
  match ls with 
    (VName b1)::x::t -> Some ((b1,x),t)
  |
    _ -> None

let lookup2 ls env_loc env_glob = 
  match ls with 

    (VName x)::t ->  (match (List.find_opt (fun (name,_) -> name = x) env_loc) with
        Some i -> Some (i,t)
      |
        None -> match (List.find_opt (fun (name,_) -> name = x) env_glob) with
          Some p -> Some (p,t) 
        |
          _ -> None )

  |
    _ -> None

let rec b_e_finder l_r_cmds num_begins num_ends = 

  match (l_r_cmds) with
    ([],r) -> None
  |
    (End::tail,r) -> if (num_begins = num_ends+1) then Some (tail,List.rev r)
    else b_e_finder (tail,End::r) num_begins (num_ends + 1)
  |
    (Begin::tail, r) -> b_e_finder (tail,Begin::r) (num_begins+1) num_ends

  |
    (If::tail, r) -> b_e_finder (tail,If::r) (num_begins+1) num_ends
  |
    (h::t,r) -> b_e_finder (t,h::r) num_begins num_ends

let rec if_else_finder l_r_cmds num_ifs num_elses = 

  match (l_r_cmds) with
    ([],r) -> None
  |
    (Else::tail,r) -> if (num_ifs = num_elses+1) then Some (tail,List.rev r)
    else if_else_finder (tail,Else::r) num_ifs (num_elses + 1)
  |
    (If::tail, r) -> if_else_finder (tail,If::r) (num_ifs+1) num_elses

  |
    (h::t,r) -> if_else_finder (t,h::r) num_ifs num_elses

let rec else_end_finder l_r_cmds num_elses num_ends = 

  match (l_r_cmds) with
    ([],r) -> None
  |
    (End::tail,r) -> if (num_elses = num_ends+1) then Some (tail,List.rev r)
    else else_end_finder (tail,End::r) num_elses (num_ends + 1)
  |
    (Else::tail, r) -> else_end_finder (tail,Else::r) (num_elses+1) num_ends
  |
    (Begin::tail,r) -> else_end_finder (tail,Begin::r) (num_elses+1) num_ends

  |
    (h::t,r) -> else_end_finder (t,h::r) num_elses num_ends


let rec if2 ls_st = 
  match ls_st with
    (VBool v::rest) -> Some (v,rest)
  |
    _ -> None


let rec eval (st : stack) (log : log) (cmds : cmds) (env_loc: env) (env_glob:env): log * stack option * cmds * env=
  match cmds with
  | Push cst :: cmds -> (
      match cst with
      | Unit -> eval (VUnit :: st) log cmds env_loc env_glob
      | Int i -> eval (VInt i :: st) log cmds env_loc env_glob
      | Bool b -> eval (VBool b :: st) log cmds env_loc env_glob
      | Name x -> eval (VName x::st) log cmds env_loc env_glob) 
  | Pop n :: cmds -> (
      match popn n st with
      | Some st -> eval st log cmds env_loc env_glob
      | _ -> (log, None,cmds,env_glob))
  | Add n :: cmds -> (
      match addn n st with
      | Some (x, st) -> eval (VInt x :: st) log cmds env_loc env_glob
      | _ -> (log, None,cmds,env_glob))
  | Sub n :: cmds -> (
      match subn n st with
      | Some (x, st) -> eval (VInt x :: st) log cmds env_loc env_glob
      | _ -> (log, None,cmds,env_glob))
  | Mul n :: cmds -> (
      match muln n st with
      | Some (x, st) -> eval (VInt x :: st) log cmds env_loc env_glob
      | _ -> (log, None,cmds,env_glob))
  | Div n :: cmds -> (
      match divn n st with
      | Some (x, st) -> eval (VInt x :: st) log cmds env_loc env_glob
      | _ -> (log, None,cmds,env_glob))
  | Trace n :: cmds -> (
      match tracen n st with
      | Some (lg, st) -> eval st (List.rev lg @ log) cmds env_loc env_glob
      | _ -> (log, None,cmds,env_glob))
  | And ::cmds -> (
      match and2 st with
        Some (x,st) -> eval (VBool x::st) log cmds env_loc env_glob
      |
        _ -> (log, None,cmds,env_glob)

    )
  |
    Or ::cmds -> (
      match or2 st with
        Some (x,st) -> eval (VBool x::st) log cmds env_loc env_glob
      |
        _ -> (log, None,cmds,env_glob)

    )

  |
    Not ::cmds -> (
      match not2 st with
        Some (x,st) -> eval (VBool x::st) log cmds env_loc env_glob
      |
        _ -> (log, None,cmds,env_glob)
    )

  |
    Equal ::cmds -> (
      match equal2 st with
        Some (x,st) -> eval (VBool x::st) log cmds env_loc env_glob
      |
        _ -> (log, None,cmds,env_glob)
    )

  |
    Lte ::cmds -> (
      match lte2 st with
        Some (x,st) -> eval (VBool x::st) log cmds env_loc env_glob
      |
        _ -> (log, None,cmds,env_glob)
    )
  |
    Local ::cmds -> (
      match local2 st with 
        Some((b1,x),st) -> eval (VUnit::st) log cmds ((b1,x)::env_loc) env_glob
      |
        _ -> (log, None,cmds,env_glob)
    )

  |

    Global ::cmds -> (
      match global2 st with 
        Some((b1,x),st) -> eval (VUnit::st) log cmds env_loc ((b1,x)::env_glob)
      |
        _ -> (log, None,cmds,env_glob)
    )
  |

    Lookup ::cmds -> (
      match lookup2 st env_loc env_glob with
        Some ((x,value),t) -> eval (value::t) log cmds env_loc env_glob
      |
        _ -> (log, None,cmds,env_glob)
    )

  |
    Begin ::cmds -> (
      match (b_e_finder (cmds,[]) 1 0) with 
        Some (rest,b_e) -> (let (l,s,c,e) = (eval [] log b_e env_loc env_glob) in 
                            match s with 
                              Some (h::t) -> (eval (h::st) l rest env_loc e)
                            |
                              _ -> (log,None,cmds,env_glob)

                           )
      |
        None -> (log,None,cmds,env_glob)
    )

  |
    End ::cmds -> (
      (log,None, cmds, env_glob)

    )

  |
    If ::cmds -> (
      match (if_else_finder (cmds,[]) 1 0) with 
      | Some (rest1, if_else) -> (match else_end_finder (rest1,[]) 1 0 with
          |Some (rest2, else_end) -> (match (if2 st) with 
              |Some (x,rest) -> if x then (eval rest log (if_else@rest2) env_loc env_glob) else (eval rest log (else_end@rest2) env_loc env_glob)
              | _ -> (log, None, cmds, env_glob))
          | _ -> (log, None, cmds, env_glob))
      | _ -> (log, None, cmds, env_glob))

  | _ -> (log, Some st, [], env_glob)



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

let reserved =
  [ "Push"
  ; "True"
  ; "False"
  ; "Pop"
  ; "Add"
  ; "Sub"
  ; "Mul"
  ; "Div"
  ; "Equal"
  ; "Lte"
  ; "And"
  ; "Or"
  ; "Not"
  ; "Trace"
  ; "Local"
  ; "Global"
  ; "Lookup"
  ; "Begin"
  ; "If"
  ; "Else"
  ; "Fun"
  ; "End"
  ; "Call"
  ; "Try"
  ; "Switch"
  ; "Case"
  ]

let name : string parser =
  let* c = satisfy is_alpha in
  let* cs = many (satisfy (fun c -> is_alphanum c || c = '_' || c = '\'')) in
  let s = implode (c :: cs) in
  if List.exists (fun x -> x = s) reserved then
    fail
  else
    pure s << ws

let unit_parser () =
  let* _ = keyword "()" in
  pure Unit

let int_parser () =
  (let* n = natural in
   pure (Int n) << ws)
  <|> let* _ = keyword "-" in
  let* n = natural in
  pure (Int (-n)) << ws

let true_parser () =
  let* _ = keyword "True" in
  pure true

let false_parser () =
  let* _ = keyword "False" in
  pure false

let bool_parser () =
  let* b = true_parser () <|> false_parser () in
  pure (Bool b)

let name_parser () =
  let* x = name in 
  pure (Name x) << ws

let const_parser () = int_parser () <|> bool_parser () <|> unit_parser () <|> name_parser()

let rec push_parser () =
  let* _ = keyword "Push" in
  let* cst = const_parser () in
  pure (Push cst)

let rec pop_parser () =
  let* _ = keyword "Pop" in
  let* n = natural in
  pure (Pop n) << ws

and add_parser () =
  let* _ = keyword "Add" in
  let* n = natural in
  pure (Add n) << ws

and sub_parser () =
  let* _ = keyword "Sub" in
  let* n = natural in
  pure (Sub n) << ws

and mul_parser () =
  let* _ = keyword "Mul" in
  let* n = natural in
  pure (Mul n) << ws

and div_parser () =
  let* _ = keyword "Div" in
  let* n = natural in
  pure (Div n) << ws

and trace_parser () =
  let* _ = keyword "Trace" in
  let* n = natural in
  pure (Trace n) << ws

and cmd_parser () =
  push_parser () <|> pop_parser () <|> trace_parser () <|> add_parser ()
  <|> sub_parser () <|> mul_parser () <|> div_parser () <|> and2_parser()
  <|> or2_parser() <|> not2_parser () <|> equal2_parser () <|> lte2_parser ()
  <|> local2_parser () <|> lookup2_parser () <|> global2_parser ()
  <|> be2_parser() <|> end2_parser () <|> if2_parser () <|> else2_parser ()

and cmds_parser () = many (cmd_parser ())

and or2_parser () = 
  let* _ = keyword "Or" in
  pure (Or) << ws 

and and2_parser () = 
  let* _ = keyword "And" in
  pure (And) << ws 

and not2_parser () = 
  let* _ = keyword "Not" in
  pure (Not) << ws 

and equal2_parser () = 
  let* _ = keyword "Equal" in
  pure (Equal) << ws 

and lte2_parser () = 
  let* _ = keyword "Lte" in
  pure (Lte) << ws 

and local2_parser () = 
  let* _ = keyword "Local" in
  pure (Local) << ws 

and lookup2_parser () = 
  let* _ = keyword "Lookup" in
  pure (Lookup) << ws 

and global2_parser () = 
  let* _ = keyword "Global" in
  pure (Global) << ws 

and be2_parser () =
  let* _ = keyword "Begin" in
  pure (Begin) << ws 

and end2_parser () =
  let* _ = keyword "End" in
  pure (End) << ws 

and if2_parser () =
  let* _ = keyword "If" in
  pure (If) << ws 

and else2_parser () =
  let* _ = keyword "Else" in
  pure (Else) << ws 




let parse_cmds s = parse (ws >> cmds_parser ()) s



let interp (src : string) : string list =
  match parse_cmds src with
  | Some (cmds, []) -> (
      match eval [] [] cmds [] [] with
      | log, Some _, _, _ -> log
      | _, None, _, _ -> [ "Error" ])
  | _ -> [ "Error" ]

let main fname =
  let src = readlines fname in
  interp src
