type formula = And  of formula * formula
             | Or of formula * formula
             | Implies of formula * formula
             | Not of formula
             | Prop of string
             | Bool of bool

type subst = (string * bool) list

let show_list show l =
  let rec sl l =
    match l with
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

let show_string_bool_pair (s,b) =
  "(\"" ^ s ^ "\"," ^ (if b then "true" else "false") ^ ")"

let show_subst = show_list show_string_bool_pair

let is_elem v l =
  List.fold_right (fun x in_rest -> if x = v then true else in_rest) l false

let rec explode = function
  | "" -> []
  | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

let dedup lst =
  let f elem to_keep =
    if is_elem elem to_keep then to_keep else elem::to_keep
  in List.fold_right f lst []

let rec lookup (f : string) (s : subst) : bool =
  match s with
  | [] -> raise (Failure ("Name \"" ^ f ^ "\" not in scope"))
  | (f',v)::_ when f' = f -> v
  | _::rest -> lookup f rest

let rec eval (f : formula) (s : subst) : bool =
  match f with
  | And (f1, f2) -> (eval f1 s) && (eval f2 s)
  | Or (f1, f2) -> (eval f1 s) || (eval f2 s)
  | Implies (f1, f2) -> not (eval f1 s) || (eval f2 s)
  | Not f' -> not (eval f' s)
  | Prop str -> lookup str s
  | Bool b -> b

let freevars (f : formula) : string list =
  let rec helper (f : formula) : string list =
    match f with
    | And (f1, f2) -> helper f1 @ helper f2
    | Or (f1, f2) -> helper f1 @ helper f2
    | Implies (f1, f2) -> helper f1 @ helper f2
    | Not f' -> helper f'
    | Prop str -> [str]
    | Bool b -> [string_of_bool b]
  in
  let lst = helper f
  in
  List.fold_right(fun x a -> if List.mem x a = false then x :: a else a) lst []

exception KeepLooking

let is_tautology_v1 (f : formula) (fs : subst -> subst option) : subst option =
  let rec helper_v1 freevar_lst eval_lst =
    if freevar_lst = [] && not (eval f eval_lst)
    then fs eval_lst
    else
      (match freevar_lst with
        | [] -> if eval f eval_lst then raise KeepLooking
              else fs eval_lst
        | hd :: tl ->
          (try helper_v1 tl ((hd, true) :: eval_lst) with
            | KeepLooking -> helper_v1 tl ((hd, false) :: eval_lst)
          )
      )
  in
  try (helper_v1 (freevars f) []) with
    | KeepLooking -> None

let is_tautology_v1_first f = is_tautology_v1 f (fun s -> Some s)

let is_tautology_v1_print_all f =
  is_tautology_v1
    f
    (fun s -> print_endline (show_subst s);
	      raise KeepLooking)

exception FoundCounterExample of subst

let is_tautology_v2 (f : formula) ( fs : subst -> unit) : subst option =
  let rec helper_v2 freevar_lst eval_lst =
    if freevar_lst = [] && not (eval f eval_lst)
    then fs eval_lst
    else
      (match freevar_lst with
        | [] -> if eval f eval_lst then () else fs eval_lst
        | hd :: tl -> helper_v2 tl ((hd, true) :: eval_lst);
                      helper_v2 tl ((hd, false) :: eval_lst)
      )
  in
  try (helper_v2 (freevars f) []); None with
    | FoundCounterExample result -> Some result
    | Failure msg -> print_endline msg ; None

let is_tautology_v2_first f = is_tautology_v2 f (fun s -> raise (FoundCounterExample s))

let is_tautology_v2_print_all f =
  is_tautology_v2
    f
    (fun s -> print_endline (show_subst s))

let test1 = is_tautology_v1_print_all (Or (Prop "P", Not (Prop "P")))
let test2 = is_tautology_v1_print_all (Or (Prop "P", Prop "Q"))
let test3 = is_tautology_v1_print_all (And (Prop "P", Prop "Q"))
let test4 = is_tautology_v2_print_all (And (Prop "P", Prop "Q"))
