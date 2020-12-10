let read_file (file_name: string) : char list =
  let ic = open_in file_name
  in
  let rec read_chars ic =
    try
      let next_char = input_char ic
      in next_char :: read_chars ic
    with
      _ -> []
  in read_chars ic

let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

(*helper function: split, transform1, transform2
  the format is get from group_by_3 in the resource code
  the List.mem function is get from TA Tasha Bornemann*)
let split (sep : char list) (lst : char list) : char list list =
  let sep = [' '; '\t'; '\n'; ';'; ':'; '.'; '?'; '!'; '/']
  in
  let accum = ([],[])
  in
  let f (cl, el) lst =
    if List.mem lst sep = true then ([], List.rev cl :: el)
    else (lst :: cl, el)
  in
  match List.fold_left f accum lst with
  |(cl, el) -> List.rev (cl :: el)

let transform1 (lst : char list list) : string list =
  List.fold_right(fun x accum -> implode(x) :: accum) lst []

let transform2 (lst : string list) : string list =
  let sep = [""]
  in
  List.filter(fun a -> List.mem a sep = false) lst

let read_into_list (str : string) : string list =
  let sep = [' '; '\t'; '\n'; ';'; ':'; '.'; '?'; '!'; '/']
  in
  let step1 = read_file str
  in
  let step2 = split sep step1
  in
  let step3 = transform1 step2
  in
  let step4 = transform2 step3
  in
  step4

let wl8 lst = List.filter(fun x -> String.length x = 8) lst

let wl6 lst = List.filter(fun x -> String.length x = 6) lst


(*answers : string -> string list*)
let answers (str : string) : string list =
  let lst = read_into_list str
  in
  let w8 = wl8 lst
  in
  let w6 = wl6 lst
  in
  let accum = []
  in
  let f x accum =
    let mystr = (String.sub x 0 3) ^ (String.sub x 5 3)
    in
    if List.mem mystr w6 = true then x :: accum
    else accum
  in
  List.fold_right f w8 accum

(*pretty_answers : string list -> (string * string) list*)
let pretty_answers (lst : string list) : (string * string) list =
  let accum = []
  in
  let f x accum =
    let str = (String.sub x 0 3) ^ (String.sub x 5 3)
    in
    (str, x) :: accum
  in
  List.fold_right f lst accum

let d1 = "../../public-class-repo/Homeworks/Files/words-small.txt"
let d2 = "../../public-class-repo/Homeworks/Files/words-google-10000.txt"
