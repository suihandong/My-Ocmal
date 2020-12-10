let read_file (file_name: string) : char list =
  let ic = open_in file_name
  in
  let rec read_chars ic =
    (* `input_char` raises an exception when it attempts to read
       past the end of the file.  This is caught to terminate
       the `read_chars` function. *)
    try
      let next_char = input_char ic
      in next_char :: read_chars ic
    with
      _ -> []
  in read_chars ic

let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i =
    if i = l then [] else s.[i] :: f (i+1)
  in f 0


(*helper function: split
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

(*step 2*)
let transform1 (step1 : char list) : char list list =
  let sep = [' '; '\t'; '\n'; ';'; ':'; '.'; '?'; '!'; '/']
  in
  split sep step1


(*step 3*)
let transform2 (step2 : char list list) : string list =
  List.fold_right(fun x accum -> implode(x) :: accum) step2 []

(*step 4*)
let transform3 (step3 : string list) : string list =
  let sep = [""]
  in
  List.filter(fun a -> List.mem a sep = false) step3

(*step 5*)
let transform4 (step4 : string list) : (string * int) list =
  List.fold_right(fun x accum -> (x,1) :: accum) step4 []

(*step 6*)
let transform5 (step5 : (string * int) list) : (string * int list) list =
  let slst = List.sort compare step5
  in
  let accum = []
  in
  let f accum (x,y) =
    match accum with
    |[] -> (x, [y]) :: []
    |(k, v) :: tl -> if x = k then (k, y :: v) :: tl
                     else (x, [y]) :: (k, v) :: tl
  in
  List.fold_left (f) accum slst

(*step 7*)
let transform6 (step6 : (string * int list) list) : (string * int) list =
  List.fold_right(fun (x, y) accum -> (x, List.length(y)) :: accum) step6 []



let word_count (fn: string) : (string * int) list =
  let step1: char list = read_file fn in
  let step2: char list list = transform1 step1 in
  let step3: string list = transform2 step2 in
  let step4: string list = transform3 step3 in
  let step5: (string * int) list = transform4 step4 in
  let step6: (string * int list) list = transform5 step5 in
  let step7: (string * int) list = transform6 step6 in
  step7
