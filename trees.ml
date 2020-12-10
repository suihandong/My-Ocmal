(* A tree type declaration. *)
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* A sample tree containing ints *)
let int_tree : int tree =
  Node (3,
        Node (1,
              Node (4, Empty, Empty), Empty),
        Node (2, Empty, Empty)
       )

(* A sample tree containing strings *)
let str_tree : string tree =
  Node ("love ",
        Node ("really ",
              Node ("I ", Empty, Empty), Empty),
        Node ("OCaml!", Empty, Empty)
        )

(* A sample tree containing ints list *)
let ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6],
              Empty,
              Node ([], Empty, Empty)
            ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )

(* A sample tree containing strings list*)
let strs_tree: string list tree =
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "],
              Node (["I "; "really "], Empty, Empty), Empty),
        Node (["your "; "favorite "; "too!"], Empty, Empty)
       )


(*functions for part 1*)
let rec size (t : 'a tree) : int =
  match t with
  |Empty -> 0
  |Node(v, tl, tr) -> 1 + size tl + size tr

let rec sum (t : int tree) : int =
  match t with
  |Empty -> 0
  |Node(v, tl, tr) -> v + sum tl + sum tr

let rec product (t : int tree) : int =
  match t with
  |Empty -> 1
  |Node(v, tl, tr) -> v * product tl * product tr

let rec charcount (t : string tree) : int =
  match t with
  |Empty -> 0
  |Node(s, tl, tr) -> String.length(s) + charcount tl + charcount tr

let rec concat (t : string tree) : string =
  match t with
  |Empty -> ""
  |Node(s, tl, tr) -> concat tl ^ s ^ concat tr


(*functions for part 2*)
let rec list_tree_size (t : 'a list tree) : int =
  match t with
  |Empty -> 0
  |Node(lst, tl, tr) -> List.length(lst) + list_tree_size tl + list_tree_size tr

let rec list_tree_sum (t : int list tree) : int =
  match t with
  |Empty -> 0
  |Node(lst, tl, tr) ->
    let rec f lst =
      match lst with
      |[] -> 0
      |hd :: tl -> hd + f tl
    in
    f lst + list_tree_sum tl + list_tree_sum tr

let rec list_tree_product (t : int list tree) : int =
  match t with
  |Empty -> 1
  |Node(lst, tl, tr) ->
    let rec f lst =
      match lst with
      |[] -> 1
      |hd :: tl -> hd * (f tl)
    in
    (f lst) * (list_tree_product tl) * (list_tree_product tr)

let rec list_tree_charcount (t : string list tree) : int =
  match t with
  |Empty -> 0
  |Node(cl, tl, tr) ->
    let rec f cl =
      match cl with
      |[] -> 0
      |hd :: tl -> String.length(hd) + f tl
    in
    f cl + list_tree_charcount tl + list_tree_charcount tr

let rec list_tree_concat (t: string list tree) : string =
  match t with
  |Empty -> ""
  |Node(cl, tl, tr) ->
    let rec f cl =
      match cl with
      |[] -> ""
      |hd :: tl -> hd ^ f tl
    in
    list_tree_concat  tl ^ f cl ^ list_tree_concat tr




(*test for part 1*)
let () =
  print_string "Testing part 1 ... " ;
  try
    assert (size str_tree = 4);
    assert (sum int_tree = 10);
    assert (product int_tree = 24);
    assert (charcount str_tree = 20);
    assert (concat str_tree = "I really love OCaml!");
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) ->
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^
              ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg

(*test for part 2*)
let () =
  print_string "Testing part 2 ... " ;
  try
    assert (list_tree_size strs_tree = 11);
    assert (list_tree_sum ints_tree = 45);
    assert (list_tree_product ints_tree = 311040);
    assert (list_tree_charcount strs_tree = 54);
    assert (list_tree_concat strs_tree =
              "I really do love Ocaml!  It must be your favorite too!");
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) ->
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^
              ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
