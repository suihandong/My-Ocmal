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

let strs_tree: string list tree =
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "],
              Node (["I "; "really "], Empty, Empty), Empty),
        Node (["your "; "favorite "; "too!"], Empty, Empty)
       )

(*function from the resource code*)
let rec reduce (t: 'a tree) (b: 'b) (f: 'a -> 'b -> 'b -> 'b) : 'b =
  match t with
  | Empty -> b
  | Node (v, tl, tr) -> f v (reduce tl b f) (reduce tr b f)

(*new functions for part 1*)
let size (t : 'a tree) : int =
  reduce t 0 (fun v tl tr -> 1 + tl + tr)

let sum (t : int tree) : int =
  reduce t 0 (fun v tl tr -> v + tl + tr)

let product (t : int tree) : int =
  reduce t 1 (fun v tl tr -> v * tl * tr)

let charcount (t : string tree) : int =
  reduce t 0 (fun v tl tr -> String.length(v) + tl + tr)

let concat (t : string tree) : string =
  reduce t "" (fun v tl tr -> tl ^ v ^ tr)

(*new functions for part 2*)
let list_tree_size (t : 'a list tree) : int =
  reduce t 0 (fun v tl tr -> List.length(v) + tl + tr)

let list_tree_sum (t : int list tree) : int =
  let f lst = List.fold_left(fun a b -> a + b) 0 lst
  in
  reduce t 0 (fun v tl tr -> f v + tl + tr)

let list_tree_product (t : int list tree) : int =
  let f lst = List.fold_left(fun a b -> a * b) 1 lst
  in
  reduce t 1 (fun v tl tr -> f v * tl * tr)

let list_tree_charcount (t : string list tree) : int =
  let f lst = List.fold_left(fun a b -> a + String.length(b)) 0 lst
  in
  reduce t 0 (fun v tl tr -> f v + tl + tr)

let list_tree_concat (t : string list tree) : string =
  let f lst = List.fold_left(fun a b -> a ^ b) "" lst
  in
  reduce t "" (fun v tl tr -> tl ^ f v ^ tr)

(*test for part 3*)
let () =
  print_string "Testing part 3 ... " ;
  try
    assert (size str_tree = 4);
    assert (sum int_tree = 10);
    assert (product int_tree = 24);
    assert (charcount str_tree = 20);
    assert (concat str_tree = "I really love OCaml!");
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
