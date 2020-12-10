type 'a btree = Nil
              | Leaf of 'a
              | Fork of 'a btree * 'a * 'a btree

(*functions for part 4*)
let rec insert_by (f : 'a -> 'a -> int) (x : 'a) (t: 'a btree) : 'a btree =
  match t with
  |Nil -> Leaf x
  |Leaf a -> if f x a < 0 then Fork(Leaf x, a, Nil)
             else if f x a > 0 then Fork(Nil, a, Leaf x)
             else Leaf a
  |Fork(tl, r, tr) -> if f x r < 0 then Fork(insert_by compare x tl, r, tr)
                      else if f x r > 0 then Fork(tl, r, insert_by compare x tr)
                      else Fork(tl, r, tr)


let from_list (f : 'a -> 'a -> int) (lst : 'a list) : 'a btree =
  List.fold_left(fun a x -> insert_by compare x a) Nil lst

let rec reduce t b f fx =
  match t with
  |Nil -> b
  |Leaf a -> fx a
  |Fork(tl, r, tr) -> f (reduce tl b f fx) r (reduce tr b f fx)

let to_list (t : 'a btree) : 'a list =
  let fx r = [r]
  in
  reduce t [] (fun tl r tr -> tl @ [r] @ tr) fx

(*Lab_07*)
(* 1. I choose the List.fold_left
   2. List.fold_left could put all elements in the accum from left to the right,
      which is easy to remove the elements that occurs after which are repeat.
      Then I can reverse the whole list to get the list ordered
   3. List.fold_right is not less appropriate because it start with the last
      element in the list, so it will put the element occurs later into the accum
      first, then remove the first occurs of that element. The result list will
      in a wrong order.
*)
let rem_dups (lst : 'a list) : 'a list =
  let accum = []
  in
  let f accum x =
    match accum with
    |[] -> x :: []
    |hd :: tl -> if List.mem x accum = true then accum
                 else x :: accum
  in
  match List.fold_left f accum lst with
  |[] -> []
  |x :: xs -> List.rev(x :: xs)


let check t =
  match t with
  |Nil -> true
  |Leaf a -> true
  |Fork(tl, r, tr) ->
    let lst = to_list t
    in
    if List.length lst = 1 then false
    else if compare (List.sort compare lst) (rem_dups lst) = 0 then true
    else false

(*test for insert functions*)
let () =
  print_string "Testing part 4 ... " ;
  try
    assert (insert_by compare 4 Nil = Leaf 4);
    assert (insert_by compare 2 (insert_by compare 4 Nil) =
              Fork (Leaf 2, 4, Nil));
    assert (insert_by compare 4 (insert_by compare 2 Nil) =
              Fork (Nil, 2, Leaf 4));
    assert (insert_by compare 4 (insert_by compare 4 Nil) =
              insert_by compare 4 Nil);
    assert (from_list compare [4;2;5;3;6;7;8] =
              Fork (Fork (Nil, 2, Leaf 3), 4,
                    Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))
                   )
           );
    assert (List.sort compare [4;2;5;3;6;7;8] =
              to_list (from_list compare [4;2;5;3;6;7;8]));
    (* Add more asserts here as you need them *)
    assert (rem_dups [1;3;3;5;1;7;2;5;7;2] = [1; 3; 5; 7; 2]);
    assert (check Nil);
    assert (check (Leaf 'c'));
    assert (check (from_list compare [4;2;5;3;6;7;8]));
    assert (not (check (Fork (Nil, 4, Nil))));
    assert (not (check (Fork (Leaf 3, 3, Nil))));
    assert (check (Fork (Fork (Nil, 2, Leaf 3), 4,
          Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))
         )));
    assert (not (check(Fork (Leaf 5, 4, Nil))));
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) ->
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
