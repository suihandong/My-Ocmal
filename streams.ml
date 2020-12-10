(* Lazy streams and generators.

   -Eric Van Wyk
 *)


(* Lazy type declaration and supporting function.
 *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values
 *)
type 'a stream = Cons of 'a * 'a stream lazee

let rec take (n:int) (s : 'a stream) : ('a list) =
 match n, s with
 | 0, _ -> []
 | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) ->
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd
     then Cons (hd, rest)
     else demand rest

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))


(* A copy of the generator declarations.
 *)
type 'a generator = unit -> 'a

let random_int (low: int) (high: int) : int generator
  = fun () -> (Random.int ((high+1) - low)) + low

let random_char : char generator
  = fun () -> Char.chr (Random.int (127 - 32) + 32)


let rec generated_values (agen: 'a generator) : 'a stream =
  Cons ( agen (), delay (fun () -> generated_values agen ))

let some_random_ints : int list =
  take 10 (generated_values (random_int 0 100))


(* For part 3 write your implementations for the streams
   - increasing_lenth_lists
   - all_coordinates
   - non_negative_coordinates
   - all_list_length_pairs
  below.
 *)


let increasing_length_lists (gen : 'a generator) : 'a list stream =
  let rec helper lst =
    Cons(lst, delay(fun () -> helper (gen () :: lst)))
  in helper []

let rec all_coordinates  : (int * int) stream =
   let rec helper ((x, y) : (int * int)) (lst : 'a list) : (int * int) stream =
      Cons((x, y), if x = 0 && y = 0 then (delay(fun () -> helper (x+1, y) lst))
                   else if x >= y && x < 1-y then (delay(fun () -> helper (x+1, y) lst))
                   else if x >= 1-y && x > y && x != 0 then (delay(fun () -> helper (x, y+1) lst))
                   else if x <= y && x+y > 0 then (delay(fun () -> helper (x-1, y) lst))
                   else (delay(fun () -> helper (x, y-1) lst))
      )
   in helper (0,0) []


let non_negative_coordinates =
  filter (fun (x, y) -> x >= 0 && y >= 0) all_coordinates

let all_list_length_pairs (gen : 'a generator) : ('a list * 'a list) stream =
  let f (x, y) = (take x (generated_values gen), take y (generated_values gen))
  in
  let s = non_negative_coordinates
  in
  map f s


(* Below, use the functions above to test the prod function and
   property given below:
 *)
let rec prod = function
  | [] -> 1
  | y::ys -> y * prod ys

let prop_prooerty (l1, l2) = prod (l1 @ l2) = prod l1 * prod l2
