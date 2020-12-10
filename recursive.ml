let odd a =
  if a mod 2 = 0 then false
  else true

let rec euclid a b =
  if a = b then a
  else if a < b then (euclid a (b-a))
  else (euclid (a-b) b)

let frac_simplify (a, b) =
  let x = euclid a b
  in
  (a/x, b/x)

let rec min_list lst =
  match lst with
  |[] -> 0
  |a ::[]-> a
  |a :: xs-> if a < min_list xs then a else min_list xs

let rec drop to_drop lst =
  match lst with
  |[] -> []
  |x::rest -> if to_drop = 1 then rest
              else if to_drop = 0 then lst
              else drop (to_drop-1) rest
