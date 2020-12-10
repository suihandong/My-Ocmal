let all_odds lst = List.filter(fun a -> a mod 2 != 0) lst

let decrement_all lst = List.map(fun a -> a-1) lst

let min_fold lst = List.fold_left(fun acc b -> if b < acc then b else acc) max_int lst

let sum_prod lst = List.fold_left(fun (x,y) b -> (x+b, y*b)) (0,1) lst

let partition_left f lst = List.fold_left(fun (a,b) x -> if f x then (a@[x],b) else (a, b@[x])) ([],[]) lst

let partition_right f lst = List.fold_right(fun x (a,b) -> if f x then ([x]@a, b) else (a, [x]@b)) lst ([],[]) 
