# Homework 4: Reasoning about Correctness.

by << Suihan Dong >>

## Problem 1

Given:
```
let rec prod = function
| [] -> 1
| y::ys -> y * prod ys

```

Show by induction that:
```
prod (l1 @ l2) = prod l1 * prod l2
```

### Principle of Induction
```
For all l, P(l) if P([]) and P(y) and P(yx) -> P (y :: ys)
```

### Base Case:  l = []

Show: prod([] @ l2) = prod [] * prod l2

```
   prod([] @ l2)
= prod l2, by properties of @
= 1 * prod l2, by properties of *
= prod[] * prod l2, by def. of prod
```

### Inductive case:  l = y :: ys

Show: prod((y :: ys) @ l2) = prod l1 * prod l2

Give: prod(ys @ l2) = ys * prod l2

```
   prod((y :: ys) @ l2)
= prod(y :: (ys @ l2)), by properties of :: and @
= y * prod(ys @ l2), by def. of prod
= y * (prod ys * prod l2), by inductive hyp.
= (y * prod ys) * prod l2, (x + sum xs) + sum l2, by associativity of *
= prod(y :: ys) * prod l2, by def. of prod
```

## Problem 2

Given:
```
let rec sum = function
| [] -> 0
| y::ys -> y + sum ys

let rec length = function
| [] -> 0
| y::ys -> 1 + length ys

let rec inc_all = function
| [] -> []
| y::ys -> (y+1) :: inc_all ys

```

Show by induction that:
```
sum (inc_all l) = length l + sum l
```

### Principle of Induction
```
For all l, P(l) if P([]) and P(y) and P(yx) -> P (y :: ys) 
```
 

### Base Case:  l = []

Show: sum (inc_all []) = length [] + sum []

```
   sum (inc_all [])
= sum [], by def. inc_all
= 0 + sum [], by properties of +
= length [] + sum [], by def. of length
```

### Inductive case: l = y :: ys

Show: sum(inc_all y :: ys) = (length y :: ys) + (sum y :: ys)

Give: sum(inc_all ys) = length ys + sum ys

```
   sum(inc_all y :: ys)
= sum((y + 1) :: inc_all ys), by def. inc_all
= (y + 1) + sum(inc_all ys), by def. sum
= (y + 1) + (length ys + sum ys),by inductive hyp.
= (1 + length ys) + (y + sum ys), by associativity of +
= length(y :: ys) + sum (y :: ys), by def. length and def. sum
```

## Problem 3
Given:
```
let rec map f l = match l with
| [] -> []
| y::ys -> f y :: map f ys

let inc x = x + 1

```
Show by induction that:
```
map inc l = inc_all l
```
### Principle of Induction
```
For all l, P(l) if P([]) and P(y) and P(yx) -> P (y :: ys) 
```

### Base Case: l = []

Show: map inc [] = inc_all []

```
   map inc []
= [], by def. of map
= inc_all [], by def. of inc_all
```

### Inductive case l = y :: ys

Show: map inc (y :: ys) = inc_all (y :: ys)

Give: map inc ys = inc_all ys

```
   map inc (y :: ys)
= inc y :: map inc ys, by def. of map
= inc y :: inc_all ys, by inductive hyp.
= (y + 1) :: inc_all ys, by def. of inc
= inc_all (y :: ys), by def. of inc_all
```

## Problem 4
Given:
```
type 'a tree = Empty
| Node of 'a * 'a tree * 'a tree

let rec to_list (t: 'a tree) : 'a list = match t with
| Empty -> []
| Node (v, tl, tr) -> to_list tl @ [v] @ to_list tr

let rec product (t : int tree) : int =
match t with
|Empty -> 1
|Node(v, tl, tr) -> v * product tl * product tr

let rec prod = function
| [] -> 1
| y::ys -> y * prod ys

```
Show by induction that:
```
prod to_list t = product t
```

### Principle of Induction
```
for all t, P(t) if  P(Empty)  and P(tl) and P(tr) -> P(Node(v, tl, tr))
```

### Base Case: t = Empty

Base Case: P(Empty)

Show: prod to_list Empty = product Empty

```
   prod to_list Empty
= prod [], by def. of to_list
= 1, by def. of prod
= product Empty, by def. of product
```


### Inductive case t = Node (v, tl, tr)

Show: prod to_list Node (v, tl, tr) = product Node (v, tl, tr)

Give: prod (to_list tl) = product tl and prod (to_list tr) = product tr

```
   prod (to_list Node (v, tl, tr))
= prod (to_list tl @ [v] @ to_list tr), by def. of to_list
= prod ([v] @ to_list tl @ to_list tr), by properties of @
= prod ((v :: []) @ to_list tl @ to_list tr), by properties of ::
= (v * 1) * prod (to_list tl @ to_list tr), by def. of prod
= v * prod (to_list tl @ to_list tr), by properties of *
= v * prod (to_list tl) * prod (to_list tr), by def. of prod
= v * product tl * product tr, by inductive hyp.
= product Node (v, tl, tr), by def. of product
```

## Problem 5
Given:
```
let rec size (t : 'a tree) : int =
match t with
|Empty -> 0
|Node(v, tl, tr) -> 1 + size tl + size tr

let size_r (t : 'a tree) : int =
reduce t 0 (fun v tl tr -> 1 + tl + tr)

let rec reduce (t: 'a tree) (b: 'b) (f: 'a -> 'b -> 'b -> 'b) : 'b =
match t with
| Empty -> b
| Node (v, tl, tr) -> f v (reduce tl b f) (reduce tr b f)

```
Show by induction that:
```
size t = size_r t
```

### Principle of Induction
```
for all t, P(t) if  P(Empty)  and P(tl) and P(tr) -> P(Node(v, tl, tr))
```

### Base Case: t = Empty
Show: size Empty = size_r Empty
```
   size Empty
= 0, by def. of size
= reduce Empty 0 (fun v tl tr -> 1 + tl + tr), by def. of reduce
= size_r Empty, by def. of size_r
```

### Inductive Case: t = Node (v, tl, tr)

Show: size Node (v, tl, tr) = size_r Node (v, tl, tr)

Give:  size tl = size_r tl and size tr = size_r tr

```
   size Node (v, tl, tr)
= 1 + size tl + size tr, by def. of size
= 1 + size_r tl + size_r tr, by inductive hyp.
= 1 + (reduce tl 0 (fun v tl tr -> 1 + tl + tr)) + (reduce tr 0 (fun v tl tr -> 1 + tl + tr)), by def. of size_r
= (f v tl tr -> 1 + tl + tr) + (reduce tl 0 (fun v tl tr -> 1 + tl + tr)) + (reduce tr 0 (fun v tl tr -> 1 + tl + tr)), by fun apply
= reduce (Node(v, tl, tr)) 0 (fun v tl tr -> 1 + tl + tr), by def. of reduce
= size_r Node(v, tl, tr), by def. of size_r
```
