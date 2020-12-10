#  Homework 6: Lazy Evaluation

by <<Suihan Dong>>
Internet ID : dongx460

## Problem 2
Given:

```
foldr f [] v = v
foldr f (x::xs) v = f x (foldr f xs v)

foldl f v [] = v
foldl f v (x::xs) = foldl f (f v x) xs

or b1 b2 = if b1 then true else b2

orl l = foldl or false l
orr l = foldr or l false
```

Evaluate:

```
orl (false :: false ::true :: false :: [])
```

### CALL BY VALUE
```
  orl (false :: false ::true :: false :: []), --the initial expression
= foldl or false (false :: false ::true :: false :: [])
= foldl or (or false false) (false ::true :: false :: [])
= foldl or (or (or false false) false) (true :: false :: [])
= foldl or (or (or (or false false) false) true) (false :: [])
= foldl or (or (or (or (or false false) false) true) false) []
= foldl or (or (or (or (or false false) false) true) false) []
= foldl or (or (or (or false false) true) false) []
= foldl or (or (or false true) false) []
= foldl or (or true false) []
= foldl or (or true false) []
= foldl or true []
= true
```

### CALL BY NAME
```
  orl (false :: false ::true :: false :: [])
= foldl or false (false :: false ::true :: false :: [])
= foldl or (or false false) (false ::true :: false :: [])
= foldl or false (false ::true :: false :: [])
= foldl or (or false false) (true :: false :: [])
= foldl or flase (true :: false :: [])
= foldl or (or flase true) (false :: [])
= foldl or true (false :: [])
= foldl or (or true false) ([])
= foldl or true []
= true
```

### CALL BY NEED
```
  orl (false :: false ::true :: false :: [])
= foldl or false (false :: false ::true :: false :: [])
= foldl or false (false :: xs)
    -- where xs = (false ::true :: false :: [])
= foldl or (or fasle false) xs
    -- where x = false and xs = (false ::true :: false :: [])
= foldl or flase (false ::true :: false :: [])
= foldl or (or false false) xs
    -- where xs = (true :: false :: [])
= foldl or flase (true :: false :: [])
= foldl or (or false true) xs
    -- where xs = false :: []
= foldl or true (false :: [])
= foldl or (or true false) []
= foldl or true []
= true
```

Evaluate:

```
orr (false :: false ::true :: false :: [])
```

### CALL BY VALUE
```
  orr (false :: false ::true :: false :: []), --the initial expression
= foldr or (false :: false ::true :: false :: []) false
= or false (foldr or (false ::true :: false :: []) false)
= or false (or false (foldr or (true :: false :: []) false)
= or false (or false (or true (foldr or (false :: []) false)))
= or false (or false (or true (or false (foldr or [] false))))
= or false (or false (or ture (or false flase)))
= or false (or false (or true flase))
= or false (or false true)
= or false (true)
= true
```

### CALL BY NAME
```
  orr (false :: false ::true :: false :: [])
= foldr or (false :: false ::true :: false :: []) false
= or false (foldr or (false ::true :: false :: []) false)
= foldr or (false ::true :: false :: []) false
= or false (foldr or (true :: false :: []) false)
= (foldr or (true :: false :: []) false)
= or true (foldr or (false :: []) flase)
= true
```

### CALL BY NEED
```
  orr (false :: false ::true :: false :: [])
= foldr or (false :: false ::true :: false :: []) false
= foldr or (false :: xs) false
    -- where xs = false ::true :: false :: []
= or false (foldr or xs false)
    -- where xs = false ::true :: false :: []
= foldr or (false ::true :: false :: []) false
= foldr or (false :: xs) false
    -- where xs = true :: false :: []
= or false (foldr or xs false)
     -- where xs = true :: false :: []
= foldr or xs false
    -- where xs = true :: false :: []
= foldr or (true :: false :: []) false
= foldr or (true :: xs) false
    -- where xs = false :: []
= or true (foldr or xs false)
    -- where xs = false :: []
= true
```
