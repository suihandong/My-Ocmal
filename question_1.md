#  Homework 6: Lazy Evaluation

by <<Suihan Dong>>
Internet ID : dongx460

## Problem 1
Given:

```
prod [] = 1
prod (x::xs) = x * prod xs

take 0 lst = [ ]
take n [ ] = [ ]
take n (x::xs) = x::take (n-1) xs

odds_from 0 v = [ ]
odds_from n v = (v+v+1) :: odds_from (n-1) (v+1)
```

Evaluate:

```
prod (take 3 (odds_from 5 0))
```

### CALL BY VALUE
```
  prod(take 3 (odds_from 5 0)) --the initial expression
= prod(take 3 ((0+0+1) :: odds_from (5-1) (0+1)))
= prod(take 3 (1 :: odds_from 4 (0+1)))
= prod(take 3 (1 :: odds_from 4 1))
= prod(take 3 (1 :: ((1+1+1) :: odds_from (4-1) (1+1))))
= prod(take 3 (1 :: (3 :: odds_from 3 (1+1))))
= prod(take 3 (1 :: (3 :: odds_from 3 2)))
= prod(take 3 (1 :: (3 :: ((2+2+1) :: odds_from (3-1) (2+1)))))
= prod(take 3 (1 :: (3 :: (5 :: odds_from 2 (2+1)))))
= prod(take 3 (1 :: (3 :: (5 :: odds_from 2 3))))
= prod(take 3 (1 :: (3 :: (5 :: ((3+3+1) :: odds_from (2-1) (3+1))))))
= prod(take 3 (1 :: (3 :: (5 :: (7 :: odds_from 1 (3+1))))))
= prod(take 3 (1 :: (3 :: (5 :: (7 :: odds_from 1 4)))))
= prod(take 3 (1 :: (3 :: (5 :: (7 :: ((4+4+1) :: odds_from (1-1) (4+1)))))))
= prod(take 3 (1 :: (3 :: (5 :: (7 :: (9 :: odds_from 0 (4+1)))))))
= prod(take 3 (1 :: (3 :: (5 :: (7 :: (9 :: odds_from 0 5))))))
= prod(take 3 (1 :: (3 :: (5 :: (7 :: (9 :: []))))))
= prod(1 :: (take (3-1) (3 :: (5 :: (7 :: (9 :: []))))))
= prod(1 :: (take 2 (3 :: (5 :: (7 :: (9 :: []))))))
= prod(1 :: (3 :: take (2-1) (5 :: (7 :: (9 :: [])))))
= prod(1 :: (3 :: take 1 (5 :: (7 :: (9 :: [])))))
= prod(1 :: (3 :: (5 :: take (1-1) (7 :: (9 :: [])))))
= prod(1 :: (3 :: (5 :: take 0 (7 :: (9 :: [])))))
= prod(1 :: (3 :: (5 :: [])))
= 1 * (prod (3 :: (5 :: [])))
= 1 * (3 * (prod (5 :: [])))
= 1 * (3 * (5 * prod []))
= 1 * (3 * (5 * 1))
= 1 * (3 * 5)
= 1 * 15
= 15
```

### CALL BY NAME
```
  prod (take 3 (odds_from 5 0))
    -- we must expanded odds_from to determine if it matches the 1st pattern of take or not, yielding
= prod (take 3 ((0+0+1) :: odds_from (5-1) (0+1)))
    -- a rule for take now matches, yielding
= prod ((0+0+1) :: (take (3-1) (odds_from (5-1) (0+1))))
    -- a rule for prod now matches, yielding
= (0+0+1) * prod (take (3-1) (odds_from (5-1) (0+1)))
    --again, odds_from must be expanded to see if the 1st take clause matches, so evaluate the 1st argument of odds_from
= (0+0+1) * prod (take (3-1) (odds_from 4 (0+1)))
    -- again, take must be expanded to see if the 1st prod clause matches, so evaluate the 1st argument of take
= (0+0+1) * prod (take 2 (odds_from 4 (0+1)))
= (0+0+1) * prod (take 2 ((0+1)+(0+1)+1) :: (odds_from (4-1) ((0+1)+1)))
= (0+0+1) * prod (((0+1)+(0+1)+1) :: (take ((2-1) (odds_from (4-1) ((0+1)+1)))))
= (0+0+1) * ((0+1)+(0+1)+1) * prod (take ((2-1) (odds_from (4-1) ((0+1)+1))))
    -- now, evaluate the first argument of odds_from to check if take work or not
= (0+0+1) * ((0+1)+(0+1)+1) * prod (take ((2-1) (odds_from 3 ((0+1)+1))))
    -- now evaluate the 1st argument of take to check if prod work or not
= (0+0+1) * ((0+1)+(0+1)+1) * prod (take (1 (odds_from 3 ((0+1)+1))))
= (0+0+1) * ((0+1)+(0+1)+1) * prod (take (1 ((((0+1)+1)+((0+1)+1)+1) :: (odds_from (3-1) (((0+1)+1)+1)))))
= (0+0+1) * ((0+1)+(0+1)+1) * prod ((((0+1)+1)+((0+1)+1)+1) :: (take (1-1) (odds_from (3-1) (((0+1)+1)+1))))
= (0+0+1) * ((0+1)+(0+1)+1) * (((0+1)+1)+((0+1)+1)+1) * prod (take (1-1) (odds_from (3-1) (((0+1)+1)+1)))
    -- now, evaluate the first argument of odds_from to check if take work or not 
= (0+0+1) * ((0+1)+(0+1)+1) * (((0+1)+1)+((0+1)+1)+1) * prod (take (1-1) (odds_from 2 (((0+1)+1)+1)))
    -- now, evaluate the first argument of take to check if take work or not 
= (0+0+1) * ((0+1)+(0+1)+1) * (((0+1)+1)+((0+1)+1)+1) * prod (take 0 (odds_from 2 (((0+1)+1)+1)))
= (0+0+1) * ((0+1)+(0+1)+1) * (((0+1)+1)+((0+1)+1)+1) * prod ([])
= 1 * 3 * 5
= 15

```

### CALL BY NEED
```
  prod (take 3 (odds_from 5 0))
= prod (take 3 ((0+0+1) :: odds_from (5-1) (0+1)))
    --since the argument for v is a value, mo where clause is used
= prod ((0+0+1) :: take (3-1) (odds_from (5-1) (0+1)))
= (0+0+1) * prod (take (3-1) (odds_from (5-1) (0+1)))
= (0+0+1) * prod (take (3-1) (odds_from 4 (0+1)))
= (0+0+1) * prod (take (3-1) ((v+v+1) :: (odds_from (4-1) (v+1))))
   where v = 0+1
    --since the argument for v was an expression and not a value we created a where clause
= (0+0+1) * prod (take 2 ((v+v+1):: (odds_from (4-1) (v+1))))
  where v = 0+1
= (0+0+1) * prod ((v+v+1) :: take (2-1) (odds_from (4-1) (v+1)))
  where v = 0+1
= (0+0+1) * (v+v+1) * prod (take 1 (odds_from (4-1) (v+1)))
  where v = 0+1
= (0+0+1) * ((1+1+1) * prod (take 1 (odds_from 3 (1+1)))
    --since v is now a value we removed the where clause and inlined the vlaue
= (0+0+1) * (1+1+1) * prod (take 1 ((((0+1)+(0+1)+1)+((0+1)+(0+1)+1)+1) :: odds_from (3-1) (1+1+1)))
= (0+0+1) * (1+1+1) * prod ((v+v+1) :: take (1-1) odds_from (3-1) (v+1))
  where v = (1+1)
  --since the argument for v was an expression and not a value we created a where clause
= (0+0+1) * (1+1+1) * (v+v+1) * prod (take (1-1) odds_from (3-1) (v+1))
  where v = (1+1)
= (0+0+1) * (1+1+1) * (v+v+1) * prod (take 0 odds_from (3-1) (v+1))
  where v = (1+1)
= (0+0+1) * (1+1+1) * ((1+1+1))+(1+1+1)+1) * prod (take 0 odds_from 2 ((1+1+1)+1))
     --since v is now a value we removed the where clause and inlined the vlaue
= (0+0+1) * (1+1+1) * ((1+1+1))+(1+1+1)+1) * prod (take 0 ((v+v+1) :: odds_from (2-1) (v+1)))
  where v = (1+1)+1
  --since the argument for v was an expression and not a value we created a where clause
= (0+0+1) * (1+1+1) * ((1+1))+(1+1)+1) * prod ([])
= (0+0+1) * (1+1+1) * ((1+1))+(1+1)+1) * 1
= 1 * 3 * 5
= 15
```

    
    

