module Hw01 where

sumUp :: [Int] -> Int
sumUp []     = 0
sumUp (x:xs) = x + (sumUp xs)

evens :: [Int] -> [Int]
evens []     = []
evens (x:xs) =
      if even x
      then x:evens xs
      else evens xs

incAll :: [Int] -> [Int]
incAll [] = []
incAll (x:xs) = (x+1):(incAll xs)

incBy :: Int -> [Int] -> [Int]
incBy _ [] = []
incBy x (y:ys) = (y+x):(incBy x ys)

append :: [Int] -> [Int] -> [Int]
append l [] = l
append l (x:xs) = append (l ++ [x]) xs

data IntTree = Empty | Node IntTree Int IntTree deriving (Eq,Show)

isLeaf :: IntTree -> Bool
isLeaf Empty = False
isLeaf (Node l x r) =
       if l == Empty && r == Empty
       then True
       else False

sumTree :: IntTree -> Int
sumTree Empty = 0
sumTree (Node l x r) = x + sumTree(l) + sumTree(r)

fringe :: IntTree -> [Int]
fringe Empty = []
fringe (Node l x r) =
       if l == Empty && r == Empty
       then [x]
       else append (fringe l) (fringe r)

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs) =
       if n > x
       then [x] ++ (insert n xs)
       else n:x:xs

helper :: [Int] -> Int -> [Int] -> [Int]
helper l n [] = insert n l
helper l n (x:xs) = helper (insert n l) x xs

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = helper [] x xs

maybeBounded :: Maybe Int -> Maybe Int -> Int -> Bool
maybeBounded Nothing Nothing x = True
maybeBounded Nothing (Just upper) x = x < upper
maybeBounded (Just lower) Nothing x = lower < x
maybeBounded (Just lower) (Just upper) x = lower < x && x < upper

binaryAux :: Maybe Int -> Maybe Int -> IntTree -> Bool
binaryAux _ _ Empty = True;
binaryAux b t (Node l x r) =
          if maybeBounded b t x
          then (binaryAux b (Just x) l) && (binaryAux (Just x) t r)
          else False

isBST :: IntTree -> Bool
isBST Empty = True
isBST (Node l x r) =
      binaryAux Nothing Nothing (Node l x r)

insertBST :: Int -> IntTree -> IntTree
insertBST n Empty = (Node Empty n Empty)
insertBST n (Node l x r) =
          if n == x
          then error "Value already in tree!"
          else  if n > x
                then (Node l x (insertBST n r))
                else (Node (insertBST n l) x r)

     
height :: IntTree -> Int
height Empty = 0
height (Node l _ r) = 1 + max (height l) (height r)

removeNode :: IntTree -> IntTree
removeNode Empty = Empty
removeNode (Node l x r) =
           if l == Empty && r == Empty
           then Empty
           else if height l > height r
                then l
                else r

deleteBST :: Int -> IntTree -> IntTree
deleteBST _ Empty = Empty
deleteBST n (Node l x r) =
          if n == x
          then removeNode (Node l x r)
          else (Node (deleteBST n l) x (deleteBST n r))
