module Hw02 where

import qualified Data.Map as Map
import Data.Map (Map, (!))

import qualified Data.Set as Set
import Data.Set (Set)

sumUp :: [Int] -> Int
sumUp l = foldr (+) 0 l 

evens :: [Int] -> [Int]
evens l = filter even l

incAll :: [Int] -> [Int]
incAll l = map (+1) l

incBy :: Int -> [Int] -> [Int]
incBy n l = map (+n) l

rev :: [Int] -> [Int]
rev l = foldr (\x y -> y ++ [x]) [] l

appendr :: [Int] -> [Int] -> [Int]
appendr l1 l2 = foldr (:) l2 l1

appendl :: [Int] -> [Int] -> [Int]
appendl l1 l2 = foldl f l2 (rev l1)
    where f x y = y:x

map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (x:xs) = (f x) : (map1 f xs)

map2 :: (a -> b) -> [a] -> [b]
map2 f l = foldr g [] l
    where g b a = f(b):a

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 f (x:xs) =
    if f x
    then x:(filter1 f xs)
    else filter1 f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p l = foldr g [] l
    where g b a = if p(b)
                  then b:a
                  else a

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f (x:xs) =
    case f x of
    (Just b) -> b:(mapMaybe f xs)
    Nothing -> mapMaybe f xs
         
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

pairUp :: [a] -> [b] -> [(a,b)]
pairUp _ [] = []
pairUp [] _ = []
pairUp (a:as) (b:bs) = (a,b):(pairUp as bs)

splitUp :: [(a,b)] -> ([a],[b])
splitUp l = (map fst l, map snd l)

sumAndLength :: [Int] -> (Int,Int)
sumAndLength l = foldl f (0,0) l
    where f (l,s) a = (l+1, s+a) 

data EitherList a b = 
    Nil
   | ConsLeft a (EitherList a b)
   | ConsRight b (EitherList a b)
   deriving (Eq, Show)

toEither :: [Either a b] -> EitherList a b
toEither [] = Nil
toEither ((Left a):xs) = ConsLeft a (toEither xs)
toEither ((Right b):xs) = ConsRight b (toEither xs)

fromEither :: EitherList a b -> [Either a b]
fromEither Nil = []
fromEither (ConsLeft a l) = (Left a):(fromEither l)
fromEither (ConsRight b l) = (Right b):(fromEither l)

mapLeft :: (a -> c) -> EitherList a b -> EitherList c b
mapLeft _ Nil = Nil
mapLeft f (ConsLeft a l) = ConsLeft (f a) (mapLeft f l)
mapLeft f (ConsRight b l) = ConsRight b (mapLeft f l)

mapRight :: (b -> c) -> EitherList a b -> EitherList a c
mapRight _ Nil = Nil
mapRight f (ConsLeft a l) = ConsLeft a (mapRight f l)
mapRight f (ConsRight b l) = ConsRight (f b) (mapRight f l)

foldrEither :: (a -> c -> c) -> (b -> c -> c) -> c -> EitherList a b -> c
foldrEither _ _ c Nil = c
foldrEither f g c (ConsLeft a l) = foldrEither f g (f a c) l
foldrEither f g c (ConsRight b l) = foldrEither f g (g b c) l

foldlEither :: (c -> a -> c) -> (c -> b -> c) -> c -> EitherList a b -> c
foldlEither _ _ c Nil = c
foldlEither f g c (ConsLeft a l) = foldlEither f g (f c a) l
foldlEither f g c (ConsRight b l) = foldlEither f g (g c b) l

newtype Node = Node { nodeName :: String } deriving (Eq,Ord,Show)

a = Node "a"
b = Node "b"
c = Node "c"
d = Node "d"
e = Node "e"

type Graph = Map Node (Set Node)

g1 = Map.fromList [(a, Set.fromList []),
                   (b, Set.fromList [d]),        
                   (c, Set.fromList []),
                   (d, Set.fromList [])]

aEdges = g1 ! a

nSet :: (Node, [Node]) -> Graph -> Bool
nSet (n, []) g = True
nSet (n, (x:xs)) g =
    if Map.lookup x g == Nothing
    then False
    else Set.member n (Map.findWithDefault (Set.fromList []) x g) && (nSet (n, xs) g)

elmBi :: (Node, (Set Node)) -> Graph -> Bool
elmBi (n, s) g = nSet (n, (Set.toList s)) g

isBi :: [(Node, (Set Node))] -> Graph -> Bool
isBi [] _ = True
isBi (x:xs) g = elmBi x g && isBi xs g

isBidi :: Graph -> Bool
isBidi g = isBi (Map.toList g) g

insertIn :: (Node, [Node]) -> Graph -> Graph
insertIn (n, []) g = g
insertIn (n, (x:xs)) g =
    if Set.member n (Map.findWithDefault (Set.fromList []) x g)
    then insertIn (n, xs) g
    else insertIn (n, xs) (Map.insert x (Set.union (Set.fromList [n]) (Map.findWithDefault (Set.fromList []) x g)) g)

putIn :: (Node, (Set Node)) -> Graph -> Graph
putIn (n, s) g = insertIn (n, (Set.toList s)) g

conG :: [(Node, (Set Node))] -> Graph -> Graph
conG [] g = g
conG (x:xs) g = conG xs (putIn x g)
-- conG l g = map (putIn g) l 

bidify :: Graph -> Graph
bidify g = conG (Map.toList g) g

data ArithExp = 
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp
  deriving (Eq,Show)

eval :: ArithExp -> Int
eval (Num i) = i
eval (Plus a b) = (eval a) + (eval b)
eval (Times a b) = (eval a) * (eval b)
eval (Neg a) = - (eval a)

data ArithExp' = 
    Num' Int
  | Plus' ArithExp' ArithExp'
  | Sub' ArithExp' ArithExp'
  | Times' ArithExp' ArithExp'
  | Neg' ArithExp'
  deriving (Eq,Show)

eval' :: ArithExp' -> Int
eval' = eval . translate

translate :: ArithExp' -> ArithExp
translate (Num' i) = (Num i)
translate (Plus' a b) = (Plus (translate a) (translate b))
translate (Sub' a b) = (Plus (translate a) (Neg (translate b)))
translate (Times' a b) = (Times (translate a) (translate b))
translate (Neg' a) = (Neg (translate a))

-- c)
-- Denotation Semantics for Subtraction:
-- [[Sub' a b]] = [[a]] - [[b]]



-- Rewrite System:
-- SubLeft
--          e1 -> e1'
--  _________________________
--  Sub' e1 e2 -> Sub' e1' e2

-- SubRight
--          e2 -> e2'
--  _________________________
--  Sub' e1 e2 -> Sub' e1 e2'