module Hw07 where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map, (!))

--1.a)

--data Answer = Yes | No | Maybe

--Answer = Yes + No + Maybe
--Answer = () + () + ()
--X = mu alpha. () + () + ()
--() + () + () 

--data MaybeInt = Nothing | Maybe Int

--MaybeInt = Nothing + Int
--MaybeInt = () + Int
--X = mu alpha. () + Int
--() + Int

--data Rope = Leaf String | Node Rope Int Rope

--Rope = Leaf String + Node Rope Int Rope
--Rope = String + (Rope,Int,Rope)
--X = mu alpha. String + (alpha,Int,alpha)
--mu alpha. String + (alpha,Int,alpha)

--b)

--() + ()

--data Boolean = True | False

--mu alpha. Int + (alpha,alpha)

--data NoValTree = Leaf Int |  Rec BType BType

--mu alpha. () + (Int,mu beta. () + (alpha,beta))

--data Rose = () | (Int,Branch)
--data Branch = () | (Rose, Branch)

--c)

-- Things of the type Mu alpha. alpha would seem to be
-- types that have constructors that are solely that type
-- like:
-- SomeType = SomeType
-- But this doesn't really make sense. There is never any need
-- to have a constructor like that. So perhaps it is some
-- expression where we are unfolding a type, as we know
-- unfolding will start with a Mu f, and give back a
-- folded version of that type. You may be able to fold,
-- and then unfold again, any number of times, as long as there
-- is one more unfold than fold.

-- Things of the type Mu alpha. alpha -> alpha would
-- appear to be expressions where you are folding back up
-- something that has been unfolded. You can then unfold
-- and refold as many times as you want, and the type
-- should remain the same.


--2. 
data Defn = Defn { dName :: String,
                   dCtors :: [(String,[Type])] }

data Type = 
    TInt
  | TBool
  | TDatatype String
  | TArrow Type Type
  deriving Show

treeDefn :: Defn
treeDefn = Defn { dName = "BinaryTree",
                  dCtors = [("Empty",[]),
                            ("Rec",[TDatatype "BinaryTree",TInt,TDatatype "BinaryTree"])
                            ]}

data RecType = 
    RTInt
  | RTBool
  | RTVar String
  | RTUnit
  | RTPair RecType RecType
  | RTSum RecType RecType
  | RTMu String RecType
  | RTArrow RecType RecType
    deriving Show

wellFormedAux :: Set String -> RecType -> Bool
wellFormedAux s RTInt = True
wellFormedAux s RTBool = True
wellFormedAux s RTUnit = True
wellFormedAux s (RTPair a b) = (wellFormed a) && (wellFormed b)
wellFormedAux s (RTSum a b) = (wellFormed a) && (wellFormed b)
wellFormedAux s (RTArrow a b) = (wellFormed a) && (wellFormed b)
wellFormedAux s (RTVar st) = Set.member st s
wellFormedAux s (RTMu st r) = wellFormedAux (Set.insert st s) r

wellFormed :: RecType -> Bool
wellFormed r = wellFormedAux Set.empty r

toRec :: Type -> RecType
toRec TInt = RTInt
toRec TBool = RTBool
toRec (TDatatype s) = RTVar s
toRec (TArrow t1 t2) = RTArrow (toRec t1) (toRec t1)

toRecList :: [Type] -> RecType
toRecList [] = RTUnit
toRecList (x:[]) = toRec x
toRecList (x:xs) = RTPair (toRec x) (toRecList xs)

toSum :: [[Type]] -> RecType
toSum [] = error "No Constructors"
toSum (x:[]) = toRecList x 
toSum (x:xs) = RTSum (toRecList x) (toSum xs)

sumIt ctors = toSum (map snd ctors)

simpleDefnToRec :: Defn -> RecType
simpleDefnToRec (Defn name ctors) = RTMu name (sumIt ctors)

--3.

newtype Mu f = Fold { unFold :: f (Mu f) }

data ListF a f = Nil | Cons a f
type List a = Mu (ListF a)

nil = Fold Nil
cons x xs = Fold $ Cons x xs

toList l = 
  case unFold l of
    Nil -> []
    Cons x xs -> x:toList xs

fromList l =
  case l of
    [] -> nil
    (x:xs) -> Fold $ Cons x (fromList xs)

data RoseTreeF a f = Empty
                   | Node a [f]
                     deriving Show
                   
type RoseTree a = Mu (RoseTreeF a)

empty = Fold Empty
rec v vs = Fold (Node v [vs])

preorder :: RoseTree a -> List a
preorder r = case unFold r of
    Empty -> nil
    Node a (x:xs) -> fromList ((toList (cons a (preorder x))) ++ (toList (preorderAux xs)))

preorderAux :: [RoseTree a] -> List a
preorderAux [] = nil
preorderAux (x:xs) = fromList((toList (preorder x)) ++ (toList (preorderAux xs)))