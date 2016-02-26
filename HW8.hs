module Hw08 where

import Control.Applicative
import Data.Char

--Part 1

data ArithExp = 
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp

eval :: ArithExp -> Int
eval (Num n) = n
eval (Plus a b) = (eval a) + (eval b)
eval (Times a b) = (eval a) * (eval b)
eval (Neg n) = 0 - (eval n)

--1.

instance Eq ArithExp where
    Num a == Num b = (a == b)
    (Plus a b) == (Plus x y) = (a == x) && (b == y)
    (Times a b) == (Times x y) = (a == x) && (b == y)
    Neg a == Neg b = (a == b)

instance Show ArithExp where
    show (Num n) = show n
    show (Plus a b) = (show a) ++ " + " ++ (show b)
    show (Times (Plus a b) (Plus c d)) =
      "(" ++ (show a) ++ " + " ++ (show b) ++ ")" ++
      " * " ++ "(" ++ (show c) ++ " + " ++ (show d) ++ ")"
    show (Times (Plus a b) c) =
      "(" ++ (show a) ++ " + " ++ (show b) ++ ")" ++ " * " ++ (show c)
    show (Times a (Plus b c)) =
      (show a) ++ " * " ++ "(" ++ (show b) ++ " + " ++ (show c) ++ ")"
    show (Times (Neg (Plus a b)) (Neg (Plus c d))) =
      "(" ++ "-" ++ (show a) ++ " - " ++ (show b) ++ ")" ++ " * " ++
      "(" ++ "-" ++ (show c) ++ " - " ++ (show d) ++ ")"
    show (Times (Neg (Plus a b)) c) =
      "(" ++ "-" ++ (show a) ++ " - " ++ (show b) ++ ")" ++ " * " ++ (show c)
    show (Times a (Neg (Plus b c))) =
      (show a) ++ " * " ++ "(" ++ "-" ++(show b) ++ " - " ++ (show c) ++ ")"
    show (Times a b) = (show a) ++ " * " ++ (show b)
    show (Neg n) = "-" ++ show n

--2.

--added "Ord a => " to type signature for toAscList for now.

class Setlike f where
  emp :: f a

  singleton :: a -> f a

  union :: Ord a => f a -> f a -> f a
  union = fold insert

  insert :: Ord a => a -> f a -> f a 
  insert = union . singleton

  delete :: Ord a => a -> f a -> f a
  delete x s = fold (\y s' -> if x == y then s' else insert y s') emp s

  isEmpty :: f a -> Bool
  isEmpty = (==0) . size

  size :: f a -> Int
  size = fold (\_ count -> count + 1) 0 

  isIn :: Ord a => a -> f a -> Bool
  isIn x s = maybe False (const True) $ getElem x s

  getElem :: Ord a => a -> f a -> Maybe a

  fold :: (a -> b -> b) -> b -> f a -> b

  toAscList :: Ord a => f a -> [a] -- must return the list sorted ascending
  toAscList = fold (:) []

deleteAux::Eq a => a -> [a] -> [a] -> [a]
deleteAux _ [] l = l
deleteAux a (x:xs) l = if (a == x)
                       then deleteAux a xs l
                       else deleteAux a xs (l ++ [x])

sizeAux::[a] -> Int -> Int
sizeAux [] n = n
sizeAux (x:xs) n = sizeAux xs (n+1)

isInAux::Eq a => a -> [a] -> Bool
isInAux _ [] = False
isInAux a (x:xs) = if (a == x)
                       then True
                       else isInAux a xs

getElemAux::Eq a => a -> [a] -> Maybe a
getElemAux _ [] = Nothing
getElemAux a (x:xs) = if (a == x)
                       then Just a
                       else getElemAux a xs

toAscListAux::Ord a => [a] -> [a]
toAscListAux [] = []
toAscListAux l = (minimum l):(toAscListAux (delete (minimum l) (l)))

instance Setlike [] where
    emp = []
    
    singleton a = [a]

    union l1 l2 = l1 ++ l2
    
    insert a l = a:l
    
    delete a (x:xs) = deleteAux a (x:xs) []
    
    isEmpty l = (size l == 0) 
    
    size l = sizeAux l 0
    
    isIn a l = isInAux a l
    
    getElem a l = getElemAux a l
    
    fold f a l = foldr f a l
    
    toAscList l = toAscListAux l

evensUpToTen :: [Int]
evensUpToTen = fold insert emp [0,2,4,6,8]

data BST a = Empty | Node (BST a) a (BST a)

unionBSTAux:: Ord a => BST a -> BST a -> BST a
unionBSTAux Empty b = b
unionBSTAux b Empty = b
unionBSTAux b (Node s1 x s2) = union (insert x b) (union s1 s2)

insertBSTAux :: Ord a => a -> BST a -> BST a
insertBSTAux a Empty = Node (Empty) a (Empty)
insertBSTAux a (Node s1 x s2) =
    if ((compare a x) == GT)
    then Node s1 x (insertBSTAux a s2)
    else Node (insertBSTAux a s1) x s2

deleteBSTAux:: Ord a => a -> BST a -> BST a
deleteBSTAux a Empty = Empty
deleteBSTAux a (Node s1 x s2) =
    if (a == x)
    then union s1 s2
    else if ((compare a x) == GT)
         then Node s1 x (deleteBSTAux a s2)
         else Node (deleteBSTAux a s1) x s2

sizeBSTAux::BST a -> Int -> Int
sizeBSTAux Empty n = n
sizeBSTAux (Node s1 x s2) n = n + (sizeBSTAux s1 0) + (sizeBSTAux s2 0)

isInBSTAux:: Ord a => a -> BST a -> Bool
isInBSTAux a Empty = False
isInBSTAux a (Node s1 x s2) =
    if (a == x)
    then True
    else (isInBSTAux a s1) || (isInBSTAux a s2)

toAscListBSTAux:: BST a -> [a]
toAscListBSTAux Empty = []
toAscListBSTAux (Node s1 x s2) = (toAscListBSTAux s1) ++ [x] ++ (toAscListBSTAux s2)

toList:: BST a -> [a]
toList Empty = []
toList (Node s1 x s2) = (toList s1) ++ [x] ++ (toList s2)

instance Setlike BST where
    emp = Empty
    
    singleton a = Node (Empty) a (Empty)

    union b1 b2 = unionBSTAux b1 b2
    
    insert x b = insertBSTAux x b
    
    delete x b = deleteBSTAux x b
    
    isEmpty b = (size b == 0)
    
    size b = sizeBSTAux b 0
    
    isIn a b = isInBSTAux a b
    
    getElem a b = if (isIn a b) then Just a else Nothing
    
    fold f a b = foldr f a (toList b)
    
    toAscList b = toAscListBSTAux b

instance Ord a => Eq (BST a) where
  Empty == Empty = True
  Node Empty v Empty == Node Empty x Empty = v == x
  Node s1 v s2 == Node t1 x t2 = (v == x) && (s1 == t1) && (s2 == t2)
  _ == _ = False

instance Show a => Show (BST a) where
  show Empty = "Empty"
  show (Node s1 v s2) = "Node ( " ++ show s1 ++ " ) " ++ show v ++ " ( " ++ show s2 ++ " )"

fromList :: (Setlike f, Ord a) => [a] -> f a
fromList [] = emp
fromList (x:[]) = singleton x
fromList (x:xs) = fold insert emp (x:xs)

difference :: (Setlike f, Ord a) => f a -> f a -> f a
difference xs ys = fold delete xs ys

subset :: (Setlike f, Ord a) => f a -> f a -> Bool
subset xs ys = isEmpty (difference ys xs)

-- .3

newtype KV k v = KV { kv :: (k,v) }

instance Eq k => Eq (KV k v) where
  (KV kv1) == (KV kv2) = fst kv1 == fst kv2

instance Ord k => Ord (KV k v) where
  compare (KV kv1) (KV kv2) = compare (fst kv1) (fst kv2)

instance (Show k, Show v) => Show (KV k v) where
  show (KV (k,v)) = show k ++ " |-> " ++ show v

type Map f k v = f (KV k v)
type ListMap k v = Map [] k v
type TreeMap k v = Map BST k v

emptyMap :: Setlike f => Map f k v
emptyMap = emp

lookup :: (Setlike f, Ord k) => k -> Map f k v -> Maybe v
lookup k m = case getElem (KV (k, undefined)) m of
    Just (KV (k, v)) -> Just v
    Nothing -> Nothing

extend :: (Setlike f, Ord k) => k -> v -> Map f k v -> Map f k v
extend k v m = insert (KV (k,v)) m

remove :: (Setlike f, Ord k) => k -> Map f k v -> Map f k v
remove k m = delete (KV (k, undefined)) m

toPair :: (KV k v) -> (k,v)
toPair (KV (k,v)) = (k,v)

toAssocList :: (Setlike f, Ord k) => Map f k v -> [(k,v)]
toAssocList m = map toPair (toAscList m)

--Part 2
-- 1.
pair :: Applicative f => f a -> f b -> f (a,b)
pair = liftA2 (,)

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> maybe Nothing (Just . first f) (parse p s)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a,s)
  f <*> a = Parser $ \s -> maybe Nothing (\(g,s') -> parse (fmap g a) s') $ parse f s

instance Alternative Parser where
  empty = Parser $ const empty
  l <|> r = Parser $ \s -> parse l s <|> parse r s


ensure :: (a -> Bool) -> Parser a -> Parser a
ensure p parser = Parser $ \s ->
   case parse parser s of
     Nothing -> Nothing
     Just (a,s') -> if p a then Just (a,s') else Nothing

lookahead :: Parser (Maybe Char)
lookahead = Parser f
  where f [] = Just (Nothing,[])
        f (c:s) = Just (Just c,c:s)

--endOrSpace :: a -> Bool
--endOrSpace a = 

token::String-> Parser String
token s = (str s <* ensure (\a -> (a == Just (' ')) || (a == Nothing)) lookahead)

eof :: Parser ()
eof = Parser $ \s -> if null s then Just ((),[]) else Nothing

zeroOrMore, oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p -- a/k/a some
zeroOrMore p = oneOrMore p <|> pure [] -- a/k/a many

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs) = if p x then Just (x,xs) else Nothing

spaces :: Parser ()
spaces = pure () <* zeroOrMore (satisfy isSpace)

char :: Char -> Parser Char
char c = spaces *> satisfy (==c)

str :: String -> Parser String
str s = spaces *> loop s
  where loop [] = pure []
        loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs

untouched :: String -> Bool
untouched "True" = False
untouched "False" = False
untouched "if" = False
untouched "then" = False
untouched "else" = False
untouched "let" = False
untouched "rec" = False
untouched "isZero" = False
untouched "incr" = False
untouched "decr" = False
untouched _ = True

theId :: Parser Id
theId = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlpha <|> satisfy isDigit)

safeID :: Parser Id
safeID = ensure untouched theId
--(check if it is already one the other tokens)


type Id = String

data Expr =
    EVar Id
  | ETrue
  | EFalse
  | EIf Expr Expr Expr
  | ENum Int
  | EIncr Expr
  | EDecr Expr
  | EIsZero Expr
  | EApp Expr Expr
  | ELam Id Expr
  
  deriving (Show, Eq)


expr :: Parser Expr
expr = (spaces *> outer <* eof)
    

outer :: Parser Expr
outer = (lam <|> iff <|> lett <|> lettRec <|> check1)

lam = (ELam <$> (spaces *> str "\\" *> spaces *> safeID) <*> (spaces *> str "->" *> outer))
iff = (EIf <$> (spaces *> token "if" *> spaces *> check1) <*> (token "then" *> spaces *> outer) <*> (token "else" *> spaces *> outer))
lett = (toLet <$> (getTrip <$> (token "let" *> spaces *> safeID) <*> (spaces *> char '=' *> outer) <*> (spaces *> token "in" *> spaces *> outer)))
lettRec = (toLetRec <$> (getTrip <$> (token "let rec" *> spaces *> safeID) <*> (spaces *> char '=' *> outer) <*> (spaces *> token "in" *> spaces *> outer)))

check1 :: Parser Expr 
check1 = (isZero <|> incr <|> decr <|> appExpr)

isZero = (EIsZero <$> (spaces *> token "isZero" *> spaces *> atom))
incr = (EIncr <$> (spaces *> token "incr" *> spaces *> atom))
decr = (EDecr <$> (spaces *> token "decr" *> spaces *> atom))

--appExpr
appExpr :: Parser Expr
appExpr = (appcase <|> atom)

appcase = (EApp <$> atom <*> (spaces *> outer))

--atom
atom :: Parser Expr
atom = (var <|> tru <|> fals <|> num <|> (char '(' *> outer <* char ')'))

var = (EVar <$> (spaces *> safeID))
tru = (giveTrue <$> (spaces *> token "true"))
fals = (giveFalse <$> (spaces *> token "false"))
num = (spaces *> (ENum . read <$> oneOrMore (satisfy isDigit)))

appHelper :: [Expr] -> Expr
appHelper (x:y:[]) = EApp x y
appHelper (x:xs) = EApp x (appHelper xs)


giveTrue :: String -> Expr
giveTrue _ = ETrue

giveFalse :: String -> Expr
giveFalse _ = EFalse

getTrip :: a -> b -> c -> (a,b,c)
getTrip a b c = (a,b,c)

appHelp :: Expr -> Expr ->  Expr
appHelp e1 e2 = EApp (e1) (e2)

toLet :: (Id, Expr, Expr) -> Expr
toLet (a,b,c) = (EApp (ELam a c) b)

toLetRec :: (Id, Expr, Expr) -> Expr
toLetRec (a,b,c) = EApp (ELam a c) (EApp (ELam "f" (EApp (ELam ("x")
  (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))) (ELam ("x")
  (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))))) (ELam a b))

--2.
-- The main advantage of taking a combinator approach to parsing seems
-- to be that lexing and parsing is all done in a single step. In other
-- words we didn't have to write a separate lexer, we just had to write
-- a parser. The disadvantage seems to be that the code is harder to
-- understand. It was pretty easy to look at the lexer.x and parser.y
-- files and understand what was going on. That isn't necessarily the
-- case here. On the other hand the advantage of using a lexer/parser
-- generator is that it is much easier to understand and read, but
-- lexing and parsing need to be done separately. It is worth saying
-- that once you understand what is going on, and what the various
-- syntactic symbols mean, it is not incredibly hard to understand the
-- combinator approach, there is just a large barrier to entry in terms
-- of the sytax. Once you understand the syntax, the two actually are
-- very similar in terms of structure (at least the way we wrote them).  