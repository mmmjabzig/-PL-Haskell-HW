module HW09 where

import Control.Monad
import Data.Array.IO
import Data.List as List
import qualified Data.Map as Map
import Data.Map (Map(..),(!))
import System.Random
import Test.QuickCheck

rand :: Int -> Int -> IO Int
rand low high = if (low <= high) then getStdRandom (randomR (low,high))
                else error "incorrect bounds"

shuffleAux :: [a] -> Int -> [a]
shuffleAux [] _ = []
shuffleAux (x:[]) _ = [x]
shuffleAux xs i = let g = mkStdGen i in
                    let r = fst (randomR (0, (length xs)-1) (g)) in
                        let y = xs !! r in
                            (y):(shuffleAux ((init (fst (splitAt (r+1) xs))) ++
                            (snd (splitAt (r+1) xs))) i)


shuffleList :: [a] -> IO [a]
shuffleList xs = do num <- rand 0 50
                    return (shuffleAux xs num)

listToarray :: [a] -> IO (IOArray Int a)
listToarray x = newListArray (0,(length(x)-1)) x

readA :: Show a => IOArray Int a -> Int -> IO ()
readA arr i = do unpacked <- return arr
                 (lo, hi) <- getBounds arr
                 elem <- readArray unpacked i
                 unpackedElem <- return elem
                 print (unpackedElem)
                 if (i < hi) then readA arr (i+1)
                 else return()

test0 :: Show a => [a] -> IO()
test0 xs = do arr <- listToarray xs
              readA arr 0


exchange :: IOArray Int a -> Int -> Int -> IO ()
exchange arr i j = do unpackedArray <- return arr
                      elemI <- readArray unpackedArray i
                      elemJ <- readArray unpackedArray j
                      writeArray unpackedArray i elemJ
                      writeArray unpackedArray j elemI

shuffleArr :: Int -> Int -> Int -> IOArray Int a -> IO ()
shuffleArr index lo hi arr = do j <- rand lo hi
                                exchange arr index j
                                if (index < hi) then shuffleArr (index+1) lo hi arr
                                else return()

shuffle :: IOArray Int a -> IO ()
shuffle arr = do (lo, hi) <- getBounds arr
                 shuffleArr 0 lo hi arr


test1 :: Show a => [a] -> IO ()
test1 xs = do arr <- listToarray xs
              HW09.shuffle arr
              readA arr 0

arr2ListAux :: IOArray Int a -> Int -> IO [a]
arr2ListAux arr n = do a <- return arr
                       e <- readArray a n
                       (lo, hi) <- getBounds arr
                       if (n < hi)
                       then do rest <- arr2ListAux arr (n+1)
                               return (e:rest)
                       else return [e]
                       

arrayToList :: IOArray Int a -> IO [a]
arrayToList arr = do arr2ListAux arr 0

test2 :: Show a => [a] -> IO ()
test2 xs = do arr <- listToarray xs
              lst <- arrayToList arr
              print (lst)

shuffleFile :: FilePath -> IO String
shuffleFile f = do entireFile <- readFile f -- read in lines from a file, and store in an array
                   unpFile <- return entireFile
                   lineLst <- return (lines unpFile)
                   arr <- listToarray lineLst
                   HW09.shuffle arr
                   lst <- arrayToList arr
                   return (unlines lst)
                   
type Mapper a k v = a -> [(k,v)]
type Reducer k v = k -> [v] -> [v]

type MapperM m a k v = a -> m [(k,v)]
type ReducerM m k v = k -> [v] -> m [v]

mapReduce :: Ord k => Mapper a k v -> Reducer k v -> [a] -> [(k,[v])]
mapReduce m r = reduce r . shuffleKeys . concatMap (map listifyVal . m)
  where listifyVal (k,v) = (k,[v])
        shuffleKeys = Map.fromListWith (++)
        reduce r = Map.toList . Map.mapWithKey r

wordCount = mapReduce countWords sumCounts
 where countWords = map (\w -> (w,1)) . words
       sumCounts _ cs = [sum cs]

listifyVals :: [(k,v)] -> [(k,[v])]
listifyVals [] = []
listifyVals (x:xs) = (listifyVal x):(listifyVals xs) where listifyVal (a,b) = (a,[b])

mapHelp :: Monad m => MapperM m a k v -> [a] -> m [(k,v)]
mapHelp m (x:[]) = m x
mapHelp m (x:xs) = do fst <- m x
                      rst <- mapHelp m xs
                      return (fst ++ rst)

reduceHelp :: Monad m => ReducerM m k v -> [(k, [v])] -> m [(k, [v])]
reduceHelp r (x:[]) = do fir <- r (fst x) (snd x)
                         return [((fst x), fir)]
reduceHelp r (x:xs) = do fir <- r (fst x) (snd x)
                         rst <- reduceHelp r xs
                         return (((fst x), fir):rst)

shuffleM :: Ord k => [(k, [v])] -> [(k, [v])]
shuffleM [] = []
shuffleM ls = Map.toList (Map.fromListWith (++) ls)

mapReduceM :: (Ord k, Monad m) => MapperM m a k v -> ReducerM m k v -> [a] -> m [(k,[v])]
mapReduceM m r input = do stp1 <- mapHelp m input
                          stp2 <- return (listifyVals stp1)
                          stp3 <- return (shuffleM stp2)
                          stp4 <- return (reduceHelp r stp3)
                          stp4
                          --stp4 <- Map.mapWithKey r stp3
                          --return (Map.toList stp4)


wordCountM = mapReduceM countWords sumCounts
 where countWords = return . map (\w -> (w,1)) . words
       sumCounts w cs = do
         when (length cs > 1) $ putStrLn $ "Lots of " ++ w ++ "!"
         return [sum cs]
                          
                        

--mapReduceM :: (Ord k, Monad m) => MapperM m a k v -> ReducerM m k v -> [a] -> m [(k,[v])]
--mapReduceM m r input = shuffleKeysM (concatMap (map listifyVal . m) input)
  --where listifyVal m (k,v) = m (k,[v])
    --    shuffleKeys = Map.fromListWith (++)
      --  reduce r = Map.toList . Map.mapWithKey r



type Id = String

data Stmt a = Assign Id a | Let Id a | Expr a deriving Show
type DoBlock a = [Stmt a]

data MExpr a = 
    MLam Id (MExpr a) | MApp (MExpr a) (MExpr a)
  | MBind a Id (MExpr a) | MReturn (MExpr a) | MEmbed a deriving Show

convert :: DoBlock a -> MExpr a
convert [] = error "bad input"
convert (b:[]) = case b of
    (Expr a) -> MEmbed a
    _ -> error "must end in Expr"
convert (b:bs) = case b of
    (Assign id a) -> MBind a id (convert bs)
    (Let id a) -> MApp (MLam id (convert bs)) (MEmbed a)
    (Expr a) -> MEmbed a

prop_rev_involutive l = List.reverse(List.reverse l) == l

collatz :: Int -> Bool
collatz n = if (n == 1) then True
            else if (even n) then collatz (n `div` 2)
                 else collatz (3*n + 1)

prop_Collatz n = n >=1 ==> collatz n

data ArithExp = 
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp
  deriving Show

eval :: ArithExp -> Int
eval (Num i) = i
eval (Plus e1 e2) = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Neg e) = 0 - eval e

randAE :: Int -> Int -> IO ArithExp
randAE l h = do n <- rand l h
                return (Num n)

instance Arbitrary ArithExp where
  arbitrary = sizedAExp 5
                   
prop_double exp = eval (Plus exp  exp) == eval (Times (Num 2) exp)


aExp :: Gen (ArithExp)
aExp = oneof [return (Num 5),
              (Plus) <$> aExp <*> aExp,
              (Times) <$> aExp <*> aExp,
              (Neg) <$> arbitrary]

sizedAExp :: Int -> Gen (ArithExp)
sizedAExp 0 = return (Num 2)
sizedAExp n = oneof [return (Num 3),
                     (Plus) <$> (sizedAExp (abs(n `div` 2))) <*> (sizedAExp (abs(n `div` 2))),
                     (Times) <$> (sizedAExp (abs(n `div` 2))) <*> (sizedAExp (abs(n `div` 2))),
                     (Neg) <$> (sizedAExp (abs(n-1)))]

smallExp :: Int -> Gen (ArithExp)
smallExp n = sizedAExp (n `mod` 5)
