module Hw03 where

import Prelude

data Instruction = 
    IPush Int
  | IPlus
  | ITimes
  | INeg
  deriving Show

type Stack = [Int]

instruction :: Instruction -> Stack -> Maybe Stack
instruction (IPush n) s = Just (n:s)
instruction IPlus (n:m:s) = Just ((n+m):s)
instruction ITimes (n:m:s) = Just ((n*m):s)
instruction INeg (n:s) = Just ((0-n):s)
instruction IPlus (n:[]) = Just [n]
instruction IPlus [] = Nothing
instruction ITimes (n:[]) = Just [n]
instruction ITimes [] = Nothing
instruction INeg [] = Nothing

-- With an instruction, you might try to perform an operation that
-- isn't possible with the given stack input. For example trying
-- to execute IPlus on the empty stack would result in an error because
-- there aren't any numbers to add together. This isn't really a problem
-- with the eval function, because everything it will need must be passed
-- in as a parameter. It doesn't need to work with a stack, Arithmetic
-- expression have a form so that they will just work.

type Program = [Instruction]

getReal :: Maybe Stack -> Stack
getReal Nothing = []
getReal (Just s) = s

program :: Program -> Stack -> Maybe Stack
program ([]) ([]) = Nothing
program ([]) s = Just s
program (i:p) s = program (p) (getReal (instruction i s))

depth :: Program -> Int -> Maybe Int
depth ([]) n = Just n
depth ((IPush x):p) n = depth p (n+1)
depth ((INeg):p) n = if (n <= 0) then Nothing else depth p n
depth ((IPlus):p) n = if (n <= 1) then Nothing else depth p (n-1)
depth ((ITimes):p) n = if (n <= 1) then Nothing else depth p (n-1)


data ArithExp = 
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp
  deriving (Eq,Show)

compile :: ArithExp -> Program
compile (Num n) = [IPush n]
compile (Plus a b) = (IPlus):((compile a) ++ (compile b))
compile (Times a b) = (ITimes):((compile a) ++ (compile b))
compile (Neg a) = (INeg):(compile a)

runAndCompile :: ArithExp -> Int
runAndCompile e = 
 case program (compile e) [] of
    Just [x] -> x
    _ -> error "Your compiler is buggy!"

-- 2.
-- (\x.\y.xy) (\x.xy) [b/y]
-- = (\x.\y.xy) (\x.xb) [(\x.xb)/x]
-- = (\y.(\x.xb) y)
-- = \y.yb

-- 3.
-- a)
-- ((\f.\g.f(g 1)) (\x.x+4)) (\y.3-y)
-- (\g.(\x.x+4) (g 1)) (\y.3-y)
-- (\x.x+4) ((\y.3-y) 1)
-- (\y.(3-(y+4)) 1)
-- 3 - 1 + 4 = 6
-- b)
-- ((\f.\g.f(g 1)) (\x.x+4)) (\y.3-y)
-- (\f.f(\y.3-y) 1 (\x.x+4))
-- (\x.x+4) (\y.3-y) 1
-- (\x.(3-x)+4) 1
-- 3 - 1 + 4 = 6

--4.
-- a)
-- minus m n = \m.\n.n pred m
--
-- b)
-- lessThan m n = \m.\n.isZero(minus (m+1) n)
--
-- c)
-- fib = y(\fib.\n.cond (isZero n) one
-- (cond (isZero (pred n)) one
-- (plus (fib (pred (pred n))) (fib (pred n)))
-- ))
--
-- d)
-- fib 2 = (y ff) 2
-- = ((xx.ff (xx)).(\x.ff(xx)) 2)
-- = ff (\x.ff(xx))(\x.ff(xx)) 2
-- = cond (isZero 2) one
--      (cond (isZero 1) one
--          (plus (fib 0) (fib 1)))
--
-- = plus ((xx.ff (xx)).(\x.ff(xx)) 0) ((xx.ff (xx)).(\x.ff(xx)) 1)
-- = plus (ff (\x.ff(xx))(\x.ff(xx)) 0) (ff (\x.ff(xx))(\x.ff(xx)) 1)
-- = plus (cond (isZero 0) one
--            (cond (isZero (pred 0)) one
--                (plus (fib (pred (pred 0))) (fib (pred 0)))))
--        (cond (isZero 1) one
--            (cond (isZero 0) one
--                (plus (fib (pred (pred 1))) (fib (pred 1)))))
-- = plus (one) (one) = two = 2
