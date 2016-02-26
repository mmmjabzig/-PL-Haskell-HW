module Hw06 where

import qualified Data.Map as Map
import Data.Map (Map, (!))

-- (a) `a x y = x ++ " " ++ y`
-- a :: [Char] -> [Char] - [Char]
-- you are appending x and y with a string, so they both must be
-- strings, and the result in turn, must also be a string
aFun x y = x ++ " " ++ y
-- (b) `b x y = x:head y`
--b :: a -> [[a]] -> [a]
-- Say that x has some type a. We are taking x, and consing it
-- onto the head of y. Because we can only cons x onto a list,
-- and we are consing it onto the head of y, y must have the
-- type [[a]], as we are taking the first list and consing on x.
-- The result then is x consed onto a list, so the result is of
-- type [a]
bFun x y = x:head y
-- (c) `c f = \y -> f y`
-- c :: (t1 -> t) -> t1 -> t
-- c takes in a function, and then applies that function to y.
-- So we have a function that has the type (t1 -> t), and
-- then we need an input of type t1, so we can then apply
-- the function, and get out a result of type t.
cFun f = \y -> f y
-- (d) `d (f,x) = f (f x)`
-- d :: (t -> t, t) -> t
-- Because we can apply f to the result f applied to x, we know
-- that the input of f must have the same type as the output of
-- f. Now, just as in c, we have a function, and an input. The
-- only difference is, this time it is in a pair. So the input
-- is a function that takes t -> t, and an input of type t, and
-- the result is of type t. 
dFun (f,x) = f (f x)
-- (e) `e x y b = if b y then x else y`
-- e :: t -> t -> (t -> Bool) -> t
-- We know first that x and y must have the same type. We also
-- know that b y must result in a boolean type. So b must be
-- some function that takes y and gives back some boolean. So
-- if we say y has type t, then b has type (t -> Bool), and x
-- must have type t. The output will either be x or y, so the
-- whole type shown above must be correct, as it takes as
-- input x y b in that order.
eFun x y b = if b y then x else y
-- (f) `f x y = foldl x Nothing (reverse y)`
-- f :: (Maybe a1 -> a -> Maybe a1) -> [a] -> Maybe a1
-- f is essentially a wrapper function for foldl. We know
-- that foldl has the type:
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- All f is doing is providing the function (b -> a -> b),
-- and the list [a]. Because the initial value is
-- Nothing, we know that we need to use the Maybe type.
-- Then we know the input of f is the function, and the
-- list, and the output will be the output of the foldl
-- function. So we end up with the type above.
fFun x y = foldl x Nothing (reverse y)

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
  | ELam Id Type Expr
  deriving (Show, Eq)

data Type =
    TyBool
  | TyNum
  | TyArrow Type Type
  deriving (Show, Eq)

type Context = Map Id Type

typeCheck :: Context -> Expr -> Either String Type
typeCheck ctx (EVar x) = Right (ctx ! x)
typeCheck ctx (ETrue) = Right TyBool
typeCheck ctx (EFalse) = Right TyBool
typeCheck ctx (EIf x y z) =
    case ((typeCheck ctx x), (typeCheck ctx y)) of
        (Right TyBool, Right t) -> Right (TyArrow TyBool t)
        (Right TyBool, Left s) -> Left s
        (Right t1, Right t2) -> Right (TyArrow t1 t2)
        (_, _) -> Left "Expected boolean for first argument"
typeCheck ctx (ENum i) = Right TyNum
typeCheck ctx (EIncr i) =
    case (typeCheck ctx i) of
        (Right TyBool) -> Left "Expected number for argument"
        (Right TyNum) -> Right (TyArrow TyNum TyNum)
        (Right t) -> Right (TyArrow t TyNum)
        (Left s) -> Left s
typeCheck ctx (EDecr i) =
    case (typeCheck ctx i) of
        (Right TyBool) -> Left "Expected number for argument"
        (Right TyNum) -> Right (TyArrow TyNum TyNum)
        (Right t) -> Right (TyArrow t TyNum)
        (Left s) -> Left s
typeCheck ctx (EIsZero x) =
    case (typeCheck ctx x) of
        (Right TyBool) -> Left "Expected number for argument"
        (Right TyNum) -> Right (TyArrow TyNum TyNum)
        (Right t) -> Right (TyArrow t TyNum)
        (Left s) -> Left s
typeCheck ctx (EApp x y) =
    case (typeCheck ctx x, typeCheck ctx y) of
        (Right t1, Right t2) -> Right (TyArrow t1 t2)
        (_, Left s) -> Left s
        (Left s, _) -> Left s
typeCheck ctx (ELam i t x) =
    case (typeCheck ctx x) of
        (Right t) -> typeCheck (Map.insert i t ctx) x
        (Left s) -> Left s

-- 3.
-- a) You would have to change the Type datatype to include some
-- kind of TyPair like TyPar Type Type . You could add an EPair
-- to Expr, but you wouldn't have to, because you can just have a
-- pair of things that are already in Expr.

-- b)
--      |- x : t  |- y : t
--     _______________________________
--      |- (x, y) : TyPair t t
--
--      |- x : t1  |- y : t2
--     _______________________________
--      |- (x, y) : TyPair t1 t2

-- 4.
-- a)
--1 let x = 2 in                    | x = 2
--2   let val fun f(y) = x + y in   | f(y) = x + y = 2 + y
--3     let val x = 7 in            | x = 7
--4       x +                       | x + f(x) = 7 + f(7) = 7 + (2 + 7) = 16
--5            f(x)                 | Result 16
--6     end                         |
--7   end                           |
--8 end;                            |
--
-- x is initially set to 2 on line 1.
-- f(y) is defined in terms of x and y.
-- x is then set to be 7
-- we then evaluate x + f(x).
-- because x has been set to 7 in the innermost let statement, we have
-- 7 + f(7)
-- But because we are dealing with static scoping, the setting of
-- x = 7 in the innermost let, cannot persist outside the scope
-- of that let statement, so f(y) = 2 + y, as x=2 in the scope of
-- that statement.
-- So the result evaluates to 7 + (2 + 7) = 16
--
-- b)
--1 let x = 2 in                    | x = 2
--2   let val fun f(y) = x + y in   | f(y) = x + y
--3     let val x = 7 in            | x = 7
--4       x +                       | x + f(x) = 7 + f(7) = 7 + (7 + 7) = 21
--5            f(x)                 | Result 21
--6     end                         |
--7   end                           |
--8 end;                            |
--
-- x is initially set to 2 on line 1.
-- f(y) is defined in terms of x and y.
-- x is then set to be 7
-- we then evaluate x + f(x).
-- because x has been set to 7 in the innermost let statement, we have
-- 7 + f(7)
-- Because we are now dealing with dynamic scoping, the setting of
-- x = 7 in the innermost let, can persist outside the scope
-- of that let statement, so f(y) = 7 + y, as the setting of
-- x = 7, can persist outside the innermost let, up to the
-- definition of f(y).
-- So the result evaluates to 7 + (7 + 7) = 21

-- 5.
-- b) Yes, because it is statically scoped, any variables declared
-- inside the let statement cannot be referenced/used outside the let
-- statement (at least with values set inside the let statement). Thus
-- if the variables aren't declared inside the let statement, they can
-- be eliminated.

-- c) No, as in the case of dynamic scoping, variable declarations
-- made within a let statement can be used/persist outisde the
-- let statement. Therefore, eliminating those variables can change
-- the result, and alter the program. So the variables cannot be
-- eliminated

-- 6.
-- a) \x((\f.\x f(f x)) (\y. y + x))  - Original
--    \x((\f.\a f(f a)) (\y. y + x))  - Rename inner x
--    \x.\a.(\y.y + x) ((\y.y + x) a) - Substitute for f
--    \x.\a.(\y.y + x) a + x          - Apply function
--    \x.\a. a + x + x                - Apply function again
--
-- b) Renaming bound variables ensures static scope, because
-- it ensures that no variable can be referenced outside the
-- lambda expression that defines it. In this case, renaming
-- the inner x prevented it from being captured by the outer
-- lambda x expression, and made sure it wasn't referenced
-- outside the scope of it's lambda.
--
-- c) foo(3)(2) = 2 + 3 + 3 = 8
--
-- d) \x((\f.\x f(f x)) (\y. y + x))
--    \x.(\y.y + x) ((\y.y + x) x) - Substitute for f
--    \x.(\y.y + x) x + x          - Apply function
--    \x. x + x + x                - Apply function again
-- The answer is simply the original lambda, expression, except
-- we don't rename any variables. This allows the inner x to
-- be referenced outside the scope of the inner lambda expression
-- for x, and be captured by the outer lambda expression for
-- x.
--
-- e) foo(3)(2) = 3 + 3 + 3 = 9
--
-- f) We have basically already gone over this in parts b and d.
-- If you rename bound variables, this ensures that they cannot
-- be captured by another lambda expression with a different scope.
-- Renaming corresponds to static scoping, whereas not renaming
-- corresponds to dynamic scoping. Allowing a variable or multiple
-- variables to be referenced outside the scope of their lambda
-- expression or let statement etc, can change the result of the
-- entire evaluation. In this case we see that not renaming the
-- inner x means that instead of getting a + x + x, where a is the
-- inner x renamed, we get instead x + x + x, which is a different
-- result. Thus these differences can affect the result, and
-- should always be considered.