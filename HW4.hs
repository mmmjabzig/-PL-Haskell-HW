module Hw04 where

import Prelude hiding (succ,pred)

import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)

type Id = String

data LCExpr = 
    LCVar Id
  | LCApp LCExpr LCExpr
  | LCLam Id LCExpr
  deriving (Show,Eq)

freeVars :: LCExpr -> Set Id
freeVars (LCVar id) = Set.fromList[id]
freeVars (LCApp l1 l2) = Set.union (freeVars l1) (freeVars l2)
freeVars (LCLam id l) = freeVars l

wellScoped :: LCExpr -> Bool
wellScoped l = (freeVars l) == Set.empty

data LCValue = Closure Id LCExpr (Env LCValue) deriving Eq
type Env a = Map Id a

extend :: Env a -> Id -> a -> Env a
extend env x v = Map.insert x v env

evalLC :: Env LCValue -> LCExpr -> LCValue
evalLC env (LCVar x) = env ! x
evalLC env (LCApp e1 e2) = 
  case evalLC env e1 of
    Closure x e env' -> evalLC (extend env' x (evalLC env e2)) e
evalLC env (LCLam x e) = Closure x e env

runLC :: LCExpr -> LCValue
runLC = evalLC Map.empty

true  = LCLam "x" (LCLam "y" (LCVar "x"))
false = LCLam "x" (LCLam "y" (LCVar "y"))
cond  = LCLam "p" (LCLam "x" (LCLam "y" (LCApp (LCApp (LCVar "p") (LCVar "t")) (LCVar "f"))))

zero = LCLam "s" (LCLam "z" (LCVar "z"))
succ = LCLam "n" (LCLam "s" (LCLam "z" 
         (LCApp (LCVar "s") (LCApp (LCApp (LCVar "n") (LCVar "s")) (LCVar "z")))))

lam :: [Id] -> LCExpr -> LCExpr
lam [] l = l
lam (x:xs) l = LCLam x (lam xs l) 

app :: [LCExpr] -> LCExpr
app [] = undefined
app (x:xs) = LCApp x (app xs)

isZero, plus, times :: LCExpr
isZero = LCLam "n" (LCApp (LCLam "x" false) true)
plus = LCLam "n" (LCLam "m" (LCApp (LCApp (LCVar "n") (succ)) (LCVar "m")))
times = LCLam "n" (LCLam "m" (LCApp (LCApp (LCVar "n") (LCApp (LCVar "m") (plus))) (zero)))

pair = lam ["a","b","c"] $ app $ map LCVar ["c","a","b"]
first = lam ["c"] $ app [LCVar "c", true]
second = lam ["c"]$ app [LCVar "c", false]

pred, sub :: LCExpr
pred = LCLam "n" (LCLam "s" (LCLam "z" (LCApp (second) (LCApp (LCApp (LCVar "n") (LCLam "p" (LCApp (pair) (LCApp ((LCApp (LCVar "s") (LCApp (first) (LCVar "p")))) (LCApp (first) (LCVar "p")))))) (LCApp (LCApp (pair) (LCVar "z")) (LCVar "z"))))))

sub = LCLam "m" (LCLam "n" (LCApp (LCApp (LCVar "n") (pred)) (LCVar "m")))

data Value = 
    VClosure Id Expr (Env Value)
  | VNumber Int
  | VBoolean Bool
  deriving Show

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

-- add in error cases!
eval :: Env Value -> Expr -> Value
eval env (EVar id) = env ! id
eval env (ETrue) = VBoolean True
eval env (EFalse) = VBoolean False
eval env (EIf e1 e2 e3) =
    case eval env e1 of
        VBoolean True -> eval env e2
        VBoolean False -> eval env e3
        VNumber _ -> error "first arg of EIf must be a VBoolean"
        VClosure _ _ _ -> error "first arg of EIf must be a VBoolean"
eval env (ENum int) = VNumber int
eval env (EIncr e) =
    case eval env e of
       VNumber int -> VNumber (int + 1)
       VBoolean _ -> error "arg of EIncr must be a VNumber"
       VClosure _ _ _ -> error "arg of EIncr must be a VNumber"
eval env (EDecr e) =
    case eval env e of
       VNumber int -> VNumber (int + 1)
       VBoolean _ -> error "arg of EIncr must be a VNumber"
       VClosure _ _ _ -> error "arg of EIncr must be a VNumber"
eval env (EIsZero e) =
    case eval env e of
       VNumber n -> VBoolean (n == 0)
       VBoolean _ -> error "arg of EIncr must be a VNumber"
       VClosure _ _ _ -> error "arg of EIncr must be a VNumber"
eval env (EApp e1 e2) =
    case eval env e1 of
       VClosure x e env' -> eval (extend env' x (eval env e2)) e
       VNumber _ -> error "first arg must be a closure"
       VBoolean _ -> error "first arg must be a closure"
eval env (ELam id e) = VClosure id e (env)


embed :: LCExpr -> Expr
embed (LCVar id) = EVar id
embed (LCApp e1 e2) = EApp (embed e1) (embed e2)
embed (LCLam id e) = ELam id (embed e) 

y :: Expr
y = ELam "f" $ EApp g g
  where g = ELam "x" $ EApp f (EApp x x)
        f = EVar "f"
        x = EVar "x"

toChurch :: Expr
toChurch = EApp y (ELam "c" (ELam "n" (EIf (EIsZero (EVar "n")) (ELam "s" (ELam "z" (EVar "z"))) (EIncr (EDecr (EVar "n"))))))

inc :: Expr
inc = EIncr (EVar "n") 

fromChurch :: Expr
fromChurch = ELam "n" (EApp (EApp (EVar "n") inc) (ENum 0))

--a) The substitution function should take 3 LCExpr (one of which will
--always be an Id) and give back one LCExpr. Because we are no longer
--using environments and closures, we just need terms that allow us to
--use the normal substitution rules defined in class (lecture 6 notes).
--We don't really need any new datatypes, we can just use LCExpr from
--earlier.

--b.
subst :: LCExpr -> LCExpr -> LCExpr -> LCExpr
subst (LCVar "i") e x =
    if (LCVar "i" == x)
    then e
    else LCVar "i"
subst (LCApp e1 e2) e x = LCApp (subst e1 e x) (subst e2 e x)
subst (LCLam x e') e n=
    if ((LCVar "x") ==  n)
    then LCLam "x" e'
    else LCLam "x" (subst e' e n)


--c.
evalSubst :: LCExpr -> LCExpr
evalSubst (LCVar "x") = LCVar "x"
evalSubst (LCLam "x" e) = LCLam "x" e
evalSubst (LCApp e1 e2) =
    case evalSubst e1 of
        (LCLam x e) -> evalSubst (subst e (evalSubst e2) e1)
        _ -> evalSubst (LCApp (evalSubst e1) e2)

--d. eval without sub is a real pain to read and to write, and is difficult
--to debug. The datatypes are more complex, when compared to eval with sub
--eval without sub will also break if there are any free variables,
--as it will be unable to find them in the map. eval with sub won't break,
--but it also won't give the right answer. This could be counted as a pro
--or a con. eval with sub may be slower, as it uses call by value, which is
--forced to evaluate all terms before plugging them back in, whereas
--eval without sub only realy needs to evaluate the expressions that
--are necessary to returning the answer.

--e. to get call by name you only really need to change one line in the code.
--In the following code:
--
-- case evalSubst e1 of
--        (LCLam x e) -> evalSubst (subst e (evalSubst e2) e1)
--        _ -> evalSubst (LCApp (evalSubst e1) e2)
--
--
--You simply replace (evalSubst e2) with e2. This way, you don't completely
--evaluate e2 unless you absolutely need to. This will prevent call-by-value,
--and allow e2 to remain unevaluated, and simply be referred to by it's name.
--The result is the following:
--
--case evalSubst e1 of
--        (LCLam x e) -> evalSubst (subst e e2 e1)
--        _ -> evalSubst (LCApp (evalSubst e1) e2)