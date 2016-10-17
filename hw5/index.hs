module Homework where
import ExprT     
import Parser    
import StackVM
import Data.Maybe
import qualified Data.Map as M
-- ex1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (Mul expr1 expr2) = (eval expr1) * (eval expr2)

-- ex2                         
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-- ex3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a 

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id    

-- ex4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x
        | x<=0 = False
        | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving(Eq,Show)
instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax(max x y)
    mul (MinMax x) (MinMax y) = MinMax(min x y)


newtype Mod7 = Mod7 Integer deriving(Eq,Show)

instance Expr Mod7 where 
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) =  Mod7 ((x+y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7((x*y) `mod` 7)

-- tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp


-- ex6 todo
