{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
-- ex1
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
fibs :: [Integer]
fibs = fmap fib [0..]

-- ex2

fib2' :: (Integer,Integer,Integer) -> Integer
fib2' (a,b,0) = a
fib2' (a,b,1) = a
fib2' (a,b,n) = fib2' (a+b,a,n-1)

fib2 :: Integer -> Integer 
fib2 n = fib2' (1,1,n)

fibs2 :: [Integer]
fibs2 = fmap fib2 [0..]

-- ex3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where 
    show = show .take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons y c) = y : streamToList c

-- ex4
streamRepeat :: a -> Stream a 
streamRepeat y = Cons y (streamRepeat y)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y ys) = Cons (f y) (streamMap f ys)

streamFromSeed :: (a->a) -> a -> Stream a
streamFromSeed f y = Cons y (streamFromSeed f (f y))

-- ex5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons y ys) zs = Cons y (interleaveStreams zs ys)

merge :: Stream a -> Stream a -> Stream a
merge (Cons y ys) zs = Cons y (merge zs ys)
s1 = streamFromSeed id 0
s2 = streamFromSeed id 1
s3 = streamFromSeed (+1) 2
ruler = merge s1 $ merge s2 s3

-- ex6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons y ys) = Cons (-y) (negate ys)
    -- | A*B = a0*b0+x*(a0*B' + A' * B)
    (+) (Cons y ys) (Cons z zs) = Cons (y+z) (ys + zs)
    (*) (Cons y ys) s@(Cons z zs) = Cons (y*z) (streamMap (*y) zs + (ys*s))

instance Fractional (Stream Integer) where
    (/) (Cons y ys) (Cons z zs) = q
        where q = Cons (y `div` z) (streamMap (`div` z) (ys - q * zs))

fibs10 :: Stream Integer
fibs10 = x / (1 - x - x * x)


-- ex7
data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
      (Matrix (a11*b11+a12*b21) (a11*b12+a12*b22)
              (a21*b11+a22*b21) (a21*b12+a22*b22))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = getA11 (f^(n-1))
  where f = Matrix 1 1 1 0

getA11 :: Matrix -> Integer
getA11 (Matrix a11 _ _ _) = a11

