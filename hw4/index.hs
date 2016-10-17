-- ex1
fun1 :: [Integer] -> Integer
fun1 = foldl (\acc x -> if even x then (x-2) * acc else acc) 1

fun2 :: Integer -> Integer
fun2 = sum 
    .filter even
    .takeWhile (/= 1)
    .iterate (\n -> if even n then (div n 2) else 3*n+1)
-- ex2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show,Eq)

-- ex3
xor :: [Bool] -> Bool
xor = foldl (\acc x -> if (x == True) then (not acc) else acc) False 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

 
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> (f acc x)) base xs

-- ex4
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
diff :: Eq a => [a] -> [a] -> [a]
diff xs ys = filter (\x -> not $ elem x ys) xs

test n = map (\x -> 2*x+1) (diff [1..n] sieve) where
    sieve = map (\(i,j) -> i+j+2*i*j) . filter (\(x,y) -> x+y + x*y <=n) $ cartProd [1..n] [1..n]