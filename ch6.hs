-- ex1
fac :: Int -> Int
fac 0 = 1
fac n
  | n > 0 = n * fac (n -1)
  | otherwise = error "negative number!"

-- ex2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n
  | n > 0 = n + sumdown (n - 1)
  | otherwise = error "negative number!"

-- ex3
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m Main.^ (n - 1))

-- ex4
euclid :: Int -> Int -> Int
euclid m n
  | m == n = n
  | m > n = euclid (m - n) n
  | otherwise = euclid m (n - m)

-- ex6
-- a
and1 :: [Bool] -> Bool
and1 [] = True
and1 (b : bs) = b && and1 bs

-- b
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (xs : xss) = xs ++ concat1 xss

-- c
replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n -1) x

-- d
(!!) :: [a] -> Int -> a
[] !! _ = error "empty list!!"
(x : _) !! 0 = x
(_ : xs) !! n = xs Main.!! (n -1)

-- e
elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 x (y : ys) = x == y || elem1 x ys

-- ex7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x > y = y : merge (x : xs) ys
  | otherwise = x : merge xs (y : ys)

-- ex8
halve :: [a] -> ([a], [a])
halve xs = splitAt s xs
  where
    s = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = halve xs

-- ex9
-- a
sum1 :: Integral a => [a] -> a
sum1 [] = 0
sum1 (x : xs) = x + sum1 xs

-- b
take1 :: Int -> [a] -> [a]
take1 _ [] = []
take1 0 _ = []
take1 n (x : xs) = x : take1 (n - 1) xs

-- c
last1 :: [a] -> a
last1 [] = error "arg is empty list!!"
last1 [x] = x
last1 (x : xs) = last1 xs