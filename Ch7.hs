module Ch7 where

import Data.Char (chr, ord)
import Data.List (sort)

type Bit = Int

bin2int :: [Bit] -> Int
-- bin2int bits = sum [w * b | (w, b) <- zip weights bits]
--   where
--     weights = iterate (* 2) 1
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
-- ord function converts char to code (ex. 'a' -> 97)
encode = concatMap (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- vote algorithm
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- another vote algorithm
ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [] -> error "empty!!"
  [c] -> c
  (c : cs) -> winner' (elim c bs)

-- ex2
-- a
all :: (a -> Bool) -> [a] -> Bool
-- all _ [] = True
-- all f (x : xs) = f x && Ch7.all f xs
-- all f = foldr ((&&) . f) True
all p = and . map p

-- b
any :: (a -> Bool) -> [a] -> Bool
-- any f = foldr ((||) . f) False
any p = or . map p

-- c
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : Ch7.takeWhile p xs
  | otherwise = []

-- d
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x = Ch7.dropWhile p xs
  | otherwise = x : xs

-- ex3
-- map' f = foldr (\x xs -> f x : xs) []
map' f = foldr ((:) . f) []

filter' p = foldr (\x xs -> if p x then x : xs else xs) []

-- ex4
dec2int :: [Int] -> Int
dec2int = foldl ((+) . (* 10)) 0

-- ex5
-- Is type reversed? I will review again.
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- ex6
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' f = unfold null (f . head) tail

-- iterate f i = id f i : iterate f (f i)
iterate' = unfold (const False) id

-- ex7
parity = (`mod` 2) . sum

addParityBit :: [Bit] -> [Bit]
addParityBit xs = xs ++ [parity xs]

send :: String -> [Bit]
send = addParityBit . encode

validateParityBit :: [Bit] -> Bool
validateParityBit xs = parity (init xs) == last xs

recieve :: [Bit] -> String
recieve xs
  | validateParityBit xs = decode (init xs)
  | otherwise = error "Parity bit is invalid!!"

-- ex8
bitLossChannel :: [Bit] -> [Bit]
bitLossChannel = tail

transmit' :: String -> String
transmit' = recieve . channel . send

bitLossTransmit :: String -> String
bitLossTransmit = recieve . bitLossChannel . send

-- ex9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

-- ex10
luhnDouble :: (Ord p, Num p) => p -> p
luhnDouble n = if n' > 9 then n' - 9 else n'
  where
    n' = n * 2

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0