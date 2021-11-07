import Data.Char

positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

let2int c = ord c - ord 'a'

int2let n = chr (ord 'a' + n)

shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode n xs = [shift n x | x <- xs]

percent n m = (fromIntegral n / fromIntegral m) * 100

lowers xs = length [x | x <- xs, x >= 'a', x <= 'z']

count x xs = length [x' | x' <- xs, x == x']

freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']] where n = lowers xs

chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate n xs = drop n xs ++ take n xs

table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

table' = freqs "kdvnhoo lv ixq"

crack xs = encode (- factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

fac 0 = 1
fac n = product [1 .. n]