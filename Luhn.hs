luhnDouble :: (Ord p, Num p) => p -> p
luhnDouble n = if n' > 9 then n' - 9 else n'
  where
    n' = n * 2

luhn :: Integral a => a -> a -> a -> a -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0