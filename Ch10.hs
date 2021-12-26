act :: IO (Char, Char)
act = do
  x <- getChar
  getChar
  y <- getChar
  return (x, y)

getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getLine'
      return (x : xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = do
  putChar x
  putStr xs

putStrLn' :: String -> IO ()
putStrLn' xs = do
  putStr xs
  putChar '\n'

strlen :: IO ()
strlen = do
  putStr' "Enter a String:"
  xs <- getLine'
  putStr' "The String has "
  putStr' (show (length xs))
  putStrLn' " characters"

-- ex1
putStr'' :: String -> IO ()
putStr'' cs = sequence_ [putChar c | c <- cs]

-- ex4
-- adder :: IO ()
-- adder = do
--   putStr "How many number? "
--   n <- getLine
--   adder' 0 (read n)

-- adder' :: Int -> Int -> IO ()
-- adder' a 0 = print a
-- adder' a n = do
--   s <- getLine
--   adder' (a + read s) (n - 1)

-- ex5
adder :: IO ()
adder = do
  putStr "How many number? "
  n <- getLine
  ns <- sequence [getInt | _ <- [0 .. (read n)]]
  print (sum ns)

getInt :: IO Int
getInt = do
  n <- getLine
  return (read n)
