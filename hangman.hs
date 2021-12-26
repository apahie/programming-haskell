import System.IO

hangman :: IO ()
hangman = do
  putStrLn "Think of a word:"
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '_'
      xs <- sgetLine
      return (x : xs)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then putStrLn "You got it!!"
    else do
      putStrLn (match word guess)
      play word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]

-- ex6
readLine :: IO String
readLine = readLine' ""

readLine' :: String -> IO String
readLine' cs = do
  c <- getCh
  putChar c
  case c of
    '\DEL' ->
      if null cs
        then readLine' ""
        else do
          putStr "\b \b" -- スペースの分、\bが最後に必要
          readLine' (tail cs)
    '\n' -> do
      return (reverse cs)
    c -> readLine' (c : cs)
