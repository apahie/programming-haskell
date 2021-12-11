-- ch9.2
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
valid Add x y = x <= y
valid Sub x y = x > y
-- valid Mul _ _ = True
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && x `mod` y == 0

-- ex5
valid' :: Op -> Int -> Int -> Bool
valid' Add _ _ = True
valid' Sub x y = True
valid' Mul _ _ = True
valid' Div x y = y /= 0 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- ch9.3
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- ex9.4
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
-- perms [] = [[]]
-- perms (x : xs) = concat (map (interleave x) (perms xs))
perms = foldr (concatMap . interleave) [[]]

choices :: [a] -> [[a]]
-- choices = concatMap perms . subs
-- ex1
choices xs = [zs | ys <- subs xs, zs <- perms ys]

-- ch9.5
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- ch9.6
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- ch9.7
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [ e
    | (ls, rs) <- split ns,
      l <- exprs ls,
      r <- exprs rs,
      e <- combine l r
  ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [ e
    | ns' <- choices ns,
      e <- exprs ns',
      eval e == [n]
  ]

-- ch9.7
main :: IO ()
main = print (solutions' ns 765)

-- ch9.8
type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res
    | (ls, rs) <- split ns,
      lx <- results ls,
      ry <- results rs,
      res <- combine' lx ry
  ]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [ (App o l r, apply o x y)
    | o <- ops,
      valid o x y
  ]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [ e
    | ns' <- choices ns,
      (e, m) <- results ns',
      m == n
  ]

-- ex2
removeone :: Eq a => a -> [a] -> [a]
removeone _ [] = []
removeone x (y : ys)
  | x == y = ys
  | otherwise = x : removeone x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x : xs) ys = elem x ys && isChoice xs (removeone x ys)

-- ex4
ns :: [Int]
ns = [1, 3, 7, 10, 25, 50]

es :: [Expr]
es = [e | ns' <- choices ns, e <- exprs ns']

es' :: [Expr]
es' = [e | ns' <- choices ns, e <- exprs ns', _ <- eval e]