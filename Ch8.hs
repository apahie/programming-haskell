module Ch8 where

import Ch7 (int2bin, rmdups)
import Data.Fixed (div')

type Pos = (Int, Int)

type Trans = Pos -> Pos

type Pair a = (a, a)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y -1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x -1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m : ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rect Float Float

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- newtype Nat = N Int

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n -1))

add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ (add m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- occurs :: Eq a => a -> Tree a -> Bool
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
-- occurs x (Node l y r) = x == y || occurs x l || occurs x r
occurs x (Node l y r)
  | x < y = occurs x l
  | x > y = occurs x r
  | otherwise = True

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l y r) = flatten l ++ [y] ++ flatten r

-- update for ex8
data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Imply Prop Prop
  | Equiv Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bbs ++ map (True :) bbs
  where
    bbs = bools (n -1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- update for ex9
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Mul x y) = value x * value y

type Cont = [Op]

data Op = EVAL Expr | ADD Int | MUL Int

eval' :: Expr -> Cont -> Int
eval' (Val n) c = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)
eval' (Mul x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MUL n : c) m = exec c (n * m)

value' :: Expr -> Int
value' e = eval' e []

-- ex1
mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult m (Succ n) = add m (mult m n)

-- ex2
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) = case compare x y of
  LT -> occurs' x l
  EQ -> True
  GT -> occurs' x r

-- ex3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

leaves :: Num p => Tree' a -> p
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- ex4
halve :: [a] -> ([a], [a])
-- halve xs = (take l xs, drop l xs)
halve xs = splitAt l xs
  where
    l = length xs `div` 2

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance ls) (balance rs)
  where
    (ls, rs) = halve xs

-- ex5
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g e = case e of
  (Val x) -> f x
  (Add e1 e2) -> g (folde f g e1) (folde f g e2)

-- ex6
eval'' :: Expr -> Int
eval'' = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- ex7
-- instance Eq a => Eq (Maybe a) where
--   Just a == Just b = a == b
--   Nothing == Nothing = True
--   _ == _ = False

-- instance Eq a => Eq [a] where
--   [] == [] = True
--   (x : xs) == (y : ys) = x == y && xs == ys
--   _ == _ = False
