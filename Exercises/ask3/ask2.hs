import Test.QuickCheck

data Tree a = T a [Tree a]
  deriving Show

--application of fold in a Tree
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (T a []) = f a []
foldTree f (T a ts) = f a map ((foldTree f) ts)

--calculates the number of nodes of Tree t
sizeTree :: Num b => Tree a -> b
sizeTree t = foldTree count t
  where count a [] = 1
        count a as = 1 + sum as
--sizeTree T a [] = 1
--sizeTree T a ts = 1 + sizeTree ts

--just a function which calculates the maximum element of a list
maxl []   = 0
maxl x:xs = if x >= y then x else y
  where y = maxl xs

--calculates the depth of Tree t
heightTree :: (Ord b, Num b) => Tree a -> b
heightTree t = foldTree addmax t
  where addmax a [] = 1
        addmax a as = 1 + maxl as

--calculates the sum of nodes of Tree t
sumTree :: Num a => Tree a -> a
sumTree t = foldTree addT t
  where addT a [] = a
        addT a as = a + sum as

--calculate the maximum element of Tree t
maxTree :: Ord a => Tree a -> a
maxTree t = foldTree maxt t
  where maxt a [] = a
        maxt a as = if a >= y then a else y
          where y = maxl as

--checks if value x exists in a node of Tree t
inTree :: Eq a => a -> Tree t -> Bool
inTree x t = foldTree check t
  where check a [] = if a==x then True else False
        check a as = if a==x then True else any (x==) as


--returns the list of all nodes of Tree t
nodes :: Tree a -> [a]
nodes t = foldTree fuse t
  where fuse a [] = [a]
        fuse a as = a : (concat as)

--calculates the number of nodes of Tree t that satisfy f
countTree :: (a -> Bool) -> Tree a -> Integer
countTree f t = foldTree sat t
  where sat a [] = if (f a) then 1 else 0
        sat a as | f a       = 1 + sum as
                 | otherwise = sum as

--returns a list with the values of leaves of Tree t
leaves :: Tree a -> [a]
leaves t = foldTree feuille t
  where feuille a [] = [a]
        feuille a as = concat as

--returns the Tree which occurs if we replace each value x of Tree t with f x
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f t = foldTree replace t
  where replace a [] = T (f a) []
        replace a as = T (f a) as

--2.3
--trims Tree at height n (erases all nodes with distance >= n from root)
trimTree :: Int -> Tree a -> Tree a
trimTree 1 (T a t)    = T a []
trimTree n (T a [])   = T a []
trimTree n (T a h:ts) = T a ((trimTree n-1 h) : (cut n-1 ts))
  where cut n []     = []
        cut n (x:xs) = (trimTree n x) : (cut n xs)


--returns the node of the Tree specified by path in list l
path :: [Int] -> Tree a -> a
path [] (T a ts)     = a
path (x:xs) (T a ts) = path xs (ts !! x) --(choose x ts)
  --where choose 0 (h:hs) = h
  --      choose n (h:hs) = choose n-1 hs
