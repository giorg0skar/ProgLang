import Test.QuickCheck

data Tree a = T a [Tree a]
  deriving Show

--3.
--3.1
instance Arbitrary a => Arbitrary (Tree a)
where arbitrary = sized tree'
tree' 0 = T a []
tree' n | n > 0 = T a (listof (tree' n `div` 2))

--3.2
prop_height :: Tree Int -> Bool
prop_height t = h > 0 && h <= (sizeTree t)
where h = heightTree t

prop_max :: Tree Int -> Bool
prop_max t = inTree (maxTree t) t

prop_nodes :: Tree Int -> Bool
prop_nodes t = exist t (nodes t)
where exist t []     = True
      exist t (x:xs) = inTree x t && exist t xs

prop_satisfy :: (Int -> Bool) -> Tree Int -> Bool
prop_satisfy f t = s > 0 && s <= (sizeTree t)
where s = countTree f t

prop_count :: Tree Int -> Bool
prop_count t = (n == sizeTree t) && (l < n || (n==1 && l==1))
where n = length (nodes t)
      l = length (leaves t)

--checks if mapTree preserves height and size of Tree t
prop_mapTree :: (a -> b) -> Tree a -> Bool
prop_mapTree f t = (heightTree t == heightTree (mapTree f t)) && (sizeTree t == sizeTree (mapTree f t))

--if value n exists in Tree t, then f n must exist in mapTree f t
prop_existmapTree :: Int -> (a -> b) -> Tree Int -> Bool
prop_existmapTree n f t = inTree n t ==> inTree (f n) (mapTree f t)

--3.3
bird :: Tree Rational
bird = T 1 ((mapTree (\x -> 1/(x+1)) bird) : [mapTree (\x -> (1/x)+1) bird])

--3.4
prop_samepath :: [Int] -> Int -> Bool
prop_samepath l n = (length l == n) && (path l bird == path l (trimTree n bird))

prop_allnumbers

prop_fib :: Int -> Bool
prop_fib n = fib n == path (replicate n 0) bird


--main = do
--  putStrLn "checking..."
--  quickCheck prop_count