{-# OPTIONS_GHC -O2 -optc-O2 #-}

import Data.Char (isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

increase x upper m | m >= upper-x = 1
                   | otherwise    = if ((upper-x) `mod` m) == 0 then ((upper-x) `div` m) else ((upper-x) `div` m) + 1

decide [s1] (a:as) m = increase s1 a m + solve as [a] m
decide (s1:s2:st) (a:as) m | s2 <= a   = increase s1 s2 m + solve (a:as) (s2:st) m
					       | otherwise = increase s1 a m  + solve as (a:s2:st) m

-- when we have finished traversing our list, we count the amplifiers for the elements in the stack
fin [s1] m = 0
fin (s1:s2:st) m = increase s1 s2 m + fin (s2:st) m

solve [] (s:st) m = fin (s:st) m
solve (a:as) [] m  = solve as [a] m
solve (a:as) (s:st) m | a < s     = solve as (a:s:st) m
				      | a > s     = decide (s:st) (a:as) m
				      | otherwise = solve as (s:st) m

fun [] m = 0
fun list m = solve list [] m

main = do
    all <- BS.getContents
    let Just (n, r1) = readInt all
    let Just (m, r2) = readInt r1
    let (l,_) = readMany readInt r2
	--text <- getContents
	--let (n1 : m1 : rest) = words text
	--let n = read n1
	--(n <- read n1) :: IO int
	--let m = read m1
	--let l = map read rest
    let num = fun l m
    print (num :: Int)
	where 
          readInt s = BSC.readInt (BSC.dropWhile isSpace s)
		--readInteger s = BSC.readInteger (BSC.dropWhile isSpace s)
	  readMany readf s = case readf s of
		                  Just (x, r) -> let (xs, t) = readMany readf r
						in  (x : xs, t)
		                  Nothing     -> ([], s)
