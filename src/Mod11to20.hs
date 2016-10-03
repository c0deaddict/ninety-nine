module Mod11to20 where

import           Mod1to10
import           Test.QuickCheck

--
-- Problem 11: Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.
data ModifiedRle a = Multiple Int a | Single a deriving (Eq, Show)
encodeModified :: Eq a => [a] -> [ModifiedRle a]
encodeModified = map modified . encode
  where
    modified (1, x) = Single x
    modified (n, x) = Multiple n x

--
-- Problem 12: Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct
-- its uncompressed version.
decodeModified :: Eq a => [ModifiedRle a] -> [a]
decodeModified = concatMap unmodified
  where
    unmodified (Single x)     = [x]
    unmodified (Multiple n x) = replicate n x

prop_encodeModified :: Eq a => [a] -> Bool
prop_encodeModified xs = decodeModified (encodeModified xs) == xs

--
-- Problem 13: Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in
-- problem 9, but only count them. As in problem P11, simplify the result list
-- by replacing the singleton lists (1 X) by X.
encodeDirect :: Eq a => [a] -> [ModifiedRle a]
encodeDirect = foldl reduce []
  where
    reduce (Single y:ys) x     | y == x = Multiple 2 y : ys
    reduce (Multiple n y:ys) x | y == x = Multiple (n + 1) y : ys
    reduce ys x                = Single x : ys

prop_encodeDirect :: Eq a => [a] -> Bool
prop_encodeDirect xs = encodeDirect xs == encodeModified xs

--
-- Problem 14: Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

prop_dupli :: [a] -> Bool
prop_dupli xs = length (dupli xs) == 2 * length xs

--
-- Problem 15: Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli _ 0 = []
repli [] _ = []
repli xs 1 = xs
repli (x:xs) n = rept x n ++ repli xs n
  where
    rept x 0 = []
    rept x n = x : rept x (n - 1)

prop_repli :: [a] -> Property
prop_repli xs =
  forAll (choose (1, 10)) $ \n ->
    length (repli xs n) == n * length xs

--
-- Problem 16: Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = chunksOf n xs >>= take (n - 1)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf 0 xs = error "Zero is not a size"
chunksOf n xs = prefix : chunksOf n suffix
  where (prefix, suffix) = splitAt n xs

prop_dropEvery :: [a] -> Property
prop_dropEvery xs = not (null xs) ==>
  forAll (choose (1, length xs)) $ \n ->
    length (dropEvery xs n) == length xs - length xs `div` n
