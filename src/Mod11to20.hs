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
