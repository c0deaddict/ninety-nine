{-# LANGUAGE TemplateHaskell #-}

module Mod11to20 where

import           Control.Arrow   (second)
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
encodeDirect = foldr reduce []
  where
    reduce x (Single y:ys)     | y == x = Multiple 2 y : ys
    reduce x (Multiple n y:ys) | y == x = Multiple (n + 1) y : ys
    reduce x ys                = Single x : ys

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


--
-- Problem 17
--
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n | n <= 0 = ([], xs)
split (x:xs) n = (x : prefix, suffix)
  where (prefix, suffix) = split xs (n - 1)

prop_split :: [a] -> Property
prop_split xs = forAll (choose (0, length xs)) $ \n ->
  let (prefix, suffix) = split xs n
  in length xs == length prefix + length suffix

--
-- Problem 18: Extract a slice from a list.
--
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits
-- included). Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _     = []
slice _ i k      | k <= 0 || k < i = []
slice (x:xs) 1 k = x : slice xs 1 (k - 1)
slice (x:xs) i k = slice xs (i - 1) (k - 1)

prop_slice :: [a] -> Property
prop_slice xs = not (null xs) ==>
  forAll (choose (1, length xs)) $ \i ->
    forAll (choose (i, length xs)) $ \k ->
      length (slice xs i k) == (k - i) + 1


--
-- Problem 19: Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate [] _     = []
rotate xs 0     = xs
rotate (x:xs) n | n > 0 = rotate (xs ++ [x]) (n - 1)
rotate xs n     | n < 0 = reverse (rotate (reverse xs) (-n))

prop_rotate :: Eq a => [a] -> Int -> Bool
prop_rotate xs n = rotate (rotate xs n) (-n) == xs


--
-- Problem 20: Remove the K'th element from a list.
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []     = (Nothing, [])
removeAt i xs     | i < 1 = (Nothing, xs)
removeAt 1 (x:xs) = (Just x, xs)
removeAt i (x:xs) = fmap (second ((:) x)) (removeAt (i - 1)) xs

prop_removeAt :: Eq a => [a] -> Property
prop_removeAt xs = forAll (choose (0, length xs + 1)) $ \i ->
  if i <= 0 || i > length xs then
    -- When index is out bounds removeAt must return Nothing
    -- The result list ys should be the same as the list xs
    case removeAt i xs of
      (Nothing, ys) -> xs == ys
      _             -> False
  else
    -- Index not out of bounds
    -- removeAt must return a element, the element should exists in xs
    -- and the length of the new list ys must be one smaller than that of xs
    case removeAt i xs of
      (Just y, ys) -> y `elem` xs && length ys == length xs - 1
      _            -> False

return []
runTests = $quickCheckAll
