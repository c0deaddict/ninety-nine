{-# LANGUAGE TemplateHaskell #-}

module Mod1to10 where

import           Test.QuickCheck

--
-- Problem 1: Find the last element of a list.
myLast :: [a] -> a
myLast []     = error "No end for empty lists"
myLast [x]    = x
myLast (_:xs) = myLast xs

prop_myLast :: Eq a => [a] -> Property
prop_myLast xs = not (null xs) ==> myLast xs == last xs

--
-- Problem 2: Find the last but one element of a list.
myButLast :: [a] -> a
myButLast []     = error "No last but one for empty lists"
myButLast [x]    = error "No last but one for one items lists"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

prop_myButLast :: Eq a => [a] -> Property
prop_myButLast xs = not (null xs) && not (null (tail xs)) ==>
  myButLast xs == (head . tail . reverse) xs

--
-- Problem 3
--
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Index out of bounds"
elementAt (x:xs) i
  | i <= 0 = error "Index out of bounds"
  | otherwise = elementAt xs (i - 1)

prop_elementAt :: Eq a => [a] -> Property
prop_elementAt xs = not (null xs) ==>
  forAll (choose (1, length xs)) $ \i ->
    elementAt xs i == head (drop (i - 1) xs)

--
-- Problem 4: Find the number of elements of a list.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

prop_myLength :: [a] -> Bool
prop_myLength xs = myLength xs == length xs

--
-- Problem 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]
-- myReverse = foldl (flip (:)) []

prop_myReverse :: Eq a => [a] -> Bool
prop_myReverse xs = myReverse xs == reverse xs

prop_myReverseAlt :: Eq a => [a] -> Bool
prop_myReverseAlt xs = myReverse xs == foldl (flip (:)) [] xs

--
-- Problem 6
--
-- Find out whether a list is a palindrome. A palindrome can be
-- read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

--
-- Problem 7: Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a] deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NestedList a) where
  arbitrary = oneof
    [ Elem <$> arbitrary
    , List <$> listOf (oneof
      [ Elem <$> arbitrary
      , List <$> listOf (Elem <$> arbitrary)
      ])
    ]

flatten :: NestedList a -> [a]
flatten (Elem v)    = [v]
-- flatten (List list) = concatMap flatten list
flatten (List list) = foldl (\acc a -> acc ++ flatten a) [] list

prop_flatten :: Eq a => NestedList a -> Bool
prop_flatten list = length (flatten list) == nestedLength list
  where
    nestedLength :: NestedList a -> Int
    nestedLength (Elem v)    = 1
    nestedLength (List list) = sum $ map nestedLength list


-- Problem 8
--
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element. The order of the elements should not be changed.
compress :: Eq a => [a] -> [a]
compress (x:xs@(y:ys))
  | x == y = compress xs
  | otherwise = x : compress xs
compress xs = xs
-- most elegant:
-- import Data.List
-- compress = map head . group

--
-- Problem 9
--
-- Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack list@(x:_) = prefix : pack suffix
  where (prefix, suffix) = span (== x) list

--
-- Problem 10
--
-- Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode = map rle . pack
  where rle prefix@(x:_) = (length prefix, x)

return []
runTests = $quickCheckAll
