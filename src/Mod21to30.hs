{-# LANGUAGE TemplateHaskell #-}

module Mod21to30 where

import           Mod11to20
import           Mod1to10

import           Data.List       hiding (group)
import           System.Random
import           Test.QuickCheck

--
-- Problem 21: Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt e [] _     = [e]
insertAt e xs i     | i <= 1 = e : xs
insertAt e (x:xs) i = x : insertAt e xs (i - 1)

prop_insertAt :: Eq a => a -> [a] -> Property
prop_insertAt e xs = forAll (choose (0, length xs + 1)) $ \i ->
  let ys = insertAt e xs i in
    length ys == length xs + 1 &&
    e `elem` ys


--
-- Problem 22: Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range i k | k < i     = []
          | i == k    = [i]
          | otherwise = i : range (i + 1) k

prop_range :: Int -> Property
prop_range i = forAll (choose (i, i + 10000)) $ \k ->
  let r = range i k in
    length r == k - i + 1 &&
    head r == i &&
    last r == k


--
-- Problem 23: Extract a given number of randomly selected elements from a list.
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
  seed <- newStdGen
  return $ rndSelect' xs seed n
  where
    rndSelectOne :: RandomGen g => [a] -> g -> (a, g)
    rndSelectOne xs gen = (xs `elementAt` i, gen')
      where (i, gen') = randomR (1, length xs) gen

    rndSelect' xs gen n
      | n <= 0 = []
      | otherwise = x : rndSelect' xs gen' (n - 1)
        where (x, gen') = rndSelectOne xs gen


--
-- Problem 24: Lotto: Draw N different random numbers from the set 1..M.
diffSelect :: RandomGen g => Int -> Int -> g -> [Int]
diffSelect n m _ | n <= 0 || m < 1 = []
diffSelect n m gen = diffSelect' n (range 1 m) gen
  where
    diffSelect' n _ _ | n <= 0 = []
    diffSelect' _ [] _ = []
    diffSelect' n xs gen = y : diffSelect' (n - 1) ys gen'
      where
        (i, gen') = randomR (1, length xs) gen
        (Just y, ys) = removeAt i xs

diffSelectIO :: Int -> Int -> IO [Int]
diffSelectIO n m = do
  seed <- newStdGen
  return $ diffSelect n m seed

prop_diffSelect :: Int -> Int -> Int -> Bool
prop_diffSelect n m seed =
  let xs = diffSelect n m (mkStdGen seed) in
    nub xs == xs && -- all elements must be unique
    length xs == min (max n 0) (max m 0)


--
-- Problem 25: Generate a random permutation of the elements of a list.
rndPermu :: RandomGen g => [a] -> g -> [a]
rndPermu [] _ = []
rndPermu [x] _ = [x]
rndPermu xs gen = y : rndPermu ys gen'
  where
    (i, gen') = randomR (1, length xs) gen
    (Just y, ys) = removeAt i xs

rndPermuIO :: [a] -> IO [a]
rndPermuIO xs = do
  seed <- newStdGen
  return $ rndPermu xs seed

prop_rndPermu :: Ord a => [a] -> Int -> Bool
prop_rndPermu xs seed =
  let ys = rndPermu xs (mkStdGen seed) in
    sort xs == sort ys

--
-- Problem 26
--
-- Generate the combinations of K distinct objects chosen from the N elements
-- of a list
--
-- In how many ways can a committee of 3 be chosen from a group of 12 people?
-- We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
-- well-known binomial coefficients). For pure mathematicians, this result may
-- be great. But we want to really generate all the possibilities in a list.
combinations :: Eq a => Int -> [a] -> [[a]]
combinations k _ | k <= 0 = []
combinations k xs = combinations' [] k xs where
  combinations' acc 0 _  = [reverse acc]
  combinations' acc k xs = concatMap comb xs where
    comb x = combinations' (x : acc) (k - 1) $ filter (/= x) xs

prop_combinations :: Int -> Property
prop_combinations seed = forAll (choose (0, upper)) $ \n ->
  let xs = diffSelect n upper (mkStdGen seed) in
    forAll (choose (1, 5)) $ \k ->
      countCombs n k == length (combinations k xs)
  where
    upper = 12

countCombs n k
  | k < 0     = 0
  | n < k     = 0
  | k == 0    = 1
  | otherwise = n * countCombs (n - 1) (k - 1)


--
-- Problem 27: Group the elements of a set into disjoint subsets.
--
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
--    2, 3 and 4 persons? Write a function that generates all the possibilities
--    and returns them in a list.
--
-- b) Generalize the above predicate in a way that we can specify a list of
--    group sizes and the predicate will return a list of groups
group :: Eq a => [Int] -> [a] -> [[[a]]]
group [] _ = []
group _ [] = []
group [k] members = map (:[]) $ ordCombs k members
group (k:xs) members = do
  comb <- ordCombs k members
  rest <- group xs (filter (`notElem` comb) members)
  return (comb : rest)

ordCombs :: Int -> [a] -> [[a]]
ordCombs k _      | k <= 0 = []
ordCombs _ []     = []
ordCombs 1 xs     = map (: []) xs
ordCombs k (x:xs) = ((:) x <$> tailsOfLength (k - 1) xs) ++ ordCombs k xs
  where
    tailsOfLength n xs = filter (minLength n) $ take n <$> tails xs
    minLength n xs = length xs >= n

--
-- Problem 28: Sorting a list of lists according to length of sublists
--
-- a) We suppose that a list contains elements that are lists themselves. The
--    objective is to sort the elements of this list according to their length.
--    E.g. short lists first, longer lists later, or vice versa.
lsort :: [[a]] -> [[a]]
lsort = sortBy (\a b -> compare (length a) (length b))

-- b) Again, we suppose that a list contains elements that are lists themselves.
--    But this time the objective is to sort the elements of this list according
--    to their length frequency; i.e., in the default, where sorting is done
--    ascendingly, lists with rare lengths are placed first, others with a more
--    frequent length come later.
lfsort :: [[a]] -> [[a]]
lfsort xs = sortBy (\a b -> compare (freq a) (freq b)) xs where
  freq x = length $ filter (length x ==) (length <$> xs)


return []
runTests = $quickCheckAll
