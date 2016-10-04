{-# LANGUAGE TemplateHaskell #-}

module Mod21to30 where

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

return []
runTests = $quickCheckAll
