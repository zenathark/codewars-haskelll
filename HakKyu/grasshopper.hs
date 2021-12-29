-- | Write a program that finds the summation of every number from 1 to num.
-- | The number will always be a positive integer greater than 0.
-- |
-- | Example:
-- |
-- | summation(2) -> 3
-- | 1 + 2
-- |
-- | summation(8) -> 36
-- | 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8

module Main (main) where

import Test.Hspec
import Text.Printf (printf)
import Data.Foldable (traverse_)

summation :: Int -> Int
summation int = sum [1..int]

test input expected =
    it (printf "Testing %s" (show input)) $ shouldBe (summation input) expected

testInputs :: [(Int, Int)]
testInputs = 
    [ (1, 1)
    , (10, 55)
    ]

main :: IO ()
main = hspec $ do
    traverse_ (uncurry test) testInputs
