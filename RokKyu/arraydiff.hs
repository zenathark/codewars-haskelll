-- Your goal in this kata is to implement a difference function, which subtracts one list from another and returns the result.

-- It should remove all values from list a, which are present in list b keeping their order.

-- difference [1,2] [1] == [2]
-- If a value is present in b, all of its occurrences must be removed from the other:

-- difference [1,2,2,2,3] [2] == [1,3]

module Main (main) where

import Test.Hspec
import Text.Printf
import Data.Foldable (traverse_)

import Data.List

difference :: Eq a => [a] -> [a] -> [a]
difference a b = difference' a b []
  where
    difference' [] b result = reverse result
    difference' (a:as) b result = 
        case a `elemIndex` b of
            Nothing -> difference' as b (a:result)
            Just _ -> difference' as b result

testData :: [(([Int], [Int]), [Int])]
testData = 
    [ (([1, 2, 3], [2]), [1, 3])
    , (([1, 2, 3], [1, 2]), [3])
    , (([1, 2, 2, 3], [2]) , [1, 3])
    ]

test (inputA, inputB) expected =
    it (printf "Testing %s - %s" (show inputA) (show inputB)) $ difference inputA inputB `shouldBe` expected

main = hspec $ do
    traverse_ (uncurry test) testData