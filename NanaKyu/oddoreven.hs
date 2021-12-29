-- | Odd or Even?
-- | Given a list of integers, determine whether the sum of its elements is odd or even.
-- | 
-- | Give your answer as a string matching "odd" or "even".
-- | 
-- | If the input array is empty consider it as: [0] (array with a zero).
-- |
-- | Examples: 
-- |
-- | Input: [0]
-- | Output: "even"
-- |
-- | Input: [0, 1, 4]
-- | Output: "odd"
-- |
-- | Input: [0, -1, -5]
-- | Output: "even"

module Main (main) where

import Test.Hspec
import Text.Printf (printf)
import Data.Foldable (traverse_)

oddOrEven :: (Integral a, Foldable t) => t a -> String
oddOrEven numbers = 
    case sum numbers `mod` 2 of
        0 -> "even"
        _ -> "odd"

test input expected =
    it (printf "Test with %s" (show input)) $ shouldBe (oddOrEven input) expected

testInputs :: [([Integer], String)]
testInputs =
    [ ([1], "odd")
    , ([-1], "odd")
    , ([1, -2], "odd")
    , ([2, 5, 34, 6], "odd")
    , ([0], "even")
    , ([2], "even")
    , ([-2], "even")
    , ([], "even") ]
    

main = hspec $ do
    traverse_ (uncurry test) testInputs