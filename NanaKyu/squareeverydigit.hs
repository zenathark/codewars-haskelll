-- | Square Every Digit
-- | Welcome. In this kata, you are asked to square every digit of a number and concatenate them.
-- | or example, if we run 9119 through the function, 811181 will come out, because 92 is 81 and 12 is 1.
-- | ote: The function accepts an integer and returns an integer

module Main (main) where

import Test.Hspec
import Text.Printf
import Data.Foldable (traverse_)

import Control.Monad.ST
import Data.STRef
import Control.Monad

squareDigitST number = runST $ do
    squaredDigits <- newSTRef 0
    currentNumber <- newSTRef number

    loop currentNumber squaredDigits

    readSTRef squaredDigits
  where
    loop currentNumber squaredDigits = do
        currentN <- readSTRef currentNumber
        let (nextN, digit) = currentN `quotRem` 10
        let squaredD = digit * digit
        if squaredD < 10
        then modifySTRef squaredDigits (\x -> x * 10 + squaredD)
        else modifySTRef squaredDigits (\x -> x * 100 + squaredD)
        writeSTRef currentNumber nextN
        when (nextN > 0) (loop currentNumber squaredDigits)


squareDigit' 0 = 0
squareDigit' currentNumber =
    squareDigit' nextNumber * zeros + currentDigitSquared
  where
    (nextNumber, currentDigit) = currentNumber `quotRem` 10
    currentDigitSquared = currentDigit * currentDigit
    zeros = if currentDigitSquared < 10
            then 10
            else 100


squareDigit :: Int -> Int
squareDigit number
  | number < 0 = negate $ squareDigit $ negate number
  | otherwise = squareDigit' number

test input expected =
    it (printf "Testing %s" (show input)) $ squareDigitST input `shouldBe` expected

testData :: [(Int, Int)]
testData =
    [ (9119, 811181)
    , (-1, -1)
    , (0, 0)
    ]

main = hspec $ do
    traverse_ (uncurry test) testData