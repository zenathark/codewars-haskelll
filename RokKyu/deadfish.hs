-- Write a simple parser that will parse and run Deadfish.

-- Deadfish has 4 commands, each 1 character long:

-- i increments the value (initially 0)
-- d decrements the value
-- s squares the value
-- o outputs the value into the return array
-- Invalid characters should be ignored.

-- parse "iiisdoso" -> [ 8, 64 ]

module Main (main) where

import Test.Hspec
import Text.Printf
import Data.Foldable (traverse_)

parseCommand 'i' lastState history = (lastState + 1, history)
parseCommand 's' lastState history = (lastState * lastState, history)
parseCommand 'd' lastState history = (lastState - 1, history)
parseCommand 'o' lastState history = (lastState, lastState:history)
parseCommand _ lastState history = (lastState, history)


parse :: String -> [Int]
parse input = parse' input (0, [])
  where
    parse' :: String -> (Int, [Int]) -> [Int]
    parse' [] (state, history) = reverse history
    parse' (command:xs) (state, history) = parse' xs $ parseCommand command state history

testData =
    [ ("i", [])
    , ("s", [])
    , ("d", [])
    , ("io", [1])
    , ("so", [0])
    , ("do", [-1])
    , ("o", [0])
    , ("iio", [2])
    , ("iiso", [4])
    , ("iiisdoso", [8, 64])
    , ("P", [])
    ]

test input expected =
    it (printf "Testing %s" input) $ parse input `shouldBe` expected

main = hspec $ do
    traverse_ (uncurry test) testData

