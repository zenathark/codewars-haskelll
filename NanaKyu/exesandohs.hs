-- | Check to see if a string has the same amount of 'x's and 'o's. The method
-- | must return a boolean and be case insensitive. The string can contain any
-- | char.
-- |
-- | Examples input/output:
-- | XO("ooxx") => true
-- | XO("xooxx") => false
-- | XO("ooxXm") => true
-- | XO("zpzpzpp") => true // when no 'x' and 'o' is present should return true
-- | XO("zzoo") => false

module ExesAndOhs where

-- Libraries for Testing
import Text.Printf (printf)
import Data.Foldable
import Test.QuickCheck
import Test.Hspec

-- Required Libraries
import Data.List
import Data.Char (toLower)


countXandO characters =
  countXandO' characters 0
  where
    countXandO' [] counter = counter == 0
    countXandO' (char:nextChars) counter = case char of
        'o' -> countXandO' nextChars $ counter + 1
        'x' -> countXandO' nextChars $ counter - 1
        otherwise -> countXandO' nextChars counter

xo chars =
  countXandO lowCaseChars
  where
    lowCaseChars = map toLower chars

test input expected =
  it (printf "Testing xo(%s)" (show input)) $ shouldBe (xo input) expected

testSets =
  [("xo", True),
  ("Xo", True),
  ("xxOo", True),
  ("xxxm", False),
  ("Oo", False),
  ("ooom", False)]

main :: IO ()
main = hspec $ do
  describe "Exes and Ohs tests" $ do
    traverse_ (\(input, expected) -> (test input expected)) testSets
