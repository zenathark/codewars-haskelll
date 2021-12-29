-- | Abbreviate a Two Word Name
-- | Write a function to convert a name into initials. This kata strictly takes two
-- | words with one space in between them.
-- | 
-- | The output should be two capital letters with a dot separating them.
-- | 
-- | It should look like this:
-- | 
-- | Sam Harris => S.H
-- | patrick feeney => P.F

module Main (main) where

import Test.Hspec
import Data.Foldable (traverse_)
import Text.Printf (printf)

import Data.Char (toUpper)

getInitials :: String -> String
getInitials fullName = printf "%c.%c" firstInitial lastInitial
    where
        firstInitial = toUpper $ head firstName
        lastInitial = toUpper $ head lastName
        (firstName, lastName) = getNames $ words fullName 
        getNames [firstName, lastName] = (firstName, lastName)
        getNames _ = error "Not enough names"

test input expected =
    it (printf "Testing %s" (show input)) $ shouldBe (getInitials input) expected


testInputs :: [(String, String)]
testInputs = 
    [ ("Henry Ford", "H.F")
    , ("donald trump", "D.T")
    ]

main :: IO ()
main = hspec $ do
    traverse_ (uncurry test) testInputs
