module Isograms where

import Test.QuickCheck
import Test.Hspec

import Data.Char
import Data.List

isIsogram' :: String -> String -> Bool
isIsogram' [] characterSet = True
isIsogram' (nextCharacter:restCharacters) characterSet
  | any (nextCharLowerCase==) characterSet = False
  | otherwise                          = isIsogram' restCharacters (nextCharLowerCase:characterSet)
  where
    nextCharLowerCase = toLower nextCharacter


isIsogram :: String -> Bool
isIsogram s = isIsogram' s []

main :: IO ()
main = hspec $ do
  describe "isIsogram" $ do
    it "testing Dermatoglyphics" $ shouldBe (isIsogram "Dermatoglyphics") True
    it "testing 'moose'" $ shouldBe (isIsogram "moose") False
    it "testing 'moOse'" $ shouldBe (isIsogram "moOse") False
    it "testing aba" $ shouldBe (isIsogram "aba") False
