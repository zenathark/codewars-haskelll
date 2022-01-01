
import Test.Hspec
import Text.Printf
import Data.Foldable (traverse_)

import Data.Char (ord)
import Data.Ord ()
import Data.List

letterScore :: Char -> Int
letterScore letter = ord letter - ord 'a' + 1

wordScore :: String -> Int
wordScore word = sum $ map letterScore word

data ScoredWord = ScoredWord 
    { label::String
    , score::Int
    , position::Int
    }

emptyScoredWord = ScoredWord "" 0 0
    
compareWords :: ScoredWord -> ScoredWord -> Ordering
compareWords ScoredWord{score=scoreA, position=positionA} ScoredWord{score=scoreB, position=positionB}
    | scoreA > scoreB = GT
    | scoreA == scoreB && positionA < positionB = GT
    | otherwise = LT

sortWords :: [ScoredWord] -> [ScoredWord]
sortWords = sortBy compareWords

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

toWords :: [String] -> [ScoredWord]
toWords allWords = map (\(index, word) -> ScoredWord word (wordScore word) index) (enumerate allWords)

headOrEmptyScoredWord :: [ScoredWord] -> ScoredWord
headOrEmptyScoredWord [] = emptyScoredWord
headOrEmptyScoredWord stringList = head stringList

high :: String -> String
high = label . headOrEmptyScoredWord . sortWords . toWords . words

test input expected = it (printf "Testing %s" input) $ shouldBe (high input) expected
testData = 
    [ ("", "")
    ]

main = hspec $  do
    traverse_ (uncurry test) testData