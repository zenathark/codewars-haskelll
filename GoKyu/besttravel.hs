-- John and Mary want to travel between a few towns A, B, C ... Mary has on a sheet of paper a list of distances between these towns. ls = [50, 55, 57, 58, 60].
-- John is tired of driving and he says to Mary that he doesn't want to drive more than t = 174 miles and he will visit only 3 towns.

-- Which distances, hence which towns, they will choose so that the sum of the distances is the biggest possible to please Mary and John?

-- Example:
-- With list ls and 3 towns to visit they can make a choice between: [50,55,57],[50,55,58],[50,55,60],[50,57,58],[50,57,60],[50,58,60],[55,57,58],[55,57,60],[55,58,60],[57,58,60].

-- The sums of distances are then: 162, 163, 165, 165, 167, 168, 170, 172, 173, 175.

-- The biggest possible sum taking a limit of 174 into account is then 173 and the distances of the 3 corresponding towns is [55, 58, 60].

-- The function chooseBestSum (or choose_best_sum or ... depending on the language) will take as parameters t (maximum sum of distances, integer >= 0), k (number of towns to visit, k >= 1) and ls (list of distances, all distances are positive or zero integers and this list has at least one element). The function returns the "best" sum ie the biggest possible sum of k distances less than or equal to the given limit t, if that sum exists, or otherwise nil, null, None, Nothing, depending on the language.

-- With C, C++, Dart, Fortran, F#, Go, Julia, Kotlin, Nim, OCaml, Pascal, Perl, PowerShell, Reason, Rust, Scala, Shell, Swift return -1.

-- Examples:
-- ts = [50, 55, 56, 57, 58] choose_best_sum(163, 3, ts) -> 163

-- xs = [50] choose_best_sum(163, 3, xs) -> nil (or null or ... or -1 (C++, C, Rust, Swift, Go, ...)

-- ys = [91, 74, 73, 85, 73, 81, 87] choose_best_sum(230, 3, ys) -> 228

-- Notes:
-- try not to modify the input list of distances ls
-- in some languages this "list" is in fact a string (see the Sample Tests).

module Main (main) where

import Control.Monad.ST
import Data.Foldable (traverse_)
import Data.STRef
import Test.Hspec
import Text.Printf

chooseBestSum' :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum' t k ls = runST $ do
  maximumDistance <- newSTRef Nothing

  evaluateNextCity t k ls 0 maximumDistance

  readSTRef maximumDistance
  where
    evaluateNextCity t 0 _ currentDistance maximumDistance = do
        s <- readSTRef maximumDistance
        case s of
          Nothing -> if currentDistance > 0 && t >= 0
            then writeSTRef maximumDistance $ Just currentDistance
            else pure ()
          Just d -> if d < currentDistance && t >= 0
            then writeSTRef maximumDistance $ Just currentDistance
            else pure ()
        pure ()
    evaluateNextCity _ _ [] _ _ = pure ()
    evaluateNextCity t k (city:ls) currentDistance maximumDistance
      | t < 0 = pure ()
      | otherwise = do
        evaluateNextCity (t - city) (k - 1) ls (currentDistance + city) maximumDistance
        evaluateNextCity t k ls currentDistance maximumDistance

chooseBestSum t k ls = chooseBestSum

test (t, k, ls) expected =
  it (printf "Testing %d %d %s" t k (show ls)) $ chooseBestSum' t k ls `shouldBe` expected

testData =
  [ ((163, 3, [50, 55, 56, 57, 58]), Just 163),
    ((163, 3, [50]), Nothing),
    ((230, 3, [91, 74, 73, 85, 73, 81, 87]), Just 228),
    ((331, 2, [91, 74, 73, 85, 73, 81, 87]), Just 178),
    ((331, 4, [91, 74, 73, 85, 73, 81, 87]), Just 331)
  ]

main = hspec $ do
  traverse_ (uncurry test) testData
