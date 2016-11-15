import           Data.List       (permutations)
import           Test.QuickCheck

import           Lib

readInt :: String -> Int
readInt = read

newtype SmallList a = SmallList [a]
 deriving ( Eq, Ord, Show, Read )

instance Functor SmallList where
  fmap f (SmallList x) = SmallList (map f x)

instance Arbitrary a => Arbitrary (SmallList a) where
  arbitrary = SmallList `fmap` (arbitrary `suchThat` (\xs -> not (null xs) && length xs < 9))


bruteForce :: [Int] -> Int
bruteForce ns = maximum $ map (readInt . concat) $ permutations numbers
    where
        numbers = map show ns

prop_biggest :: SmallList (NonNegative Int) -> Bool
prop_biggest (SmallList ns) = read (biggest numbers) == bruteForce numbers
    where
        numbers = map getNonNegative ns

main :: IO ()
main = verboseCheck $ property prop_biggest

