{-# LANGUAGE FlexibleInstances #-}

import           Data.List       (permutations)
import           Test.QuickCheck

import           Lib

readInt :: String -> Int
readInt = read

bruteForce :: [Int] -> Int
bruteForce ns = maximum $ map (readInt . concat) $ permutations numbers
    where
        numbers = map show ns

prop_biggest :: SmallList (NonNegative Int) -> Bool
prop_biggest (SmallList ns) = read (biggest numbers) == bruteForce numbers
    where
        numbers = map getNonNegative ns

main :: IO ()
main = quickCheck $ property prop_biggest

























newtype SmallList a = SmallList [a]
 deriving ( Eq, Ord, Read)

instance (Show x) => Show (SmallList (NonNegative x)) where
  show (SmallList xs) = show $ map getNonNegative xs

smallNonEmpty :: [a] -> Bool
smallNonEmpty xs = not (null xs) && length xs < 9

instance Arbitrary a => Arbitrary (SmallList a) where
  arbitrary = SmallList `fmap` (arbitrary `suchThat` smallNonEmpty)
  shrink (SmallList x) = map SmallList $ filter smallNonEmpty $ shrinkList shrink x
