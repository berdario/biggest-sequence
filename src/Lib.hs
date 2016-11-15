module Lib
    ( biggest
    ) where

import           Data.List (sortBy)

comp GT _ _ _ = GT
comp LT _ _ _ = LT
comp EQ _ [] [] = EQ
comp EQ previous (x:_) [] = compare x previous
comp EQ previous [] (y:_) = compare previous y
comp EQ _ (x:xs) (y:ys) = comp (compare x y) (max x y) xs ys

biggest :: [Int] -> String
biggest = concat . sortBy (flip $ comp EQ '0') . map show

