module Lib
    ( biggest
    ) where

import           Data.List (sortBy)

biggest :: [Int] -> String
biggest = concat . sortBy (flip compare) . map show

