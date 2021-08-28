module Application.Helper.Average (average) where

import Data.Foldable
import Data.Semigroup
import IHP.Prelude

data Average n = Average {length :: !Int, sum :: !n}

averageDatum :: n -> Average n
averageDatum n = Average 1 n

getAverage :: (Num n, Fractional n) => Average n -> Maybe n
getAverage (Average l n) =
  if l == 0
    then Nothing
    else Just $ n / fromIntegral l

instance Num n => Semigroup (Average n) where
  Average lx nx <> Average ly ny = Average (lx + ly) (nx + ny)

instance Num n => Monoid (Average n) where
  mappend = (<>)
  mempty = Average 0 0

average :: (Fractional n, Foldable t) => t n -> Maybe n
average = getAverage . foldMap averageDatum
