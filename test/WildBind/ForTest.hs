module WildBind.ForTest (
  SampleInput(..),
  SampleState(..)
) where

import Control.Applicative ((<$>))
import Test.QuickCheck (Arbitrary(arbitrary,shrink), arbitraryBoundedEnum)

data SampleInput = SIa | SIb | SIc
                 deriving (Show, Eq, Ord, Enum, Bounded)

instance Arbitrary SampleInput where
  arbitrary = arbitraryBoundedEnum

data SampleState = SS { unSS :: String }
                 deriving (Show, Eq, Ord)

instance Arbitrary SampleState where
  arbitrary = SS <$> arbitrary
  shrink (SS s) = SS <$> shrink s

