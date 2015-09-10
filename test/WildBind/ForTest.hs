module WildBind.ForTest (
  SampleInput(..),
  SampleState(..),
  SampleBackState(..)
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


data SampleBackState = SB { unSB :: Int }
                     deriving (Show, Eq, Ord)

instance Enum SampleBackState where
  toEnum = SB
  fromEnum = unSB

