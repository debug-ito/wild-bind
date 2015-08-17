module WildBind.BindingSpec (main, spec) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty)
import Data.Maybe (isNothing)

import Test.Hspec
import Test.QuickCheck (Gen, arbitraryBoundedEnum, arbitrary, property)

import WildBind.Binding(Binding, boundAction)

main :: IO ()
main = hspec spec

data SampleInput = SIa | SIb | SIc
                 deriving (Show, Eq, Ord, Enum, Bounded)

data SampleState = SS String
                 deriving (Show, Eq, Ord)

genSamples :: Gen (SampleState, SampleInput)
genSamples = (,) <$> (SS <$> arbitrary) <*> arbitraryBoundedEnum

spec :: Spec
spec = do
  describe "Binding (Monoid instances)" $ do
    it "mempty returns empty binding" $ property $ do
      (s, i) <- genSamples
      return (isNothing $ boundAction mempty s i)
