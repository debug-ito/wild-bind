module WildBind.BindingSpec (main, spec) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty)
import Data.Maybe (isNothing)

import Test.Hspec
import Test.QuickCheck (Gen, Arbitrary(arbitrary,shrink), arbitraryBoundedEnum, property)

import qualified WildBind.Binding as WB

main :: IO ()
main = hspec spec

data SampleInput = SIa | SIb | SIc
                 deriving (Show, Eq, Ord, Enum, Bounded)

instance Arbitrary SampleInput where
  arbitrary = arbitraryBoundedEnum

data SampleState = SS String
                 deriving (Show, Eq, Ord)

instance Arbitrary SampleState where
  arbitrary = SS <$> arbitrary
  shrink (SS s) = SS <$> shrink s

-- genStatelessBinding :: Arbitrary a => IORef [a] -> Gen (WB.Binding s SampleInput)
-- genStatelessBinding = do
--   WB.on' <$> arbitraryBoundedEnum

mempty_stateless :: WB.Binding SampleState SampleInput
mempty_stateless = mempty

spec :: Spec
spec = do
  describe "Binding (Monoid instances)" $ do
    it "mempty returns empty binding" $ property
      ( isNothing <$> (WB.boundAction mempty_stateless <$> arbitrary <*> arbitrary) )
