module WildBind.BindingSpec (main, spec) where

import Test.Hspec

import Data.Monoid (mempty)

import WildBind.Binding(Binding)

main :: IO ()
main = hspec spec

data SampleInput = SIa | SIb | SIc
                 deriving (Show, Eq, Ord, Enum, Bounded)

data SampleState = SS String
                 deriving (Show, Eq, Ord)

spec :: Spec
spec = do
  describe "Binding (Monoid instances)" $ do
    it "mempty returns empty binding" $ do
      True `shouldBe` False
      -- use QuickCheck to check the emptiness.
