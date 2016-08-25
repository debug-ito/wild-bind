module Main (main,spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "README.md" $ do
  it "should fail for now" $ do
    True `shouldBe` False

