module WildBindSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "wildBind" $ do
    it "TODO: check wildBind's expected behavior, like, updating grab-sets according to Binding." $ do
      True `shouldBe` False
