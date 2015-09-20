module WildBind.X11.Internal.QueueSpec (spec) where

import Test.Hspec

import qualified WildBind.X11.Internal.Queue as Q

new :: [Int] -> Q.Queue Int
new = Q.unshiftAll Q.empty

newAndPop :: [Int] -> (Maybe Int, Q.Queue Int)
newAndPop = Q.pop . new

spec :: Spec
spec = describe "WildBind.Internal.Queue" $ do
  specify "empty" $ do
    newAndPop [] `shouldBe` (Nothing, Q.empty)
  specify "one elem" $ do
    newAndPop [1] `shouldBe` (Just 1, Q.empty)
  specify "two elems" $ do
    newAndPop [1,2] `shouldBe` (Just 1, new [2])
  specify "three elems" $ do
    newAndPop [1,2,3] `shouldBe` (Just 1, new [2,3])
  specify "unshift" $ do
    (Q.pop $ Q.unshift Q.empty 2) `shouldBe` ((Just 2, Q.empty) :: (Maybe Int, Q.Queue Int))
