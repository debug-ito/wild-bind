module WildBind.SeqSpec (main,spec) where

import Control.Applicative ((<*>))
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as State
import Test.Hspec

import WildBind.Binding
  ( binds, on, run, as,
    boundActions, actDescription,
    boundInputs,
    Binding
  )
import WildBind.Description (ActionDescription)
import WildBind.Seq (prefix)

import WildBind.ForTest
  ( SampleInput(..), SampleState(..),
    evalStateEmpty, execAll,
    boundDescs, curBoundInputs, curBoundDescs
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_prefix
  spec_prefix'

spec_prefix :: Spec
spec_prefix = describe "prefix" $ do
  let base_b = binds $ do
        on SIa `as` "a" `run` return ()
        on SIb `as` "b" `run` return ()
  specify "no prefix" $ do
    let b = prefix [] [] base_b
    boundDescs b (SS "") `shouldMatchList`
      [ (SIa, "a"),
        (SIb, "b")
      ]
  specify "one prefix" $ evalStateEmpty $ do
    State.put $ prefix [] [SIc] base_b
    liftIO . (`shouldMatchList` [SIc]) =<< curBoundInputs (SS "")
    execAll (SS "") [SIc]
    liftIO . (`shouldMatchList` [(SIa, "a"), (SIb, "b")]) =<< curBoundDescs (SS "")
    execAll (SS "") [SIc]
    liftIO . (`shouldMatchList` [(SIa, "a"), (SIb, "b")]) =<< curBoundDescs (SS "")
    execAll (SS "") [SIa]
    liftIO . (`shouldMatchList` [SIc]) =<< curBoundInputs (SS "")
  specify "two prefixes" $ evalStateEmpty $ do
    State.put $ prefix [] [SIc, SIb] base_b
    liftIO . (`shouldMatchList` [SIc]) =<< curBoundInputs (SS "")
    execAll (SS "") [SIc]
    liftIO . (`shouldMatchList` [SIb]) =<< curBoundInputs (SS "")
    execAll (SS "") [SIb]
    liftIO . (`shouldMatchList` [(SIa, "a"), (SIb, "b")]) =<< curBoundDescs (SS "")
    execAll (SS "") [SIa]
    liftIO . (`shouldMatchList` [SIc]) =<< curBoundInputs (SS "")
  specify "reset binding" $ do
    True `shouldBe` False
    

spec_prefix' :: Spec
spec_prefix' = describe "prefix'" $ do
  it "should allow nested prefixes" $ do
    True `shouldBe` False -- TODO
    
