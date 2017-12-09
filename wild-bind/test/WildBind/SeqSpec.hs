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
    checkBoundInputs (SS "") [SIc]
    execAll (SS "") [SIc] 
    checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
    execAll (SS "") [SIc]
    checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIc]
  specify "two prefixes" $ evalStateEmpty $ do
    State.put $ prefix [] [SIc, SIb] base_b
    checkBoundInputs (SS "") [SIc]
    execAll (SS "") [SIc]
    checkBoundInputs (SS "") [SIb]
    execAll (SS "") [SIb]
    checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIc]
  specify "cancel binding" $ evalStateEmpty $ do
    State.put $ prefix [SIa] [SIc, SIb] base_b
    checkBoundInputs (SS "") [SIa, SIc]
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIa, SIc]
    execAll (SS "") [SIc]
    checkBoundInputs (SS "") [SIa, SIb]
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIa, SIc]
    execAll (SS "") [SIc, SIb]
    checkBoundDescs (SS "") [(SIa, "cancel"), (SIb, "b")]
    execAll (SS "") [SIb]
    checkBoundInputs (SS "") [SIa, SIc]
    execAll (SS "") [SIc, SIb]
    checkBoundDescs (SS "") [(SIa, "cancel"), (SIb, "b")]
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIa, SIc]


checkBoundInputs :: (Eq i, Show i) => s -> [i] -> State.StateT (Binding s i) IO ()
checkBoundInputs fs expected = liftIO . (`shouldMatchList` expected) =<< curBoundInputs fs

checkBoundDescs :: (Eq i, Show i) => s -> [(i, ActionDescription)] -> State.StateT (Binding s i) IO ()
checkBoundDescs fs expected = liftIO . (`shouldMatchList` expected) =<< curBoundDescs fs

spec_prefix' :: Spec
spec_prefix' = describe "prefix'" $ do
  it "should allow nested prefixes" $ do
    True `shouldBe` False -- TODO
    
