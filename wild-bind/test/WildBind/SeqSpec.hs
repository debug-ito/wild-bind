module WildBind.SeqSpec (main,spec) where

import Control.Applicative ((<*>))
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as State
import Data.Monoid ((<>))
import Test.Hspec

import WildBind.Binding
  ( binds, on, run, as,
    boundActions, actDescription,
    boundInputs,
    Binding
  )
import WildBind.Description (ActionDescription)
import WildBind.Seq
  ( prefix,
    toSeq, fromSeq, withPrefix
  )

import WildBind.ForTest
  ( SampleInput(..), SampleState(..),
    evalStateEmpty, execAll,
    boundDescs, curBoundInputs, curBoundDescs, curBoundDesc
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_prefix
  spec_SeqBinding

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

checkBoundDesc :: (Ord i) => s -> i -> ActionDescription -> State.StateT (Binding s i) IO ()
checkBoundDesc fs input expected = liftIO . (`shouldBe` Just expected) =<< curBoundDesc fs input
  

spec_SeqBinding :: Spec
spec_SeqBinding = describe "SeqBinding" $ do
  let b_a = binds $ on SIa `as` "a" `run` return ()
      b_b = binds $ on SIb `as` "b" `run` return ()
  describe "withPrefix" $ do
    it "should allow nesting" $ evalStateEmpty $ do
      State.put $ fromSeq $ withPrefix [SIb] $ withPrefix [SIc] $ withPrefix [SIa] $ toSeq (b_a <> b_b)
      checkBoundInputs (SS "") [SIb]
      execAll (SS "") [SIb]
      checkBoundInputs (SS "") [SIc]
      execAll (SS "") [SIc]
      checkBoundInputs (SS "") [SIa]
      execAll (SS "") [SIa]
      checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
      execAll (SS "") [SIa]
      checkBoundInputs (SS "") [SIb]
  describe "mappend" $ do
    it "should be able to combine SeqBindings with different prefixes." $ evalStateEmpty $ do
      State.put $ fromSeq $ withPrefix [SIc] $ ( (withPrefix [SIa, SIc] $ toSeq $ b_a)
                                                 <> (withPrefix [SIa] $ toSeq $ b_b)
                                               )
      checkBoundInputs (SS "") [SIc]
      execAll (SS "") [SIc]
      checkBoundInputs (SS "") [SIa]
      execAll (SS "") [SIa]
      checkBoundInputs (SS "") [SIc, SIb]
      checkBoundDesc (SS "") SIb "b"
      execAll (SS "") [SIb]
      checkBoundInputs (SS "") [SIc]
      execAll (SS "") [SIc, SIa]
      checkBoundInputs (SS "") [SIc, SIb]
      execAll (SS "") [SIc]
      checkBoundDescs (SS "") [(SIa, "a")]
      execAll (SS "") [SIa]
      checkBoundInputs (SS "") [SIc]
      
      
      
