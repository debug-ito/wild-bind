module WildBindSpec (main, spec) where

import Test.Hspec
import Control.Concurrent (Chan, readChan)
import Data.IORef (IORef, modifyIORef)

import qualified WildBind as WB

newtype EventChan s i = EventChan { unEventChan :: Chan (WB.FrontEvent s i) }

instance WB.FrontEventSource (EventChan s i) s i where
  nextEvent = readChan . unEventChan

data GrabHistory i = GSet i | GUnset i

newtype GrabRef i = GrabRef { unGrabRef :: IORef [GrabHistory i] }

instance WB.FrontInputDevice (GrabRef i) i where
  defaultActionDescription _ _ = ""
  setGrab f i = modifyIORef (unGrabRef f) (GSet i :)
  unsetGrab f i = modifyIORef (unGrabRef f) (GUnset i :)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "wildBind" $ do
    it "TODO: check wildBind's expected behavior, like, updating grab-sets according to Binding." $ do
      True `shouldBe` False
