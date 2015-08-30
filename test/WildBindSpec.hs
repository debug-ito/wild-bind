module WildBindSpec (main, spec) where

import Test.Hspec
import Control.Concurrent.STM (atomically, TChan, readTChan, writeTChan)

import qualified WildBind as WB

newtype EventChan s i = EventChan { unEventChan :: TChan (WB.FrontEvent s i) }

instance WB.FrontEventSource (EventChan s i) s i where
  nextEvent = atomically . readTChan . unEventChan

data GrabHistory i = GSet i | GUnset i

newtype GrabChan i = GrabChan { unGrabChan :: TChan (GrabHistory i) }

instance WB.FrontInputDevice (GrabChan i) i where
  defaultActionDescription _ _ = ""
  setGrab f i = atomically $ writeTChan (unGrabChan f) (GSet i)
  unsetGrab f i = atomically $ writeTChan (unGrabChan f) (GUnset i)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "wildBind" $ do
    it "TODO: check wildBind's expected behavior, like, updating grab-sets according to Binding." $ do
      True `shouldBe` False
