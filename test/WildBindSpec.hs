module WildBindSpec (main, spec) where

import Test.Hspec
import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Concurrent (forkIOWithUnmask, killThread)
import Control.Concurrent.STM (atomically, TChan, readTChan, tryReadTChan, writeTChan, newTChanIO)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified WildBind as WB
import qualified WildBind.Binding as WBB
import WildBind.ForTest (SampleInput(..), SampleState(..))

newtype EventChan s i = EventChan { unEventChan :: TChan (WB.FrontEvent s i) }

instance WB.FrontEventSource (EventChan s i) s i where
  nextEvent = atomically . readTChan . unEventChan

data GrabHistory i = GSet i | GUnset i deriving (Show, Eq, Ord)

newtype GrabChan i = GrabChan { unGrabChan :: TChan (GrabHistory i) }

instance WB.FrontInputDevice (GrabChan i) i where
  defaultActionDescription _ _ = ""
  setGrab f i = atomically $ writeTChan (unGrabChan f) (GSet i)
  unsetGrab f i = atomically $ writeTChan (unGrabChan f) (GUnset i)


outChanOn :: MonadIO m => TChan a -> i -> a -> (i, WBB.Action m ())
outChanOn out_chan input out_elem = WBB.on' input "" (liftIO $ atomically $ writeTChan out_chan out_elem)

withWildBind :: Ord i => WBB.Binding s i -> (EventChan s i -> GrabChan i -> IO ()) -> IO ()
withWildBind binding action = do
  echan <- EventChan <$> newTChanIO
  gchan <- GrabChan <$> newTChanIO
  let spawnWildBind = forkIOWithUnmask $ \umask -> umask $ WB.wildBind gchan echan binding
  bracket spawnWildBind killThread (\_ -> action echan gchan)

emitEvent :: TChan (WB.FrontEvent s i) -> WB.FrontEvent s i -> IO ()
emitEvent chan event = atomically $ writeTChan chan event

shouldProduce :: (Show a, Eq a) => TChan a -> a -> IO ()
shouldProduce chan expectation = (atomically $ readTChan chan) `shouldReturn` expectation

readAll :: TChan a -> IO [a]
readAll chan = atomically $ readAll' [] where
  readAll' acc = do
    mret <- tryReadTChan chan
    case mret of
      Nothing -> return (reverse acc)
      Just ret -> readAll' (ret : acc)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "wildBind" $ do
    it "should enable input grabs" $ do
      ochan <- newTChanIO
      let b = WBB.stateless [outChanOn ochan SIa 'A',
                             outChanOn ochan SIb 'B']
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WB.FEChange $ SS ""
        emitEvent echan $ WB.FEInput (SS "") SIa
        ochan `shouldProduce` 'A'
        ghist <- readAll gchan
        ghist `shouldMatchList` [GSet SIa, GSet SIb]
      
