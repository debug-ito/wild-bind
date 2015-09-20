module WildBindSpec (main, spec) where

import Test.Hspec
import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Control.Exception (bracket)
import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.STM (atomically, TChan, readTChan, tryReadTChan, writeTChan, newTChanIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.State as State

import qualified WildBind.FrontEnd as WBF
import qualified WildBind.Binding as WBB
import qualified WildBind.Exec as WBE
import WildBind.ForTest (SampleInput(..), SampleState(..), SampleBackState(..))

newtype EventChan s i = EventChan { unEventChan :: TChan (WBF.FrontEvent s i) }

data GrabHistory i = GSet i | GUnset i deriving (Show, Eq, Ord)

newtype GrabChan i = GrabChan { unGrabChan :: TChan (GrabHistory i) }

frontEnd :: EventChan s i -> GrabChan i -> WBF.FrontEnd s i
frontEnd echan gchan = WBF.FrontEnd {
  WBF.frontDefaultDescription = const "",
  WBF.frontSetGrab = \i -> atomically $ writeTChan (unGrabChan gchan) (GSet i),
  WBF.frontUnsetGrab = \i -> atomically $ writeTChan (unGrabChan gchan) (GUnset i),
  WBF.frontNextEvent = atomically $ readTChan $ unEventChan echan
  }

outChanOn :: MonadIO m => TChan a -> i -> a -> (i, WBB.Action m ())
outChanOn out_chan input out_elem = WBB.on input "" (liftIO $ atomically $ writeTChan out_chan out_elem)

outChanOnS :: TChan a -> i -> a -> bs -> (i, WBB.Action (State.StateT bs IO) ())
outChanOnS out_chan input out_elem next_state = WBB.on input "" $ do
  liftIO $ atomically $ writeTChan out_chan out_elem
  State.put next_state

withWildBind :: Ord i => WBB.Binding s i -> (EventChan s i -> GrabChan i -> IO ()) -> IO ()
withWildBind binding action = do
  echan <- EventChan <$> newTChanIO
  gchan <- GrabChan <$> newTChanIO
  let spawnWildBind = forkIOWithUnmask $ \umask -> umask $ WBE.wildBind binding (frontEnd echan gchan)
  bracket spawnWildBind killThread (\_ -> action echan gchan)

emitEvent :: TChan (WBF.FrontEvent s i) -> WBF.FrontEvent s i -> IO ()
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

shouldNowMatch :: (Show a, Eq a) => TChan a -> [a] -> IO ()
shouldNowMatch chan expectation = readAll chan >>= (`shouldMatchList` expectation)

changeAndInput :: s -> i -> [WBF.FrontEvent s i]
changeAndInput s i = [WBF.FEChange s, WBF.FEInput s i]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "wildBind" $ do
    it "should enable input grabs" $ do
      ochan <- newTChanIO
      let b = WBB.binds [outChanOn ochan SIa 'A',
                         outChanOn ochan SIb 'B']
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange $ SS ""
        emitEvent echan $ WBF.FEInput (SS "") SIa
        ochan `shouldProduce` 'A'
        ghist <- readAll gchan
        ghist `shouldMatchList` [GSet SIa, GSet SIb]
    it "should enable/disable grabs when the front-end state changes" $ do
      ochan <- newTChanIO
      let b = (WBB.whenFront (\(SS s) -> s == "A") $ WBB.binds [outChanOn ochan SIa 'A'])
              <>
              (WBB.whenFront (\(SS s) -> s == "B") $ WBB.binds [outChanOn ochan SIb 'B'])
              <>
              (WBB.whenFront (\(SS s) -> s == "C") $ WBB.binds [outChanOn ochan SIc 'C'])
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        mapM_ (emitEvent echan) $ changeAndInput (SS "A") SIa
        ochan `shouldProduce` 'A'
        gchan `shouldNowMatch` [GSet SIa]
        mapM_ (emitEvent echan) $ changeAndInput (SS "B") SIb
        ochan `shouldProduce` 'B'
        gchan `shouldNowMatch` [GUnset SIa, GSet SIb]
        mapM_ (emitEvent echan) $ changeAndInput (SS "C") SIc
        ochan `shouldProduce` 'C'
        gchan `shouldNowMatch` [GUnset SIb, GSet SIc]
        emitEvent echan $ WBF.FEChange (SS "")
        threadDelay 10000
        gchan `shouldNowMatch` [GUnset SIc]
    it "should enable/disable grabs when the back-end state changes" $ do
      ochan <- newTChanIO
      let b' = WBB.binds' $ \bs -> case bs of
            (SB 0) -> [outChanOnS ochan SIa 'A' (SB 1)]
            (SB 1) -> [outChanOnS ochan SIb 'B' (SB 0)]
            _ -> []
          b = WBB.startFrom (SB 0) b'
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange (SS "")
        threadDelay 10000
        gchan `shouldNowMatch` [GSet SIa]
        emitEvent echan $ WBF.FEInput (SS "") SIa
        ochan `shouldProduce` 'A'
        threadDelay 10000
        gchan `shouldNowMatch` [GUnset SIa, GSet SIb]
        emitEvent echan $ WBF.FEInput (SS "") SIb
        ochan `shouldProduce` 'B'
        threadDelay 10000
        gchan `shouldNowMatch` [GUnset SIb, GSet SIa]
          
        
      
