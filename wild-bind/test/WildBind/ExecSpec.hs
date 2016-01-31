module WildBind.ExecSpec (main, spec) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.STM (atomically, TChan, readTChan, tryReadTChan, writeTChan, newTChanIO)
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.State as State
import Data.Monoid ((<>))
import Test.Hspec

import qualified WildBind.Binding as WBB
import qualified WildBind.Exec as WBE
import qualified WildBind.FrontEnd as WBF

import WildBind.ForTest (SampleInput(..), SampleState(..), SampleBackState(..))

newtype EventChan s i = EventChan { unEventChan :: TChan (WBF.FrontEvent s i) }

data GrabHistory i = GSet i | GUnset i deriving (Show, Eq, Ord)

newtype GrabChan i = GrabChan { unGrabChan :: TChan (GrabHistory i) }

frontEnd :: EventChan s i -> GrabChan i -> WBF.FrontEnd s i
frontEnd echan gchan = WBF.FrontEnd
  { WBF.frontDefaultDescription = const "",
    WBF.frontSetGrab = \i -> atomically $ writeTChan (unGrabChan gchan) (GSet i),
    WBF.frontUnsetGrab = \i -> atomically $ writeTChan (unGrabChan gchan) (GUnset i),
    WBF.frontNextEvent = atomically $ readTChan $ unEventChan echan
  }

_write :: MonadIO m => TChan a -> a -> m ()
_write tc = liftIO . atomically . writeTChan tc

outChanOn :: MonadIO m => TChan a -> i -> a -> (i, WBB.Action m ())
outChanOn out_chan input out_elem = WBB.on input "" (out_chan `_write` out_elem)

outChanOnS :: TChan a -> i -> a -> bs -> (i, WBB.Action (State.StateT bs IO) ())
outChanOnS out_chan input out_elem next_state = WBB.on input "" $ do
  liftIO $ atomically $ writeTChan out_chan out_elem
  State.put next_state

withWildBind' :: Ord i => (WBF.FrontEnd s i -> IO ()) -> (EventChan s i -> GrabChan i -> IO ()) -> IO ()
withWildBind' exec action = do
  echan <- EventChan <$> newTChanIO
  gchan <- GrabChan <$> newTChanIO
  let spawnWildBind = forkIOWithUnmask $ \umask -> umask $ exec (frontEnd echan gchan)
  bracket spawnWildBind killThread (\_ -> action echan gchan)
  
withWildBind :: Ord i => WBB.Binding s i -> (EventChan s i -> GrabChan i -> IO ()) -> IO ()
withWildBind binding action = withWildBind' (WBE.wildBind binding) action

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
changeAndInput s i = [WBF.FEChange s, WBF.FEInput i]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  wildBindSpec
  optionSpec

wildBindSpec :: Spec
wildBindSpec = do
  describe "wildBind" $ do
    it "should enable input grabs" $ do
      ochan <- newTChanIO
      let b = WBB.binds [outChanOn ochan SIa 'A',
                         outChanOn ochan SIb 'B']
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange $ SS ""
        emitEvent echan $ WBF.FEInput SIa
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
      let b' = WBB.caseBack $ \bs -> case bs of
            (SB 0) -> WBB.binds' [outChanOnS ochan SIa 'A' (SB 1)]
            (SB 1) -> WBB.binds' [outChanOnS ochan SIb 'B' (SB 0)]
            _ -> mempty
          b = WBB.startFrom (SB 0) b'
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange (SS "")
        threadDelay 10000
        gchan `shouldNowMatch` [GSet SIa]
        emitEvent echan $ WBF.FEInput SIa
        ochan `shouldProduce` 'A'
        threadDelay 10000
        gchan `shouldNowMatch` [GUnset SIa, GSet SIb]
        emitEvent echan $ WBF.FEInput SIb
        ochan `shouldProduce` 'B'
        threadDelay 10000
        gchan `shouldNowMatch` [GUnset SIb, GSet SIa]

shouldNextMatch :: (Show a, Eq a) => TChan [a] -> [a] -> IO ()
shouldNextMatch tc expected = do
  got <- atomically $ readTChan tc
  got `shouldMatchList` expected

optionSpec :: Spec
optionSpec = do
  describe "optBindingHook" $ do
    it "hooks change of binding because front-end state changes" $ do
      hook_chan <- newTChanIO
      out_chan <- newTChanIO
      let opt = WBE.def { WBE.optBindingHook = _write hook_chan }
          b = WBB.whenFront (== SS "hoge") $ WBB.binds [
            WBB.on SIa "a button" (out_chan `_write` 'a'),
            WBB.on SIb "b button" (out_chan `_write` 'b') ]
      withWildBind' (WBE.wildBind' opt b) $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange (SS "hoge")
        hook_chan `shouldNextMatch` [(SIa, "a button"), (SIb, "b button")]
        emitEvent echan $ WBF.FEInput SIa
        hook_chan `shouldNextMatch` [(SIa, "a button"), (SIb, "b button")]
        out_chan `shouldProduce` 'a'
        gchan `shouldNowMatch` [GSet SIa, GSet SIb]
        emitEvent echan $ WBF.FEChange (SS "")
        hook_chan `shouldNextMatch` []
        gchan `shouldNowMatch` [GUnset SIa, GUnset SIb]
    it "hooks change of binding because back-end state changes" $ do
      hook_chan <- newTChanIO
      out_chan <- newTChanIO
      let opt = WBE.def { WBE.optBindingHook = _write hook_chan  }
          b = WBB.startFrom (SB 0) $ WBB.caseBack $ \bs -> case bs of
            (SB 0) -> WBB.binds' [WBB.on SIa "a button" (out_chan `_write` 'a' >> State.put (SB 1))]
            (SB 1) -> WBB.binds' [WBB.on SIa "A BUTTON" (out_chan `_write` 'A' >> State.put (SB 0)),
                                  WBB.on SIc "c button" (out_chan `_write` 'c')]
            _ -> mempty
      withWildBind' (WBE.wildBind' opt b) $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange (SS "")
        hook_chan `shouldNextMatch` [(SIa, "a button")]
        gchan `shouldNowMatch` [GSet SIa]
        emitEvent echan $ WBF.FEInput SIa
        out_chan `shouldProduce` 'a'
        hook_chan `shouldNextMatch` [(SIa, "A BUTTON"), (SIc, "c button")]
        gchan `shouldNowMatch` [GSet SIc]
        emitEvent echan $ WBF.FEInput SIc
        out_chan `shouldProduce` 'c'
        hook_chan `shouldNextMatch` [(SIa, "A BUTTON"), (SIc, "c button")]
        gchan `shouldNowMatch` []
        emitEvent echan $ WBF.FEInput SIa
        out_chan `shouldProduce` 'A'
        hook_chan `shouldNextMatch` [(SIa, "a button")]
        gchan `shouldNowMatch` [GUnset SIc]
        
