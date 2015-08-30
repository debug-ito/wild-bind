module WildBind.BindingSpec (main, spec) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (void, join)
import Data.Monoid (mempty, (<>))
import Data.Maybe (isNothing, fromJust)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)

import Test.Hspec
import Test.QuickCheck (Gen, Arbitrary(arbitrary,shrink), arbitraryBoundedEnum, property,
                        listOf, sample')
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State
import qualified Lens.Micro as Lens

import qualified WildBind.Binding as WB

main :: IO ()
main = hspec spec

data SampleInput = SIa | SIb | SIc
                 deriving (Show, Eq, Ord, Enum, Bounded)

instance Arbitrary SampleInput where
  arbitrary = arbitraryBoundedEnum

data SampleState = SS { unSS :: String }
                 deriving (Show, Eq, Ord)

instance Arbitrary SampleState where
  arbitrary = SS <$> arbitrary
  shrink (SS s) = SS <$> shrink s

data SampleBackState = SB { unSB :: Int }
                     deriving (Show, Eq, Ord)

instance Enum SampleBackState where
  toEnum = SB
  fromEnum = unSB

data BiggerSampleBackState = BSB { _lSB :: SampleBackState, _rSB :: SampleBackState }
                           deriving (Show, Eq, Ord)

lSB :: Lens.Lens' BiggerSampleBackState SampleBackState
lSB = Lens.lens _lSB (\bsb sb -> bsb { _lSB = sb })

rSB :: Lens.Lens' BiggerSampleBackState SampleBackState
rSB = Lens.lens _rSB (\bsb sb -> bsb { _rSB = sb })

newStrRef :: MonadIO m => m (IORef String)
newStrRef = liftIO $ newIORef []

withStrRef :: MonadIO m => (IORef String -> (String -> m ()) -> m ()) -> m ()
withStrRef action = do
  out <- newStrRef
  let checkOut exp_str = liftIO $ readIORef out `shouldReturn` exp_str
  action out checkOut

outOn :: MonadIO m => IORef [a] -> i -> a -> (i, WB.Action m ())
outOn out_ref input out_elem = WB.on' input "" $ liftIO $ modifyIORef out_ref (out_elem :)

outOnS :: MonadIO m => IORef [a] -> i -> a -> (s -> s) -> (i, WB.Action (State.StateT s m) ())
outOnS out_ref input out_elem modifier = WB.on' input "" $ do
  State.modify modifier
  liftIO $ modifyIORef out_ref (out_elem :)

genStatelessBinding :: Arbitrary a => IORef [a] -> Gen (WB.Binding s SampleInput)
genStatelessBinding out_list = do
  let outputRandomElem = do
        out_elem <- arbitrary
        return $ modifyIORef out_list (out_elem :)
  WB.stateless <$> (listOf $ WB.on' <$> arbitrary <*> pure "" <*> outputRandomElem)

generate :: Gen a -> IO a
generate = fmap head . sample'

inputAll :: Ord i => WB.Binding s i -> s -> [i] -> IO (WB.Binding s i)
inputAll b _ [] = return b
inputAll binding state (i:rest) = case WB.boundAction binding state i of
  Nothing -> inputAll binding state rest
  Just act -> join $ inputAll <$> WB.actDo act <*> return state <*> return rest

execAll :: Ord i => s -> [i] -> State.StateT (WB.Binding s i) IO ()
execAll state inputs = do
  b <- State.get
  next_b <- liftIO $ inputAll b state inputs
  State.put next_b

execAll' :: Ord i => [i] -> State.StateT (WB.Binding SampleState i) IO ()
execAll' = execAll (SS "")

mempty_stateless :: WB.Binding SampleState SampleInput
mempty_stateless = mempty

checkMappend :: (WB.Binding SampleState SampleInput -> WB.Binding SampleState SampleInput) -> IO ()
checkMappend append_op = do
  out_ref <- newStrRef
  rand_binding <- generate $ genStatelessBinding out_ref
  let execute b = void $ inputAll b (SS "") =<< generate (listOf arbitrary)
  execute rand_binding
  out_orig <- readIORef out_ref
  writeIORef out_ref []
  execute (append_op rand_binding)
  readIORef out_ref `shouldReturn` out_orig

actRun :: Maybe (WB.Action IO a) -> IO ()
actRun = void . WB.actDo . fromJust

checkInputsS :: (Show i, Eq i) => s -> [i] -> State.StateT (WB.Binding s i) IO ()
checkInputsS state exp_in = State.get >>= \b -> lift $ WB.boundInputs b state `shouldMatchList` exp_in

checkInputsS' :: (Show i, Eq i) => [i] -> State.StateT (WB.Binding SampleState i) IO ()
checkInputsS' = checkInputsS (SS "")

evalStateEmpty :: State.StateT (WB.Binding SampleState SampleInput) IO () -> IO ()
evalStateEmpty s = State.evalStateT s mempty_stateless

spec :: Spec
spec = do
  describe "Binding (Monoid instances)" $ do
    it "mempty returns empty binding" $ property
      ( isNothing <$> (WB.boundAction mempty_stateless <$> arbitrary <*> arbitrary) )
    it "mempty `mappend` random == mempty" $ do
      checkMappend (mempty <>)
    it "random `mappend` mempty == mempty" $ do
      checkMappend (<> mempty)
  describe "stateless" $ do
    it "returns a stateless Binding" $ withStrRef $ \out checkOut -> do
      let b = WB.stateless [outOn out SIa 'A', outOn out SIb 'B']
      WB.boundInputs b (SS "") `shouldMatchList` [SIa, SIb]
      WB.boundAction b (SS "") SIc `shouldSatisfy` isNothing
      actRun $ WB.boundAction b (SS "") SIa
      checkOut "A"
      actRun $ WB.boundAction b (SS "") SIb
      checkOut "BA"
  describe "whenFront" $ do
    it "adds a condition on the front-end state" $ withStrRef $ \out checkOut -> do
      let b = WB.whenFront (\(SS s) -> s == "hoge") $ WB.stateless [outOn out SIa 'A']
      WB.boundInputs b (SS "") `shouldMatchList` []
      WB.boundAction b (SS "") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "foobar") `shouldMatchList` []
      WB.boundAction b (SS "foobar") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "hoge") `shouldMatchList` [SIa]
      actRun $ WB.boundAction b (SS "hoge") SIa
      checkOut "A"
    it "is AND condition" $ withStrRef $ \out checkOut -> do
      let raw_b = WB.stateless [outOn out SIa 'A']
          b = WB.whenFront ((<= 5) . length . unSS) $ WB.whenFront ((3 <=) . length . unSS) $ raw_b
      WB.boundInputs b (SS "ho") `shouldMatchList` []
      WB.boundAction b (SS "ho") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "hogehoge") `shouldMatchList` []
      WB.boundAction b (SS "hogehoge") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "hoge") `shouldMatchList` [SIa]
      actRun $ WB.boundAction b (SS "hoge") SIa
      checkOut "A"
  describe "Binding (mappend)" $ do
    it "combines two stateless Bindings" $ withStrRef $ \out checkOut -> do
      let b1 = WB.stateless [outOn out SIa 'A']
          b2 = WB.stateless [outOn out SIb 'B']
          b = b1 <> b2
      WB.boundInputs b (SS "") `shouldMatchList` [SIa, SIb]
      void $ inputAll b (SS "") [SIa, SIb]
      checkOut "BA"
    it "front-end conditions are preserved" $ withStrRef $ \out _ -> do
      let b1 = WB.whenFront ((3 <=) . length . unSS) $ WB.stateless [outOn out SIa 'A']
          b2 = WB.whenFront ((<= 5) . length . unSS) $ WB.stateless [outOn out SIb 'B']
          b = b1 <> b2
      WB.boundInputs b (SS "aa") `shouldMatchList` [SIb]
      WB.boundInputs b (SS "aabb") `shouldMatchList` [SIa, SIb]
      WB.boundInputs b (SS "aabbcc") `shouldMatchList` [SIa]
    it "prefers the latter Binding" $ withStrRef $ \out checkOut -> do
      let b1 = WB.stateless [outOn out SIa '1', outOn out SIb 'B']
          b2 = WB.stateless [outOn out SIa '2']
          b = b1 <> b2
      WB.boundInputs b (SS "") `shouldMatchList` [SIa, SIb]
      actRun $ WB.boundAction b (SS "") SIa
      checkOut "2"
    it "preserves implicit back-end states" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let b1 = WB.startFrom (SB 0) $ WB.stateful $ \bs -> case bs of
            SB 0 -> [outOnS out SIa '0' (\_ -> SB 1)]
            SB 1 -> [outOnS out SIa '1' (\_ -> SB 0)]
            _ -> []
          b2 = WB.startFrom (SB 0) $ WB.stateful $ \bs -> case bs of
            SB 0 -> [outOnS out SIb '2' (\_ -> SB 1)]
            SB 1 -> [outOnS out SIb '3' (\_ -> SB 0)]
            _ -> []
      State.put (b1 <> b2)
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIa]
      checkOut "0"
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIb]
      checkOut "20"
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIa]
      checkOut "120"
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIb]
      checkOut "3120"
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIa]
      checkOut "03120"
  describe "convFrontState" $ do
    it "converts front-end state" $ withStrRef $ \out checkOut -> do
      let orig_b = WB.whenFront (("hoge" ==) . unSS) $ WB.stateless [outOn out SIa 'A']
          b = WB.convFrontState SS orig_b
      WB.boundInputs b "" `shouldMatchList` []
      WB.boundInputs b "hoge" `shouldMatchList` [SIa]
      actRun $ WB.boundAction b "hoge" SIa
      checkOut "A"
  describe "convInput" $ do
    it "converts input symbols" $ withStrRef $ \out checkOut -> do
      let orig_b = WB.stateless [outOn out SIa 'A']
          b = WB.convInput show orig_b
      WB.boundInputs b (SS "") `shouldMatchList` ["SIa"]
      actRun $ WB.boundAction b (SS "") "SIa"
      checkOut "A"
  describe "convBackState" $ do
    it "TODO" $ (undefined :: IO ())
  describe "stateful" $ do
    it "returns a stateful Binding" $ withStrRef $ \out checkOut -> do
      let b = WB.stateful $ \bs ->
            [outOnS out SIa (head $ show $ unSB bs) succ]
      WB.boundInputs' b (SB 0)  (SS "") `shouldBe` [SIa]
      WB.boundInputs' b (SB 10) (SS "hoge") `shouldBe` [SIa]
      void $ inputAll (WB.startFrom (SB 0) b) (SS "") $ replicate 12 SIa
      checkOut "119876543210"
    it "can create a stateful Binding with different bound inputs for different back-end state" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      State.put $ WB.startFrom (SB 0) $ WB.stateful $ \bs -> case bs of
        SB 0 -> [outOnS out SIa 'A' (\_ -> SB 1)]
        SB 1 -> [outOnS out SIb 'B' (\_ -> SB 2)]
        SB 2 -> [outOnS out SIc 'C' (\_ -> SB 0)]
        _ -> []
      checkOut ""
      checkInputsS (SS "") [SIa]
      execAll (SS "") [SIa]
      checkOut "A"
      checkInputsS (SS "") [SIb]
      execAll (SS "") [SIb]
      checkOut "BA"
      checkInputsS (SS "") [SIc]
      execAll (SS "") [SIc]
      checkOut "CBA"
      checkInputsS (SS "") [SIa]
  describe "Binding (mappend, stateful)" $ do
    it "shares the explicit back-end state" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let b1 = WB.stateful $ \bs -> case bs of
            SB 0 -> [outOnS out SIa 'A' (\_ -> SB 1)]
            SB 1 -> [outOnS out SIb 'B' (\_ -> SB 2),
                     outOnS out SIc 'b' (\_ -> SB 2)]
            SB 2 -> [outOnS out SIc 'C' (\_ -> SB 0)]
            _ -> []
          b2 = WB.stateful $ \bs -> case bs of
            SB 1 -> [outOnS out SIb 'D' (\_ -> SB 0)]
            _ -> []
          b = b1 <> b2
      State.put $ WB.startFrom (SB 0) b
      checkInputsS (SS "") [SIa]
      execAll (SS "") [SIa]
      checkOut "A"
      checkInputsS (SS "") [SIb, SIc]
      execAll (SS "") [SIb]
      checkOut "DA"
      checkInputsS (SS "") [SIa]
      execAll (SS "") [SIa, SIc]
      checkOut "bADA"
      checkInputsS (SS "") [SIc]
      execAll (SS "") [SIc]
      checkOut "CbADA"
      checkInputsS (SS "") [SIa]

  describe "extendAt" $ do
    it "extend the explicit state" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let bl = WB.stateful $ \bs -> case bs of
            SB 0 -> [outOnS out SIa '0' (\_ -> SB 1)]
            SB 1 -> [outOnS out SIa '1' (\_ -> SB 0)]
            _ -> []
          br = WB.stateful $ \bs -> case bs of
            SB 0 -> [outOnS out SIb '2' (\_ -> SB 1)]
            SB 1 -> [outOnS out SIb '3' (\_ -> SB 0)]
            _ -> []
          bg = WB.stateful $ \bs -> case bs of
            BSB (SB 0) (SB 0) -> [outOnS out SIc '4' (\_ -> BSB (SB 1) (SB 1))]
            _ -> []
          b = (WB.extendAt lSB bl) <> (WB.extendAt rSB br) <> bg
      State.put $ WB.startFrom (BSB (SB 0) (SB 0)) b
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIa]
      checkOut "0"
      checkInputsS' [SIa, SIb]
      execAll' [SIb]
      checkOut "20"
      checkInputsS' [SIa, SIb]
      execAll' [SIb]
      checkOut "320"
      checkInputsS' [SIa, SIb]
      execAll' [SIa]
      checkOut "1320"
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIc]
      checkOut "41320"
      checkInputsS' [SIa, SIb]
      execAll' [SIa, SIb]
      checkOut "3141320"

  describe "extend" $ do
    it "extends a stateless Binding" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let bl = WB.stateless [
            outOn out SIa 'a',
            outOn out SIb 'b',
            outOn out SIc 'c']
          bs = WB.stateful $ \bstate -> case bstate of
            SB 0 -> [outOnS out SIa 'A' (\_ -> SB 1)]
            SB 1 -> [outOnS out SIb 'B' (\_ -> SB 2)]
            SB 2 -> [outOnS out SIc 'C' (\_ -> SB 0)]
            _ -> []
      State.put $ WB.startFrom (SB 0) $ (WB.extend bl <> bs)
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIb, SIc, SIa]
      checkOut "Acb"
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIa, SIc, SIb]
      checkOut "BcaAcb"
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIa, SIb, SIc]
      checkOut "CbaBcaAcb"
      

-- add condition function to back-end state? like 'whenBS'
