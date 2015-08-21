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
import qualified Control.Monad.Trans.State as State

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


newStrRef :: IO (IORef [Char])
newStrRef = newIORef []

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
    it "returns a stateless Binding" $ do
      out <- newStrRef
      let b = WB.stateless [outOn out SIa 'A', outOn out SIb 'B']
      WB.boundInputs b (SS "") `shouldMatchList` [SIa, SIb]
      WB.boundAction b (SS "") SIc `shouldSatisfy` isNothing
      actRun $ WB.boundAction b (SS "") SIa
      readIORef out `shouldReturn` "A"
      actRun $ WB.boundAction b (SS "") SIb
      readIORef out `shouldReturn` "BA"
  describe "whenS" $ do
    it "adds a condition on the front-end state" $ do
      out <- newStrRef
      let b = WB.whenS (\(SS s) -> s == "hoge") $ WB.stateless [outOn out SIa 'A']
      WB.boundInputs b (SS "") `shouldMatchList` []
      WB.boundAction b (SS "") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "foobar") `shouldMatchList` []
      WB.boundAction b (SS "foobar") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "hoge") `shouldMatchList` [SIa]
      actRun $ WB.boundAction b (SS "hoge") SIa
      readIORef out `shouldReturn` "A"
    it "is AND condition" $ do
      out <- newStrRef
      let raw_b = WB.stateless [outOn out SIa 'A']
          b = WB.whenS ((<= 5) . length . unSS) $ WB.whenS ((3 <=) . length . unSS) $ raw_b
      WB.boundInputs b (SS "ho") `shouldMatchList` []
      WB.boundAction b (SS "ho") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "hogehoge") `shouldMatchList` []
      WB.boundAction b (SS "hogehoge") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "hoge") `shouldMatchList` [SIa]
      actRun $ WB.boundAction b (SS "hoge") SIa
      readIORef out `shouldReturn` "A"
  describe "Binding (mappend)" $ do
    it "combines two stateless Bindings" $ do
      out <- newStrRef
      let b1 = WB.stateless [outOn out SIa 'A']
          b2 = WB.stateless [outOn out SIb 'B']
          b = b1 <> b2
      WB.boundInputs b (SS "") `shouldMatchList` [SIa, SIb]
      void $ inputAll b (SS "") [SIa, SIb]
      readIORef out `shouldReturn` "BA"
    it "front-end conditions are preserved" $ do
      out <- newStrRef
      let b1 = WB.whenS ((3 <=) . length . unSS) $ WB.stateless [outOn out SIa 'A']
          b2 = WB.whenS ((<= 5) . length . unSS) $ WB.stateless [outOn out SIb 'B']
          b = b1 <> b2
      WB.boundInputs b (SS "aa") `shouldMatchList` [SIb]
      WB.boundInputs b (SS "aabb") `shouldMatchList` [SIa, SIb]
      WB.boundInputs b (SS "aabbcc") `shouldMatchList` [SIa]
    it "prefers the latter Binding" $ do
      out <- newStrRef
      let b1 = WB.stateless [outOn out SIa '1']
          b2 = WB.stateless [outOn out SIa '2']
          b = b1 <> b2
      WB.boundInputs b (SS "") `shouldMatchList` [SIa]
      actRun $ WB.boundAction b (SS "") SIa
      readIORef out `shouldReturn` "2"
  describe "convFrontState" $ do
    it "converts front-end state" $ do
      out <- newStrRef
      let orig_b = WB.whenS (("hoge" ==) . unSS) $ WB.stateless [outOn out SIa 'A']
          b = WB.convFrontState SS orig_b
      WB.boundInputs b "" `shouldMatchList` []
      WB.boundInputs b "hoge" `shouldMatchList` [SIa]
      actRun $ WB.boundAction b "hoge" SIa
      readIORef out `shouldReturn` "A"
  describe "convInput" $ do
    it "converts input symbols" $ do
      out <- newStrRef
      let orig_b = WB.stateless [outOn out SIa 'A']
          b = WB.convInput show orig_b
      WB.boundInputs b (SS "") `shouldMatchList` ["SIa"]
      actRun $ WB.boundAction b (SS "") "SIa"
      readIORef out `shouldReturn` "A"
  describe "convBackState" $ do
    it "TODO" $ (undefined :: IO ())
  describe "stateful" $ do
    it "returns a stateful Binding" $ do
      out <- newStrRef
      let b = WB.stateful $ \bs ->
            [outOnS out SIa (head $ show $ unSB bs) succ]
      WB.boundInputs' b (SB 0)  (SS "") `shouldBe` [SIa]
      WB.boundInputs' b (SB 10) (SS "hoge") `shouldBe` [SIa]
      void $ inputAll (WB.startFrom (SB 0) b) (SS "") $ replicate 12 SIa
      readIORef out `shouldReturn` "119876543210"
