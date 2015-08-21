module WildBind.BindingSpec (main, spec) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (void, join)
import Data.Monoid (mempty, mappend)
import Data.Maybe (isNothing, fromJust)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)

import Test.Hspec
import Test.QuickCheck (Gen, Arbitrary(arbitrary,shrink), arbitraryBoundedEnum, property,
                        listOf, sample')

import qualified WildBind.Binding as WB

main :: IO ()
main = hspec spec

data SampleInput = SIa | SIb | SIc
                 deriving (Show, Eq, Ord, Enum, Bounded)

instance Arbitrary SampleInput where
  arbitrary = arbitraryBoundedEnum

data SampleState = SS String
                 deriving (Show, Eq, Ord)

instance Arbitrary SampleState where
  arbitrary = SS <$> arbitrary
  shrink (SS s) = SS <$> shrink s

newStrRef :: IO (IORef [Char])
newStrRef = newIORef []

outOn :: IORef [a] -> i -> a -> (i, WB.Action IO ())
outOn out_ref input out_elem = WB.on' input "" $ modifyIORef out_ref (out_elem :)

genStatelessBinding :: Arbitrary a => IORef [a] -> Gen (WB.Binding s SampleInput)
genStatelessBinding out_list = do
  let outputRandomElem = do
        out_elem <- arbitrary
        return $ modifyIORef out_list (out_elem :)
  WB.stateless <$> (listOf $ WB.on' <$> arbitrary <*> pure "" <*> outputRandomElem)

generate :: Gen a -> IO a
generate = fmap head . sample'

inputAll :: Ord i => WB.Binding s i -> s -> [i] -> IO ()
inputAll _ _ [] = return ()
inputAll binding state (i:rest) = case WB.boundAction binding state i of
  Nothing -> inputAll binding state rest
  Just act -> join $ inputAll <$> WB.actDo act <*> return state <*> return rest

mempty_stateless :: WB.Binding SampleState SampleInput
mempty_stateless = mempty

checkMappend :: (WB.Binding SampleState SampleInput -> WB.Binding SampleState SampleInput) -> IO ()
checkMappend append_op = do
  out_ref <- newStrRef
  rand_binding <- generate $ genStatelessBinding out_ref
  let execute b = inputAll b (SS "") =<< generate (listOf arbitrary)
  execute rand_binding
  out_orig <- readIORef out_ref
  writeIORef out_ref []
  execute (append_op rand_binding)
  readIORef out_ref `shouldReturn` out_orig

spec :: Spec
spec = do
  describe "Binding (Monoid instances)" $ do
    it "mempty returns empty binding" $ property
      ( isNothing <$> (WB.boundAction mempty_stateless <$> arbitrary <*> arbitrary) )
    it "mempty `mappend` random == mempty" $ do
      checkMappend (mempty `mappend`)
    it "random `mappend` mempty == mempty" $ do
      checkMappend (`mappend` mempty)
  describe "stateless" $ do
    it "returns a stateless Binding" $ do
      out <- newStrRef
      let b = WB.stateless [outOn out SIa 'A', outOn out SIb 'B']
      WB.boundAction b (SS "") SIc `shouldSatisfy` isNothing
      void $ WB.actDo $ fromJust $ WB.boundAction b (SS "") SIa
      readIORef out `shouldReturn` "A"
      void $ WB.actDo $ fromJust $ WB.boundAction b (SS "") SIb
      readIORef out `shouldReturn` "BA"
