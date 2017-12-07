module WildBind.ForTest
       ( SampleInput(..),
         SampleState(..),
         SampleBackState(..),
         inputAll,
         execAll,
         evalStateEmpty,
         boundDescs,
         curBoundInputs,
         curBoundDescs
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as State
import Test.QuickCheck (Arbitrary(arbitrary,shrink), arbitraryBoundedEnum)
import Test.Hspec (shouldReturn)

import qualified WildBind.Binding as WB
import qualified WildBind.Description as WBD

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

evalStateEmpty :: State.StateT (WB.Binding SampleState SampleInput) IO () -> IO ()
evalStateEmpty s = State.evalStateT s mempty

boundDescs :: WB.Binding s i -> s -> [(i, WBD.ActionDescription)]
boundDescs b s = map toDesc $ WB.boundActions b s
  where
    toDesc (i, act) = (i, WB.actDescription act)

curBoundInputs :: s -> State.StateT (WB.Binding s i) IO [i]
curBoundInputs s = State.gets WB.boundInputs <*> pure s

curBoundDescs :: s -> State.StateT (WB.Binding s i) IO [(i, WBD.ActionDescription)]
curBoundDescs s = State.gets boundDescs <*> pure s
