-- |
-- Module: WildBind.X11.TestUtil
-- Description:  
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module WildBind.X11.TestUtil
       ( checkIfX11Available
       ) where

import Control.Monad (when)
import Control.Applicative ((<$>))
import System.Environment (lookupEnv)
import Test.Hspec

isDisplaySet :: IO Bool
isDisplaySet = exists <$> lookupEnv "DISPLAY" where
  exists (Just v) = v /= ""
  exists _ = False

checkIfX11Available :: Spec -> Spec
checkIfX11Available = before $ do
  available <- isDisplaySet
  when (not available) $ pendingWith "DISPLAY env is not set. Skipped."
