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
import System.Environment (getEnv)
import Test.Hspec

isDisplaySet :: IO Bool
isDisplaySet = (/= "") <$> getEnv "DISPLAY"

checkIfX11Available :: Spec -> Spec
checkIfX11Available = before $ do
  available <- isDisplaySet
  when (not available) $ pendingWith "DISPLAY env is not set. Skipped."
