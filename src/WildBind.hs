-- |
-- Module: WildBind
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module WildBind (
  
) where

import Data.Text (Text)
import Data.Foldable (Foldable, forM_)

class Input i

class Input i => InputGenerator g i where
  inputTo :: g -> (i -> IO ()) -> IO ()

class Input i => WindBind w i where
  bind :: w -> i -> Maybe (IO ())

instance (Foldable f, Input i) => InputGenerator (f i) i where
  inputTo = forM_


-----

data SampleInput = SIA | SIB | SIC deriving (Eq,Ord,Show)

instance Input SampleInput


