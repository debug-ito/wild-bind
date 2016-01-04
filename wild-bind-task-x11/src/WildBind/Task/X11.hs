-- |
-- Module: WildBind.Task.X11
-- Description: Task to install and export everything you need to use WildBind in X11
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
-- This module exports everything you probably need to use WildBind in
-- X11 environments.
module WildBind.Task.X11 (
  -- * Execution
  wildNumPad,
  wildNumPad',
  -- * Re-exports
  module WildBind,
  module WildBind.X11,
  module WildBind.Indicator,
  module Data.Monoid
) where

import WildBind
import WildBind.X11
import WildBind.Indicator
import Data.Monoid

import WildBind.X11.Internal.Key (KeySymLike, ModifierLike)

-- | A convenient function to create an executable action with X11
-- 'FrontEnd', 'Indicator' for a number pad.
--
-- With this function, the Enter key is bound to toggling the
-- 'Indicator', ignoring the binding you provide.
--
-- For the input type @i@, you can use 'NumPadUnlockedInput' or
-- 'NumPadLockedInput'.
wildNumPad :: (NumPadPosition i, KeySymLike i, ModifierLike i, Describable i, Ord i, Enum i, Bounded i)
              => Binding ActiveWindow i -> IO ()
wildNumPad orig_binding = do
  let enter_likes = filter ((== NumLEnter) . toNumPad) $ enumFromTo minBound maxBound
      enter_binds ind = binds $ map (\input -> on input "Toggle Indicator" $ togglePresence ind) enter_likes
  wildNumPad' $ \ind -> orig_binding <> enter_binds ind

-- | A more flexible version of 'wildNumPad'. It passes you an
-- 'Indicator', and uses the 'Binding' you return as-is.
wildNumPad' :: (NumPadPosition i, KeySymLike i, ModifierLike i, Describable i, Ord i, Enum i, Bounded i)
               => (Indicator ActiveWindow i -> Binding ActiveWindow i) -> IO ()
wildNumPad' create_binding = withNumPadIndicator $ \ind -> withFrontEnd $ wildBindWithIndicator ind (create_binding ind)
