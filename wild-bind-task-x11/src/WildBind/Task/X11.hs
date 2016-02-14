-- |
-- Module: WildBind.Task.X11
-- Description: Task to install and export everything you need to use WildBind in X11
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
-- This module exports everything you probably need to use WildBind in
-- X11 environments.
module WildBind.Task.X11
       ( -- * Execution
         wildNumPad,
         wildNumPad',
         -- * Re-exports
         module WildBind.Binding,
         module WildBind.Description,
         module WildBind.Input.NumPad,
         -- ** From basic modules
         module Control.Monad.IO.Class,
         module Control.Monad.Trans.State,
         module Data.Monoid,
         Text,
         -- ** From "WindBind.X11"
         Window, ActiveWindow, winInstance, winClass, winName,
         -- ** From "WildBind.Indicator"
         Indicator, NumPadPosition(..), updateDescription, getPresence, setPresence, togglePresence
       ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Monoid
import Data.Text (Text)

import WildBind.Binding
import WildBind.Description
import WildBind.Input.NumPad
import WildBind.X11
  ( Window, ActiveWindow, winInstance, winClass, winName,
    withFrontEnd
  )
import WildBind.Indicator
  ( Indicator, NumPadPosition(..),
    updateDescription, getPresence, setPresence, togglePresence,
    withNumPadIndicator, wildBindWithIndicator
  )
import WildBind.X11.Internal.Key (KeySymLike, ModifierLike)

-- | A convenient function to create an executable action with X11
-- 'FrontEnd' and 'Indicator' for a number pad.
--
-- > main :: IO ()
-- > main = wildNumPad $ binds [on NumCenter "test" $ putStrLn "You pushed center."]
-- 
-- Note that the executable must be compiled by ghc with
-- __@-threaded@ option enabled.__
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
