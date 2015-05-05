-- |
-- Module: WildBind.X11.Internal.Window
-- Description: types and functions related to X11 windows
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE OverloadedStrings #-}
module WildBind.X11.Internal.Window (
  Window(winName,winDesc),
  ActiveWindow,
  getActiveWindow
) where

import Data.Text (Text)
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibE

import WildBind (FrontState)

-- | Information about window
data Window = Window {
  winName :: Text, -- ^ name of the window
  winDesc :: Text  -- ^ description of the window
} deriving (Eq,Ord,Show)
instance FrontState Window

-- | Use this type especially when the 'Window' is active.
type ActiveWindow = Window

-- | An empty Window instance used for fallback
emptyWindow :: Window
emptyWindow = Window "" ""

-- | Get currently active 'Window'.
getActiveWindow :: Xlib.Display -> IO ActiveWindow
getActiveWindow = undefined


-- | Check whether specified feature is supported by the window
-- manager(?) Port of libxdo's _xdo_ewmh_is_supported() function.
ewmhIsSupported :: Xlib.Display -> String -> IO Bool
ewmhIsSupported disp feature_str = do
  req <- Xlib.internAtom disp "_NET_SUPPORTED" False
  feature <- Xlib.internAtom disp feature_str False
  result <- XlibE.getWindowProperty32 disp req (Xlib.defaultRootWindow disp)
  case result of
    Nothing -> return False
    Just atoms -> return $ any ((feature ==) . fromIntegral) atoms
