-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Maybe this should be released as a separate package.
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module WildBind.X11 (
  X11Handle,
  ActiveWindow,
  initX11Handle
) where

import Control.Applicative ((<$>))

import Graphics.X11.Xlib (Display, Window, XEventPtr)
import qualified Graphics.X11.Xlib as Xlib
import Data.Text (Text)

import WildBind (FrontState, FrontInputDevice(..), FrontEventSource(..), FrontEvent(FEInput))
import WildBind.NumPad (NumPadUnlockedInput(..))

data ActiveWindow = ActiveWindow {
  awName :: Text,
  awDesc :: Text
} deriving (Eq,Ord,Show)
instance FrontState ActiveWindow

emptyActiveWindow :: ActiveWindow
emptyActiveWindow = ActiveWindow "" ""

data X11Handle = X11Handle {
  x11Display :: Display
}

x11RootWindow :: X11Handle -> Window
x11RootWindow = Xlib.defaultRootWindow . x11Display

initX11Handle :: IO X11Handle
initX11Handle = X11Handle <$> Xlib.openDisplay ""

keySymToNumPadUnlockedInput :: Xlib.KeySym -> Maybe NumPadUnlockedInput
keySymToNumPadUnlockedInput k
  | k == Xlib.xK_KP_Up = Just NumUp
  | k == Xlib.xK_KP_Down = Just NumDown
  | k == Xlib.xK_KP_Left = Just NumLeft
  | k == Xlib.xK_KP_Right = Just NumRight
  | k == Xlib.xK_KP_Home = Just NumHome
  | k == Xlib.xK_KP_Page_Up = Just NumPageUp
  | k == Xlib.xK_KP_Page_Down = Just NumPageDown
  | k == Xlib.xK_KP_End = Just NumEnd
  | k == Xlib.xK_KP_Begin = Just NumCenter
  | k == Xlib.xK_KP_Insert = Just NumInsert
  | k == Xlib.xK_KP_Delete = Just NumDelete
  | k == Xlib.xK_KP_Enter = Just NumEnter
  | k == Xlib.xK_KP_Divide = Just NumDivide
  | k == Xlib.xK_KP_Multiply = Just NumMulti
  | k == Xlib.xK_KP_Subtract = Just NumMinus
  | k == Xlib.xK_KP_Add = Just NumPlus
  | otherwise = Nothing

convertEvent :: XEventPtr -> IO (Maybe NumPadUnlockedInput)
convertEvent xev = convert' =<< Xlib.get_EventType xev
  where
    convert' xtype
      | xtype == Xlib.keyRelease = do
        mkeysym <- fst <$> (Xlib.lookupString $ Xlib.asKeyEvent xev)
        return (keySymToNumPadUnlockedInput =<< mkeysym)
      | otherwise = return Nothing

instance FrontInputDevice X11Handle NumPadUnlockedInput where
  defaultActionDescription = undefined
  setGrab = undefined

instance FrontEventSource X11Handle ActiveWindow NumPadUnlockedInput where
  nextEvent handle = Xlib.allocaXEvent $ \xev -> do
    Xlib.nextEvent (x11Display handle) xev
    maybe (nextEvent handle) (return . FEInput emptyActiveWindow) =<< convertEvent xev
    
