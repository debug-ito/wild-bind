{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: WildBind.X11.Internal.GrabMan
-- Description: internal key grab manager
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. Package users should not rely on this.__
module WildBind.X11.Internal.GrabMan
       ( GrabMan,
         new,
         set,
         unset,
         onRef
       ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Monoid (Monoid(..), (<>))

import WildBind.X11.Internal.Key (XKeyEvent(..), press, KeyEventType(..))

data GrabEvent =
  GrabEvent
  { grabPress :: Bool,
    grabRelease :: Bool
  }
  deriving (Show,Eq,Ord)

instance Monoid GrabEvent where
  mempty = GrabEvent False False
  mappend a b = GrabEvent { grabPress = (grabPress a) || (grabPress b),
                            grabRelease = (grabRelease a) || (grabRelease b)
                          }

grabFor :: XKeyEvent -> GrabEvent
grabFor e = case xKeyEventType e of
  KeyPress -> mempty { grabPress = True }
  KeyRelease -> mempty { grabRelease = True }

unsetGrabEvent :: XKeyEvent -> GrabEvent -> GrabEvent
unsetGrabEvent e ge = case xKeyEventType e of
  KeyPress -> ge { grabPress = False }
  KeyRelease -> ge { grabRelease = False }

newtype GrabMan = GrabMan (M.Map XKeyEvent GrabEvent)
                deriving (Monoid,Show,Eq,Ord)

new :: IO (IORef GrabMan)
new = newIORef mempty

set :: XKeyEvent -> GrabMan -> (GrabMan, Bool)
set raw_input (GrabMan gm) = (GrabMan new_gm, do_set_grab)
  where
    input = press raw_input
    do_set_grab = M.member input gm
    new_gm = M.insertWith (<>) input (grabFor raw_input) gm

unset :: XKeyEvent -> GrabMan -> (GrabMan, Bool)
unset raw_input (GrabMan gm) = (GrabMan new_gm, do_unset_grab)
  where
    input = press raw_input
    (new_gm, do_unset_grab) = case M.lookup input gm of
      Nothing -> (gm, False)
      Just cur_ge -> let new_ge = unsetGrabEvent raw_input cur_ge
                         removed = new_ge == mempty
                     in ( if removed
                          then M.delete input gm
                          else M.insert input new_ge gm,
                          removed
                        )

onRef :: IORef GrabMan -> (GrabMan -> (GrabMan, a)) -> IO a
onRef gm_ref f = do
  cur_gm <- readIORef gm_ref
  let (new_gm, ret) = f cur_gm
  writeIORef gm_ref new_gm
  return ret

