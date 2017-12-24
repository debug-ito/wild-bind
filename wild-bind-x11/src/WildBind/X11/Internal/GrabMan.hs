{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: WildBind.X11.Internal.GrabMan
-- Description: internal key grab manager
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. Package users should not rely on this.__
module WildBind.X11.Internal.GrabMan
       ( GrabMan,
         GrabOp(..),
         new,
         modify
       ) where

import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Foldable (foldr)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Set as S
import qualified Graphics.X11.Xlib as Xlib

import WildBind.X11.Internal.Key
  ( XKeyEvent(..), press, KeyEventType(..),
    KeyMaskMap, XKeyInput(..)
  )

type GrabField = (Xlib.KeySym, Xlib.KeyMask)

type GrabbedInputs k = M.Map GrabField (S.Set k)

insertG :: Ord k => GrabField -> k -> GrabbedInputs k -> (GrabbedInputs k, Bool)
insertG field key inputs = (new_inputs, is_new_entry)
  where
    is_new_entry = M.member field inputs
    new_inputs = M.insertWith S.union field (S.singleton key) inputs

deleteG :: Ord k => GrabField -> k -> GrabbedInputs k -> (GrabbedInputs k, Bool)
deleteG field key inputs = (new_inputs, is_entry_deleted)
  where
    (new_inputs, is_entry_deleted) = case M.lookup field inputs of
      Nothing -> (inputs, False)
      Just cur_grabbed -> let new_grabbed = S.delete key cur_grabbed
                              removed = new_grabbed == mempty
                          in ( if removed
                               then M.delete field inputs
                               else M.insert field new_grabbed inputs,
                               
                               removed
                             )

data GrabOp = DoSetGrab | DoUnsetGrab deriving (Show,Eq,Ord)

modifyG :: Ord k => GrabOp -> GrabField -> k -> GrabbedInputs k -> (GrabbedInputs k, Bool)
modifyG op = case op of
  DoSetGrab -> insertG
  DoUnsetGrab -> deleteG

data GrabMan k =
  GrabMan
  { gmKeyMaskMap :: KeyMaskMap,
    gmDisplay :: Xlib.Display,
    gmRootWindow :: Xlib.Window,
    gmGrabbedInputs :: GrabbedInputs k
  }
  deriving (Show,Eq,Ord)

new :: KeyMaskMap -> Xlib.Display -> Xlib.Window -> IO (IORef (GrabMan k))
new kmm disp win = newIORef $ GrabMan { gmKeyMaskMap = kmm,
                                        gmDisplay = disp,
                                        gmRootWindow = win,
                                        gmGrabbedInputs = mempty
                                      }

grabFieldsFor :: XKeyInput k => KeyMaskMap -> k -> NonEmpty GrabField
grabFieldsFor kmmap k = do
  sym <- return $ toKeySym k
  modmask <- toModifierMasks kmmap k
  return (sym, modmask)

modifyGM :: (XKeyInput k, Ord k) => GrabOp -> k -> GrabMan k -> (GrabMan k, [GrabField])
modifyGM op input gm = foldr modifySingle (gm, []) fields
  where
    fields = grabFieldsFor (gmKeyMaskMap gm) input
    modifySingle field (cur_gm, cur_changed) = (new_gm, new_changed)
      where
        (new_gi, modified) = modifyG op field input $ gmGrabbedInputs cur_gm
        new_gm = cur_gm { gmGrabbedInputs = new_gi }
        new_changed = if modified then (field : cur_changed) else cur_changed

modify :: (XKeyInput k, Ord k) => IORef (GrabMan k) -> GrabOp -> k -> IO ()
modify gm_ref op input = do
  cur_gm <- readIORef gm_ref
  let (new_gm, changed_fields) = modifyGM op input cur_gm
  writeIORef gm_ref new_gm
  return ()
  -- TODO: 

