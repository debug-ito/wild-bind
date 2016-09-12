-- |
-- Module: WildBind.X11.Internal.Key
-- Description: types and functions related to key symbols and their conversion
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. Package users should not rely on this.__
module WildBind.X11.Internal.Key
       ( -- * Conversion between key types
         KeySymLike(..), 
         xEventToKeySymLike,
         -- * Key grabs
         ModifierLike,
         xGrabKey, xUngrabKey
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Data.Bits ((.|.))
import qualified Data.Bits as Bits
import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe)
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibE

import qualified WildBind.Input.NumPad as NumPad

-- | Convertible to/from Xlib's 'KeySym'
--
-- prop> fromKeySym . toKeySym == Just
class KeySymLike k where
  fromKeySym :: Xlib.KeySym -> Maybe k
  toKeySym :: k -> Xlib.KeySym

instance KeySymLike Xlib.KeySym where
  fromKeySym = Just
  toKeySym = id

fromKeySymDef :: (Bounded k, Enum k) => (k -> Xlib.KeySym) -> Xlib.KeySym -> Maybe k
fromKeySymDef to_conv ks = M.lookup ks $ M.fromList $ map (\n -> (to_conv n, n)) $ enumFromTo minBound maxBound 

instance KeySymLike NumPad.NumPadUnlocked where
  fromKeySym = fromKeySymDef toKeySym
  toKeySym n = case n of
    NumPad.NumUp -> Xlib.xK_KP_Up
    NumPad.NumDown -> Xlib.xK_KP_Down
    NumPad.NumLeft -> Xlib.xK_KP_Left
    NumPad.NumRight -> Xlib.xK_KP_Right
    NumPad.NumHome -> Xlib.xK_KP_Home
    NumPad.NumPageUp -> Xlib.xK_KP_Page_Up
    NumPad.NumPageDown -> Xlib.xK_KP_Page_Down
    NumPad.NumEnd -> Xlib.xK_KP_End
    NumPad.NumCenter -> Xlib.xK_KP_Begin
    NumPad.NumInsert -> Xlib.xK_KP_Insert
    NumPad.NumDelete -> Xlib.xK_KP_Delete
    NumPad.NumEnter -> Xlib.xK_KP_Enter
    NumPad.NumDivide -> Xlib.xK_KP_Divide
    NumPad.NumMulti -> Xlib.xK_KP_Multiply
    NumPad.NumMinus -> Xlib.xK_KP_Subtract
    NumPad.NumPlus -> Xlib.xK_KP_Add

instance KeySymLike NumPad.NumPadLocked where
  -- Xlib handles the [(.) (Delete)] key in a weird way. In the input
  -- event, it brings XK_KP_Decimal when NumLock enabled, XK_KP_Delete
  -- when NumLock disabled. However, XKeysymToKeycode() function won't
  -- return the correct keycode for XK_KP_Decimal. (I'm not sure how
  -- much this behavior depends on user's environment...) As a
  -- workaround in this instance, we map NumLPeriod -> XK_KP_Delete,
  -- but in the reverse map, we also respond to XK_KP_Decimal.
  fromKeySym ks | ks == Xlib.xK_KP_Decimal = Just NumPad.NumLPeriod
                | otherwise                = (fromKeySymDef toKeySym) ks
  toKeySym n = case n of
    NumPad.NumL0 -> Xlib.xK_KP_0
    NumPad.NumL1 -> Xlib.xK_KP_1
    NumPad.NumL2 -> Xlib.xK_KP_2
    NumPad.NumL3 -> Xlib.xK_KP_3
    NumPad.NumL4 -> Xlib.xK_KP_4
    NumPad.NumL5 -> Xlib.xK_KP_5
    NumPad.NumL6 -> Xlib.xK_KP_6
    NumPad.NumL7 -> Xlib.xK_KP_7
    NumPad.NumL8 -> Xlib.xK_KP_8
    NumPad.NumL9 -> Xlib.xK_KP_9
    NumPad.NumLDivide -> Xlib.xK_KP_Divide
    NumPad.NumLMulti -> Xlib.xK_KP_Multiply
    NumPad.NumLMinus -> Xlib.xK_KP_Subtract
    NumPad.NumLPlus -> Xlib.xK_KP_Add
    NumPad.NumLEnter -> Xlib.xK_KP_Enter
    NumPad.NumLPeriod -> Xlib.xK_KP_Delete
    -- XKeysymToKeycode() didn't return the correct keycode for XK_KP_Decimal in numpaar code...


-- | Extract the KeySym associated with the XEvent.
xEventToKeySym :: Xlib.XEventPtr -> MaybeT IO Xlib.KeySym
xEventToKeySym xev = MaybeT (fst <$> (Xlib.lookupString $ Xlib.asKeyEvent xev))

-- | Extract the 'KeySymLike' associated with the XEvent.
xEventToKeySymLike :: KeySymLike k => Xlib.XEventPtr -> MaybeT IO k
xEventToKeySymLike xev = (MaybeT . return . fromKeySym) =<< xEventToKeySym xev

-- | Internal abstract of key modifiers
data ModifierKey = ModNumLock deriving (Eq,Ord,Show,Bounded,Enum)

-- | Convertible into a set of Modifiers.
class ModifierLike k where
  toModifiers :: k -> [ModifierKey]

instance ModifierLike NumPad.NumPadUnlocked where
  toModifiers _ = []

instance ModifierLike NumPad.NumPadLocked where
  toModifiers _ = [ModNumLock]

-- | Convert a 'KeySymLike' into a KeyCode and ButtonMask for grabbing.
xKeyCode :: (KeySymLike k, ModifierLike k) => Xlib.Display -> k -> IO (Xlib.KeyCode, Xlib.ButtonMask)
xKeyCode disp key = (,) <$> Xlib.keysymToKeycode disp (toKeySym key) <*> createMask disp (toModifiers key)

createMask :: Xlib.Display -> [ModifierKey] -> IO Xlib.ButtonMask
createMask _ [] = return 0
createMask disp (modkey:rest) = do
  modifier_index <- fromIntegral <$> getXModifier disp modkey
  (Bits.shift 1 modifier_index .|.) <$> createMask disp rest

type XModifierMap = [(Xlib.Modifier, [Xlib.KeyCode])]

getXModifierMap :: Xlib.Display -> IO XModifierMap
getXModifierMap = XlibE.getModifierMapping

-- | Look up modifier for the given 'ModifierKey'. This is necessary
-- especially for NumLock modifier, because it is highly dynamic in
-- KeyCode realm. If no modifier is associated with the 'ModifierKey',
-- it returns 0.
--
-- c.f:
--
-- * grab_key.c of xbindkey package
-- * http://tronche.com/gui/x/xlib/input/keyboard-grabbing.html
-- * http://tronche.com/gui/x/xlib/input/keyboard-encoding.html
lookupXModifier :: Xlib.Display -> XModifierMap -> ModifierKey -> IO Xlib.Modifier
lookupXModifier disp xmmap ModNumLock = do
  numlock_code <- Xlib.keysymToKeycode disp Xlib.xK_Num_Lock
  return $ maybe 0 id $ listToMaybe $ mapMaybe (lookupXMod' numlock_code) xmmap
  where
    lookupXMod' key_code (xmod, codes) = if key_code `elem` codes
                                         then Just xmod
                                         else Nothing

getXModifier :: Xlib.Display -> ModifierKey -> IO Xlib.Modifier
getXModifier disp key = do
  xmmap <- getXModifierMap disp
  lookupXModifier disp xmmap key

-----

-- | Grab the specified key on the specified window. The key is
-- captured from now on, so the window won't get that.
xGrabKey :: (KeySymLike k, ModifierLike k) => Xlib.Display -> Xlib.Window -> k -> IO ()
xGrabKey disp win key = do
  (code, mask) <- xKeyCode disp key
  Xlib.grabKey disp code mask win False Xlib.grabModeAsync Xlib.grabModeAsync

-- grabKey throws an exception if that key for the window is already
-- grabbed by another X client. For now, we don't handle that
-- exception.

-- | Release the grab on the specified key.
xUngrabKey :: (KeySymLike k, ModifierLike k) => Xlib.Display -> Xlib.Window -> k -> IO ()
xUngrabKey disp win key = do
  (code, mask) <- xKeyCode disp key
  Xlib.ungrabKey disp code mask win
