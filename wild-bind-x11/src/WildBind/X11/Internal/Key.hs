-- |
-- Module: WildBind.X11.Internal.Key
-- Description: types and functions related to key symbols and their conversion
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. Package users should not rely on this.__
module WildBind.X11.Internal.Key
       ( -- * Key
         XKeyInput(..),
         xKeyEventToXKeyInput,
         -- * Modifiers
         KeyMaskMap(..),
         getKeyMaskMap,
         -- * Grabs
         xGrabKey,
         xUngrabKey,
         -- * High-level input symbols
         XModKey(..),
         XMod(..),
         ToXModKey(..),
         (.+)
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Data.Bits ((.|.), (.&.))
import qualified Data.Bits as Bits
import Data.Foldable (foldr, fold)
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Foreign
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibE

import WildBind.Description (Describable(..))
import qualified WildBind.Input.NumPad as NumPad

-- | 'Xlib.KeyMask' values assigned to each modifier keys/states. If
-- the modifier doesn't exist, the mask is 0.
data KeyMaskMap =
  KeyMaskMap
  { maskShift :: Xlib.KeyMask,
    maskControl :: Xlib.KeyMask,
    maskAlt :: Xlib.KeyMask,
    maskSuper :: Xlib.KeyMask,
    maskNumLock :: Xlib.KeyMask,
    maskCapsLock :: Xlib.KeyMask,
    maskShiftLock :: Xlib.KeyMask,
    maskScrollLock :: Xlib.KeyMask
  }
  deriving (Show,Eq,Ord)

-- | Class of data types that can be handled by X11. The data type can
-- tell X11 to grab key with optional modifiers, and it can be
-- extracted from a X11 Event object.
class XKeyInput k where
  toKeySym :: k -> Xlib.KeySym
  -- ^ Get the X11 keysym for this input.
  toModifierMasks :: KeyMaskMap -> k -> [Xlib.KeyMask]
  -- ^ Get modifers masks to grab the keysym. The grab action is
  -- repeated for all modifier masks. If the result is empty, X11
  -- grabs the keysym without any modifers.
  fromKeyEvent :: KeyMaskMap -> Xlib.KeySym -> Xlib.KeyMask -> Maybe k
  -- ^ Create the input object from a keysym and a modifier (got from
  -- XEvent.)

-- | Partial inverse of 'toKeySym'.
fromKeySymDef :: (Bounded k, Enum k) => (k -> Xlib.KeySym) -> Xlib.KeySym -> Maybe k
fromKeySymDef to_conv ks = M.lookup ks $ M.fromList $ map (\n -> (to_conv n, n)) $ enumFromTo minBound maxBound 

instance XKeyInput NumPad.NumPadUnlocked where
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
  toModifierMasks _ _ = []
  fromKeyEvent _ keysym _ = fromKeySymDef toKeySym keysym

instance XKeyInput NumPad.NumPadLocked where
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
    
  toModifierMasks kmmap _ = [maskNumLock kmmap]

  -- Xlib handles the [(.) (Delete)] key in a weird way. In the input
  -- event, it brings XK_KP_Decimal when NumLock enabled, XK_KP_Delete
  -- when NumLock disabled. However, XKeysymToKeycode() function won't
  -- return the correct keycode for XK_KP_Decimal. (I'm not sure how
  -- much this behavior depends on user's environment...) As a
  -- workaround in this instance, we map NumLPeriod -> XK_KP_Delete,
  -- but in the reverse map, we also respond to XK_KP_Decimal.
  fromKeyEvent _ keysym _ | keysym == Xlib.xK_KP_Decimal = Just NumPad.NumLPeriod
                          | otherwise = fromKeySymDef toKeySym keysym


-- | Extract the 'XKeyInput' from the XKeyEvent.
xKeyEventToXKeyInput :: XKeyInput k => KeyMaskMap -> Xlib.XKeyEventPtr -> MaybeT IO k
xKeyEventToXKeyInput kmmap kev = do
  keysym <- MaybeT (fst <$> Xlib.lookupString kev)
  (_, _, _, _, _, _, _, status, _, _) <- liftIO $ Xlib.get_KeyEvent $ Foreign.castPtr kev
  MaybeT $ return $ fromKeyEvent kmmap keysym status

-- | Convert a 'XKeyInput' into a KeyCode and KeyMasks for grabbing.
xKeyCode :: XKeyInput k => KeyMaskMap -> Xlib.Display -> k -> IO (Xlib.KeyCode, [Xlib.KeyMask])
xKeyCode kmmap disp key = (,) <$> keysym <*> masks
  where
    keysym = Xlib.keysymToKeycode disp $ toKeySym key
    masks = pure $ defaultMask $ toModifierMasks kmmap key
    defaultMask [] = [0]
    defaultMask ms = ms

type XModifierMap = [(Xlib.Modifier, [Xlib.KeyCode])]

-- | Get current 'KeyMaskMap'.
getKeyMaskMap :: Xlib.Display -> IO KeyMaskMap
getKeyMaskMap disp = do
  xmodmap <- getXModifierMap disp
  let maskFor = lookupModifierKeyMask disp xmodmap
  numlock_mask <- maskFor Xlib.xK_Num_Lock
  capslock_mask <- maskFor Xlib.xK_Caps_Lock
  shiftlock_mask <- maskFor Xlib.xK_Shift_Lock
  scrolllock_mask <- maskFor Xlib.xK_Scroll_Lock
  alt_mask <- maskFor Xlib.xK_Alt_L
  super_mask <- maskFor Xlib.xK_Super_L
  return KeyMaskMap { maskShift = Xlib.shiftMask,
                      maskControl = Xlib.controlMask,
                      maskAlt = alt_mask,
                      maskSuper = super_mask,
                      maskNumLock = numlock_mask,
                      maskCapsLock = capslock_mask,
                      maskShiftLock = shiftlock_mask,
                      maskScrollLock = scrolllock_mask
                    }

getXModifierMap :: Xlib.Display -> IO XModifierMap
getXModifierMap = XlibE.getModifierMapping

-- | Look up a modifier keymask associated to the given keysym. This
-- is necessary especially for NumLock modifier, because it is highly
-- dynamic in KeyCode realm. If no modifier is associated with the
-- 'ModifierKey', it returns 0.
--
-- c.f:
--
-- * grab_key.c of xbindkey package
-- * http://tronche.com/gui/x/xlib/input/keyboard-grabbing.html
-- * http://tronche.com/gui/x/xlib/input/keyboard-encoding.html
lookupModifierKeyMask :: Xlib.Display -> XModifierMap -> Xlib.KeySym -> IO Xlib.KeyMask
lookupModifierKeyMask disp xmmap keysym = do
  keycode <- Xlib.keysymToKeycode disp keysym
  return $ maybe 0 modifierToKeyMask $ listToMaybe $ mapMaybe (lookupXMod' keycode) xmmap
  where
    lookupXMod' key_code (xmod, codes) = if key_code `elem` codes
                                         then Just xmod
                                         else Nothing

modifierToKeyMask :: Xlib.Modifier -> Xlib.KeyMask
modifierToKeyMask = Bits.shift 1 . fromIntegral

-- | Grab the specified key on the specified window. The key is
-- captured from now on, so the window won't get that.
xGrabKey :: XKeyInput k => KeyMaskMap -> Xlib.Display -> Xlib.Window -> k -> IO ()
xGrabKey kmmap disp win key = do
  (code, masks) <- xKeyCode kmmap disp key
  forM_ masks $ \mask -> Xlib.grabKey disp code mask win False Xlib.grabModeAsync Xlib.grabModeAsync

-- grabKey throws an exception if that key for the window is already
-- grabbed by another X client. For now, we don't handle that
-- exception.

-- | Release the grab on the specified key.
xUngrabKey :: XKeyInput k => KeyMaskMap -> Xlib.Display -> Xlib.Window -> k -> IO ()
xUngrabKey kmmap disp win key = do
  (code, masks) <- xKeyCode kmmap disp key
  forM_ masks $ \mask -> Xlib.ungrabKey disp code mask win

-- | X11 key modifiers.
data XMod = Shift
          | Ctrl
          | Alt
          | Super
          deriving (Show,Eq,Ord)

-- | X11 keysym with a set of modifiers.
data XModKey = XModKey (S.Set XMod) Xlib.KeySym
             deriving (Show,Eq,Ord)

instance XKeyInput XModKey where
  toKeySym (XModKey _ ks) = ks
  toModifierMasks kmmap (XModKey mods _) =
    map (.|. xModsToKeyMask kmmap mods) $ lockVariations kmmap
  fromKeyEvent kmmap keysym mask = Just $ XModKey (keyMaskToXMods kmmap mask) keysym

class ToXModKey k where
  toXModKey :: k -> XModKey

instance ToXModKey XModKey where
  toXModKey = id

-- | KeySym with empty 'XMod' set.
instance ToXModKey Xlib.KeySym where
  toXModKey keysym = XModKey mempty keysym

instance Describable XModKey where
  describe (XModKey mods keysym) = T.pack (mods_str ++ Xlib.keysymToString keysym)
    where
      mods_str = fold $ S.map (\m -> show m ++ "+") mods

xModToKeyMask :: KeyMaskMap -> XMod -> Xlib.KeyMask
xModToKeyMask kmmap modi = case modi of
  Shift -> maskShift kmmap
  Ctrl -> maskControl kmmap
  Alt -> maskAlt kmmap
  Super -> maskSuper kmmap

xModsToKeyMask :: KeyMaskMap -> S.Set XMod -> Xlib.KeyMask
xModsToKeyMask kmmap = foldr f 0
  where
    f modi mask = xModToKeyMask kmmap modi .|. mask

lockVariations :: KeyMaskMap -> [Xlib.KeyMask]
lockVariations kmmap = nub $ do
  numl <- [0, maskNumLock kmmap]
  capsl <- [0, maskCapsLock kmmap]
  shiftl <- [0, maskShiftLock kmmap]
  scl <- [0, maskScrollLock kmmap]
  return (numl .|. capsl .|. shiftl .|. scl)

keyMaskToXMods :: KeyMaskMap -> Xlib.KeyMask -> S.Set XMod
keyMaskToXMods kmmap mask = S.fromList$ toXMod =<< [ (maskShift, Shift),
                                                      (maskControl, Ctrl),
                                                      (maskAlt, Alt),
                                                      (maskSuper, Super)
                                                    ]
  where
    toXMod (acc, mod_symbol) = if (mask .&. acc kmmap) == 0
                               then []
                               else [mod_symbol]

-- | Add a 'XMod' to 'XModKey' or 'Xlib.KeySym'.
(.+) :: ToXModKey k => XMod -> k -> XModKey
modi .+ mkey = case toXModKey mkey of
  XModKey mods ks -> XModKey (S.insert modi mods) ks

infixr 6 .+
