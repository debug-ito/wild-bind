{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
-- |
-- Module: WildBind.Indicator
-- Description: Graphical indicator for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module exports the 'Indicator', a graphical interface that
-- explains the current bindings to the user. The 'Indicator' uses
-- 'optBindingHook' in 'Option' to receive the current bindings from
-- wild-bind.
module WildBind.Indicator
    ( -- * Construction
      withNumPadIndicator
      -- * Execution
    , wildBindWithIndicator
      -- * Low-level function
    , bindingHook
      -- * Indicator type and its actions
    , Indicator
    , updateDescription
    , getPresence
    , setPresence
    , togglePresence
    , quit
      -- ** Conversion
    , adaptIndicator
      -- ** Binding
    , toggleBinding
      -- * Generalization of number pad types
    , NumPadPosition (..)
    ) where

import           Control.Applicative          ((<$>))
import           Control.Concurrent           (newEmptyMVar, putMVar, rtsSupportsBoundThreads,
                                               takeMVar)
import           Control.Concurrent.Async     (withAsync)
import           Control.Exception            (finally, throwIO)
import           Control.Monad                (forM_, void)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Reader   (ReaderT, ask, runReaderT)
import           Data.IORef                   (newIORef, readIORef)
import qualified Data.Map                     as M
import           Data.Monoid                  (First (First), mconcat)
import           Data.Text                    (Text, pack)
import           Data.Word                    (Word32)
import           System.Environment           (getArgs)
import           System.IO                    (hPutStrLn, stderr)

import           WildBind                     (Action (Action), ActionDescription, Binding,
                                               Binding', FrontEnd (frontDefaultDescription),
                                               Option (optBindingHook), binding, defOption,
                                               wildBind')
import           WildBind.Input.NumPad        (NumPadLocked (..), NumPadUnlocked (..))

import           Paths_wild_bind_indicator    (getDataFileName)


---- Imports about Gtk
import           GI.Gdk.Functions             (threadsAddIdle)
import           GI.GLib.Constants            (pattern PRIORITY_DEFAULT)
import           GI.Gtk                       (AttrOp ((:=)))
import qualified GI.Gtk                       as GIAttr (get, on, set)
import           GI.Gtk.Enums                 (Justification (..), WindowType (..))
import qualified GI.Gtk.Functions             as GIFunc
import           GI.Gtk.Objects.Button        (buttonNew, buttonSetAlignment)
import           GI.Gtk.Objects.CheckMenuItem (checkMenuItemNewWithMnemonic, checkMenuItemSetActive)
import           GI.Gtk.Objects.Container     (containerAdd)
import           GI.Gtk.Objects.Label         (Label, labelNew, labelSetJustify, labelSetLineWrap,
                                               labelSetText)
import           GI.Gtk.Objects.Menu          (Menu, menuNew, menuPopup)
import           GI.Gtk.Objects.MenuItem      (menuItemNewWithMnemonic)
import           GI.Gtk.Objects.Misc          (miscSetAlignment)
import           GI.Gtk.Objects.StatusIcon    (statusIconNewFromFile)
import           GI.Gtk.Objects.Table         (Table, tableAttachDefaults, tableNew)
import           GI.Gtk.Objects.Widget        (Widget, widgetHide, widgetSetSizeRequest,
                                               widgetShowAll)
import           GI.Gtk.Objects.Window        (Window, windowMove, windowNew, windowSetKeepAbove,
                                               windowSetTitle)


-- | Indicator interface. @s@ is the front-end state, @i@ is the input
-- type.
data Indicator s i
  = Indicator
      { updateDescription :: i -> ActionDescription -> IO ()
        -- ^ Update and show the description for the current binding.
      , getPresence       :: IO Bool
        -- ^ Get the current presence of the indicator. Returns 'True' if
        -- it's present.
      , setPresence       :: Bool -> IO ()
        -- ^ Set the presence of the indicator.
      , quit              :: IO ()
        -- ^ Destroy the indicator. This usually means quitting the entire
        -- application.
      , allButtons        :: [i]
        -- ^ list of all buttons on which the indicator displays
        -- descriptions.
      }

-- | Toggle the presence of the indicator.
togglePresence :: Indicator s i -> IO ()
togglePresence ind = (setPresence ind . not) =<< getPresence ind

-- | Convert actions in the input 'Indicator' so that those actions
-- can be executed from a non-GTK-main thread.
transportIndicator :: Indicator s i -> Indicator s i
transportIndicator ind = ind { updateDescription = \i d -> postGUIAsync $ updateDescription ind i d,
                               getPresence = postGUISync $ getPresence ind,
                               setPresence = \visible -> postGUIAsync $ setPresence ind visible,
                               quit = postGUISync $ quit ind
                             }


-- | Something that can be mapped to number pad's key positions.
class NumPadPosition a where
  toNumPad :: a -> NumPadLocked

instance NumPadPosition NumPadLocked where
  toNumPad = id

instance NumPadPosition NumPadUnlocked where
  toNumPad input = case input of
    NumInsert   -> NumL0
    NumEnd      -> NumL1
    NumDown     -> NumL2
    NumPageDown -> NumL3
    NumLeft     -> NumL4
    NumCenter   -> NumL5
    NumRight    -> NumL6
    NumHome     -> NumL7
    NumUp       -> NumL8
    NumPageUp   -> NumL9
    NumDivide   -> NumLDivide
    NumMulti    -> NumLMulti
    NumMinus    -> NumLMinus
    NumPlus     -> NumLPlus
    NumEnter    -> NumLEnter
    NumDelete   -> NumLPeriod

-- | Data type keeping read-only config for NumPadIndicator.
data NumPadConfig
  = NumPadConfig
      { confButtonWidth, confButtonHeight :: Int
      , confWindowX, confWindowY          :: Int
      , confIconPath                      :: FilePath
      }

numPadConfig :: IO NumPadConfig
numPadConfig = do
  icon <- getDataFileName "icon.svg"
  return NumPadConfig
    { confButtonWidth = 70,
      confButtonHeight = 45,
      confWindowX = 0,
      confWindowY = 0,
      confIconPath = icon
    }

-- | Contextual monad for creating NumPadIndicator
type NumPadContext = ReaderT NumPadConfig IO

-- | Initialize the indicator and run the given action.
--
-- The executable must be compiled by ghc with __@-threaded@ option enabled.__
-- Otherwise, it aborts.
withNumPadIndicator :: (NumPadPosition i, Enum i, Bounded i) => (Indicator s i -> IO ()) -> IO ()
withNumPadIndicator action = if rtsSupportsBoundThreads then impl else error_impl where
  error_impl = throwIO $ userError "You need to build with -threaded option when you use WildBind.Indicator.withNumPadIndicator function."
  textArgs = fmap (map pack) $ getArgs
  impl = do
    void $ (GIFunc.init . Just) =<< textArgs
    conf <- numPadConfig
    indicator <- createMainWinAndIndicator conf
    status_icon <- createStatusIcon conf indicator
    status_icon_ref <- newIORef status_icon
    withAsync (asyncAction indicator) $ \_ -> do
      GIFunc.main
      void $ readIORef status_icon_ref -- to prevent status_icon from being garbage-collected. See https://github.com/gtk2hs/gtk2hs/issues/60
  createMainWinAndIndicator conf = flip runReaderT conf $ do
    win <- newNumPadWindow
    (tab, updater) <- newNumPadTable
    containerAdd win tab
    let indicator = Indicator
          { updateDescription = \i d -> updater i d,
            getPresence = GIAttr.get win #visible,
            setPresence = \visible -> if visible then widgetShowAll win else widgetHide win,
            quit = GIFunc.mainQuit,
            allButtons = enumFromTo minBound maxBound
          }
    void $ GIAttr.on win #deleteEvent $ \_ -> do
      widgetHide win
      return True -- Do not emit 'destroy' signal
    return indicator
  asyncAction indicator =
    (action $ transportIndicator indicator) `finally` (postGUIAsync GIFunc.mainQuit)
  createStatusIcon conf indicator = do
    status_icon <- statusIconNewFromFile $ confIconPath conf
    void $ GIAttr.on status_icon #popupMenu $ \button time -> do
      menu <- makeStatusMenu indicator
      menuPopup menu (Nothing :: Maybe Widget) (Nothing :: Maybe Widget) Nothing button time
    return status_icon


-- | Run 'WildBind.wildBind' with the given 'Indicator'. 'ActionDescription's
-- are shown by the 'Indicator'.
wildBindWithIndicator :: Ord i => Indicator s i -> Binding s i -> FrontEnd s i -> IO ()
wildBindWithIndicator ind b front = wildBind' (defOption { optBindingHook = bindingHook ind front }) b front

-- | Create an action appropriate for 'optBindingHook' in 'Option'
-- from 'Indicator' and 'FrontEnd'.
bindingHook :: Ord i => Indicator s1 i -> FrontEnd s2 i -> [(i, ActionDescription)] -> IO ()
bindingHook ind front bind_list = forM_ (allButtons ind) $ \input -> do
  let desc = M.findWithDefault (frontDefaultDescription front input) input (M.fromList bind_list)
  updateDescription ind input desc


newNumPadWindow :: NumPadContext Window
newNumPadWindow = do
  win <- windowNew WindowTypeToplevel
  windowSetKeepAbove win True
  GIAttr.set win [ #skipPagerHint := True,
                   #skipTaskbarHint := True,
                   #acceptFocus := False,
                   #focusOnMap := False
                 ]
  windowSetTitle win "WildBind Description"
  win_x <- (fromIntegral . confWindowX) <$> ask
  win_y <- (fromIntegral . confWindowY) <$> ask
  windowMove win win_x win_y
  return win

-- | Get the action to describe @i@, if that @i@ is supported. This is
-- a 'Monoid', so we can build up the getter by 'mconcat'.
type DescriptActionGetter i = i -> First (ActionDescription -> IO ())

newNumPadTable :: NumPadPosition i => NumPadContext (Table, (i -> ActionDescription -> IO ()))
newNumPadTable = do
  tab <- tableNew 5 4 False
  -- NumLock is unboundable, so it's treatd in a different way from others.
  (\label -> labelSetText label "NumLock") =<< addButton tab 0 1 0 1
  descript_action_getter <-
    fmap mconcat $ sequence $
      [ getter NumLDivide $ addButton tab 1 2 0 1,
        getter NumLMulti $ addButton tab 2 3 0 1,
        getter NumLMinus $ addButton tab 3 4 0 1,
        getter NumL7 $ addButton tab 0 1 1 2,
        getter NumL8 $ addButton tab 1 2 1 2,
        getter NumL9 $ addButton tab 2 3 1 2,
        getter NumLPlus $ addButton tab 3 4 1 3,
        getter NumL4 $ addButton tab 0 1 2 3,
        getter NumL5 $ addButton tab 1 2 2 3,
        getter NumL6 $ addButton tab 2 3 2 3,
        getter NumL1 $ addButton tab 0 1 3 4,
        getter NumL2 $ addButton tab 1 2 3 4,
        getter NumL3 $ addButton tab 2 3 3 4,
        getter NumLEnter $ addButton tab 3 4 3 5,
        getter NumL0 $ addButton tab 0 2 4 5,
        getter NumLPeriod $ addButton tab 2 3 4 5
      ]
  let description_updater = \input -> case descript_action_getter $ toNumPad input of
        First (Just act) -> act
        First Nothing    -> const $ return ()
  return (tab, description_updater)
  where
    getter :: Eq i => i -> NumPadContext Label -> NumPadContext (DescriptActionGetter i)
    getter bound_key get_label = do
      label <- get_label
      return $ \in_key -> First (if in_key == bound_key then Just $ labelSetText label else Nothing)

addButton :: Table -> Word32 -> Word32 -> Word32 -> Word32 -> NumPadContext Label
addButton tab left right top bottom = do
  lab <- labelNew Nothing
  labelSetLineWrap lab True
  miscSetAlignment lab 0 0.5
  labelSetJustify lab JustificationLeft
  button <- buttonNew
  buttonSetAlignment button 0 0.5
  containerAdd button lab
  tableAttachDefaults tab button left right top bottom
  bw <- (fromIntegral . confButtonWidth) <$> ask
  bh <- (fromIntegral . confButtonHeight) <$> ask
  widgetSetSizeRequest lab (bw * fromIntegral (right - left)) (bh * fromIntegral (bottom - top))
  return lab

makeStatusMenu :: Indicator s i -> IO Menu
makeStatusMenu ind = impl where
  impl = do
    menu <- menuNew
    containerAdd menu =<< makeQuitItem
    containerAdd menu =<< makeToggler
    return menu
  makeQuitItem = do
    quit_item <- menuItemNewWithMnemonic "_Quit"
    widgetShowAll quit_item
    void $ GIAttr.on quit_item #activate (quit ind)
    return quit_item
  makeToggler = do
    toggler <- checkMenuItemNewWithMnemonic "_Toggle description"
    widgetShowAll toggler
    checkMenuItemSetActive toggler =<< getPresence ind
    void $ GIAttr.on toggler #toggled (togglePresence ind)
    return toggler

-- | Map input type of 'Indicator', so that it can adapt to the new
-- input type @i'@.
--
-- If the contra-mapper function returns 'Nothing', those input
-- symbols are ignored by the 'Indicator'.
--
-- @since 0.2.0.0
adaptIndicator :: (i -> i') -- ^ mapper function
               -> (i' -> Maybe i) -- ^ contra-mapper function
               -> Indicator s i -- ^ original
               -> Indicator s i' -- ^ adapted indicator
adaptIndicator mapper cmapper ind =
  ind { updateDescription = newDesc,
        allButtons = map mapper $ allButtons ind
      }
  where
    newDesc input = case cmapper input of
      Nothing         -> const $ return ()
      Just orig_input -> updateDescription ind orig_input

-- | A binding that toggles presence of the 'Indicator'.
--
-- @since 0.2.0.0
toggleBinding :: (NumPadPosition i, Ord i, Enum i, Bounded i)
              => Indicator s i
              -> NumPadLocked -- ^ the button to bind the 'togglePresence' action
              -> Binding' bs fs i
toggleBinding ind button = binding $ map (\input -> (input, Action "Toggle description" $ togglePresence ind)) help_likes
  where
    help_likes = filter ((== button) . toNumPad) $ enumFromTo minBound maxBound


-- | Schedule the given action to be executed by Gdk. The given action
-- can include manipulation of Gtk+ objects. This function can be
-- called by a thread that is different from the Gtk+ main loop
-- thread. This function doesn't wait for the given action to finish.
--
-- See https://github.com/haskell-gi/haskell-gi/wiki/Using-threads-in-Gdk-and-Gtk--programs
postGUIAsync :: IO () -> IO ()
postGUIAsync action = void $ threadsAddIdle PRIORITY_DEFAULT (action >> return False)

-- | Similar to 'postGUIAsync', but this function waits for the action
-- to finish.
postGUISync :: IO a -> IO a
postGUISync action = do
  mret <- newEmptyMVar
  postGUIAsync $ do
    ret <- action
    putMVar mret ret
  takeMVar mret
