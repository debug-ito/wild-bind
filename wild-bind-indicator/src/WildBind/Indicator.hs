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
         withNumPadIndicator,
         -- * Execution
         wildBindWithIndicator,
         -- * Low-level function
         bindingHook,
         -- * Indicator type and its actions
         Indicator,
         updateDescription,
         getPresence,
         setPresence,
         togglePresence,
         quit,
         -- * Generalization of number pad types
         NumPadPosition(..)
       ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkFinally, rtsSupportsBoundThreads)
import Control.Exception (throwIO)
import Control.Monad (void, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.IORef (newIORef, readIORef)
import qualified Data.Map as M
import Data.Monoid (mconcat, First(First))
import Data.Text (Text)
import Graphics.UI.Gtk
  ( initGUI, mainGUI, postGUIAsync, postGUISync, mainQuit,
    Window, windowNew, windowSetKeepAbove, windowSkipPagerHint,
    windowSkipTaskbarHint, windowAcceptFocus, windowFocusOnMap,
    windowSetTitle, windowMove,
    AttrOp((:=)),
    widgetShowAll, widgetSetSizeRequest, widgetVisible, widgetHide,
    Table, tableNew, tableAttachDefaults,
    buttonNew, buttonSetAlignment,
    Label, labelNew, labelSetLineWrap, labelSetJustify, Justification(JustifyLeft), labelSetText,
    miscSetAlignment,
    containerAdd,
    deleteEvent,
    statusIconNewFromFile, statusIconPopupMenu,
    Menu, menuNew, menuItemNewWithMnemonic, menuItemActivated, menuPopup,
    checkMenuItemNewWithMnemonic, checkMenuItemSetActive, checkMenuItemToggled
  )
import qualified Graphics.UI.Gtk as G (get, set, on)
import System.IO (stderr, hPutStrLn)

import WildBind ( ActionDescription, Option(optBindingHook),
                  FrontEnd(frontDefaultDescription), Binding,
                  wildBind', defOption
                )
import WildBind.Input.NumPad (NumPadUnlocked(..), NumPadLocked(..))

import Paths_wild_bind_indicator (getDataFileName)


-- | Indicator interface. @s@ is the front-end state, @i@ is the input
-- type.
data Indicator s i =
  Indicator
  { updateDescription :: i -> ActionDescription -> IO (),
    -- ^ Update and show the description for the current binding.
    
    getPresence :: IO Bool,
    
    -- ^ Get the current presence of the indicator. Returns 'True' if
    -- it's present.
    
    setPresence :: Bool -> IO (),
    -- ^ Set the presence of the indicator.

    quit :: IO ()
    -- ^ Destroy the indicator. This usually means quitting the entire
    -- application.
  }

-- | Toggle the presence of the indicator.
togglePresence :: Indicator s i -> IO ()
togglePresence ind = (setPresence ind . not) =<< getPresence ind

-- | Convert actions in the input 'Indicator' so that those actions
-- can be executed from a non-GTK-main thread.
transportIndicator :: Indicator s i -> Indicator s i
transportIndicator ind = Indicator { updateDescription = \i d -> postGUIAsync $ updateDescription ind i d,
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
    NumInsert -> NumL0
    NumEnd -> NumL1
    NumDown -> NumL2
    NumPageDown -> NumL3
    NumLeft -> NumL4
    NumCenter -> NumL5
    NumRight -> NumL6
    NumHome -> NumL7
    NumUp -> NumL8
    NumPageUp -> NumL9
    NumDivide -> NumLDivide
    NumMulti -> NumLMulti
    NumMinus -> NumLMinus
    NumPlus -> NumLPlus
    NumEnter -> NumLEnter
    NumDelete -> NumLPeriod

-- | Data type keeping read-only config for NumPadIndicator.
data NumPadConfig =
  NumPadConfig { confButtonWidth, confButtonHeight :: Int,
                 confWindowX, confWindowY :: Int,
                 confIconPath :: FilePath
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

-- | Initialize the indicator and run the given action. This function
-- should be used directly under @main@ function.
--
-- > main :: IO ()
-- > main = withNumPadIndicator $ \indicator -> ...
-- 
-- The executable must be compiled by ghc with __@-threaded@ option enabled.__
-- Otherwise, it aborts.
withNumPadIndicator :: NumPadPosition i => (Indicator s i -> IO ()) -> IO ()
withNumPadIndicator action = if rtsSupportsBoundThreads then impl else error_impl where
  error_impl = throwIO $ userError "You need to build with -threaded option when you use WildBind.Indicator.withNumPadIndicator function."
  impl = do
    void $ initGUI
    conf <- numPadConfig
    indicator <- createMainWinAndIndicator conf
    status_icon <- createStatusIcon conf indicator
    status_icon_ref <- newIORef status_icon
    mainGUI
    void $ readIORef status_icon_ref -- to prevent status_icon from being garbage-collected. See https://github.com/gtk2hs/gtk2hs/issues/60
  createMainWinAndIndicator conf = flip runReaderT conf $ do
    win <- newNumPadWindow
    (tab, updater) <- newNumPadTable
    liftIO $ containerAdd win tab
    let indicator = Indicator
          { updateDescription = \i d -> updater i d,
            getPresence = G.get win widgetVisible,
            setPresence = \visible -> if visible then widgetShowAll win else widgetHide win,
            quit = mainQuit
          }
    liftIO $ void $ G.on win deleteEvent $ do
      liftIO $ widgetHide win
      return True -- Do not emit 'destroy' signal
    liftIO $ void $ forkFinally (action $ transportIndicator indicator) finalAction
    return indicator
  finalAction ret = do
    case ret of
      Right _ -> return ()
      Left exception -> hPutStrLn stderr ("Fatal Error from WildBind: " ++ show exception)
    postGUIAsync mainQuit
  createStatusIcon conf indicator = do
    status_icon <- statusIconNewFromFile $ confIconPath conf
    void $ G.on status_icon statusIconPopupMenu $ \mbutton time -> do
      menu <- makeStatusMenu indicator
      menuPopup menu $ (\button -> return (button, time)) =<< mbutton
    return status_icon


-- | Run 'WildBind.wildBind' with the given 'Indicator'. 'ActionDescription's
-- are shown by the 'Indicator'.
wildBindWithIndicator :: (Ord i, Enum i, Bounded i) => Indicator s i -> Binding s i -> FrontEnd s i -> IO ()
wildBindWithIndicator ind binding front = wildBind' (defOption { optBindingHook = bindingHook ind front }) binding front

-- | Create an action appropriate for 'optBindingHook' in 'Option'
-- from 'Indicator' and 'FrontEnd'.
bindingHook :: (Ord i, Enum i, Bounded i) => Indicator s1 i -> FrontEnd s2 i -> [(i, ActionDescription)] -> IO ()
bindingHook ind front bind_list = forM_ (enumFromTo minBound maxBound) $ \input -> do
  let desc = M.findWithDefault (frontDefaultDescription front input) input (M.fromList bind_list)
  updateDescription ind input desc
                       
  
newNumPadWindow :: NumPadContext Window
newNumPadWindow = do
  win <- liftIO $ windowNew
  liftIO $ windowSetKeepAbove win True
  liftIO $ G.set win [ windowSkipPagerHint := True,
                       windowSkipTaskbarHint := True,
                       windowAcceptFocus := False,
                       windowFocusOnMap := False
                     ]
  liftIO $ windowSetTitle win ("WildBind Description" :: Text)
  win_x <- confWindowX <$> ask
  win_y <- confWindowY <$> ask
  liftIO $ windowMove win win_x win_y
  return win

-- | Get the action to describe @i@, if that @i@ is supported. This is
-- a 'Monoid', so we can build up the getter by 'mconcat'.
type DescriptActionGetter i = i -> First (ActionDescription -> IO ())

newNumPadTable :: NumPadPosition i => NumPadContext (Table, (i -> ActionDescription -> IO ()))
newNumPadTable = do
  tab <- liftIO $ tableNew 5 4 False
  -- NumLock is unboundable, so it's treatd in a different way from others.
  (\label -> liftIO $ labelSetText label ("NumLock" :: Text)) =<< addButton tab 0 1 0 1
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
        First Nothing -> const $ return ()
  return (tab, description_updater)
  where
    getter :: Eq i => i -> NumPadContext Label -> NumPadContext (DescriptActionGetter i)
    getter bound_key get_label = do
      label <- get_label
      return $ \in_key -> First (if in_key == bound_key then Just $ labelSetText label else Nothing)

addButton :: Table -> Int -> Int -> Int -> Int -> NumPadContext Label
addButton tab left right top bottom = do
  lab <- liftIO $ labelNew (Nothing :: Maybe Text)
  liftIO $ labelSetLineWrap lab True
  liftIO $ miscSetAlignment lab 0 0.5
  liftIO $ labelSetJustify lab JustifyLeft
  button <- liftIO $ buttonNew
  liftIO $ buttonSetAlignment button (0, 0.5)
  liftIO $ containerAdd button lab
  liftIO $ tableAttachDefaults tab button left right top bottom
  bw <- confButtonWidth <$> ask
  bh <- confButtonHeight <$> ask
  liftIO $ widgetSetSizeRequest lab (bw * (right - left)) (bh * (bottom - top))
  return lab

makeStatusMenu :: Indicator s i -> IO Menu
makeStatusMenu ind = impl where
  impl = do
    menu <- menuNew
    containerAdd menu =<< makeQuitItem
    containerAdd menu =<< makeToggler
    return menu
  makeQuitItem = do
    quit_item <- menuItemNewWithMnemonic ("_Quit" :: Text)
    widgetShowAll quit_item
    void $ G.on quit_item menuItemActivated (quit ind)
    return quit_item
  makeToggler = do
    toggler <- checkMenuItemNewWithMnemonic ("_Toggle description" :: Text)
    widgetShowAll toggler
    checkMenuItemSetActive toggler =<< getPresence ind
    void $ G.on toggler checkMenuItemToggled (togglePresence ind)
    return toggler
