-- |
-- Module: WildBind.Indicator
-- Description: Graphical indicator for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module WildBind.Indicator (
  withNumPadIndicator,
  Indicator,
  updateDescription,
  getPresence,
  setPresence,
  togglePresence,
  NumPadPosition(..)
) where

import Control.Monad (void)
import Control.Applicative ((<$>))
import Data.Monoid (mconcat, First(First))

import WildBind (ActionDescription)
import WildBind.Input.NumPad (NumPadUnlockedInput(..), NumPadLockedInput(..))
import Graphics.UI.Gtk (
  initGUI, mainGUI,
  Window, windowNew, windowSetKeepAbove, windowSkipPagerHint,
  windowSkipTaskbarHint, windowAcceptFocus, windowFocusOnMap,
  windowSetTitle,
  set, AttrOp((:=)),
  widgetShowAll, widgetSetSizeRequest,
  Table, tableNew, tableAttachDefaults,
  buttonNew, buttonSetAlignment,
  Label, labelNew, labelSetLineWrap, labelSetJustify, Justification(JustifyLeft), labelSetText,
  miscSetAlignment,
  containerAdd
  )
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)

-- | Indicator interface. @s@ is the front-end state, @i@ is the input
-- type.
data Indicator s i = Indicator {
  updateDescription :: [(i, ActionDescription)] -> IO (),
  -- ^ Update and show the description for the current binding.
  getPresence :: IO Bool,
  -- ^ Get the current presence of the indicator. Returns 'True' if
  -- it's present.
  setPresence :: Bool -> IO ()
  -- ^ Set the presence of the indicator.
  }

-- | Toggle the presence of the indicator.
togglePresence :: Indicator s i -> IO ()
togglePresence ind = (setPresence ind . not) =<< getPresence ind


-- | Something that can be mapped to number pad's key positions.
class NumPadPosition a where
  toNumPad :: a -> NumPadLockedInput

instance NumPadPosition NumPadLockedInput where
  toNumPad = id

instance NumPadPosition NumPadUnlockedInput where
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
data NumPadConfig = NumPadConfig {
  confButtonWidth :: Int,
  confButtonHeight :: Int
  }

numPadConfig :: NumPadConfig
numPadConfig = NumPadConfig {
  confButtonWidth = 70,
  confButtonHeight = 45
  }

-- | Contextual monad for creating NumPadIndicator
type NumPadContext = ReaderT NumPadConfig IO

-- | Initialize the indicator and run the given action. This function
-- should be used directly under @main@ function.
--
-- > main :: IO ()
-- > main = withNumPadIndicator $ \indicator -> ...
withNumPadIndicator :: NumPadPosition i => (Indicator s i -> IO ()) -> IO ()
withNumPadIndicator action = do
  void $ initGUI
  win <- flip runReaderT numPadConfig newNumPadWindow
  widgetShowAll win
  mainGUI
  
newNumPadWindow :: NumPadContext Window
newNumPadWindow = do
  win <- liftIO $ windowNew
  liftIO $ windowSetKeepAbove win True
  liftIO $ set win [windowSkipPagerHint := True,
                    windowSkipTaskbarHint := True,
                    windowAcceptFocus := False,
                    windowFocusOnMap := False]
  liftIO $ windowSetTitle win ("WildBind Status" :: Text)
  return win

-- | Get the action to describe @i@, if that @i@ is supported. This is
-- a 'Monoid', so we can build up the getter by 'mconcat'.
type DescriptActionGetter i = i -> First (ActionDescription -> IO ())

newNumPadTable :: NumPadPosition i => NumPadContext (Table, ([(i, ActionDescription)] -> IO ()))
newNumPadTable = do
  tab <- liftIO $ tableNew 5 4 False
  -- NumLock is unboundable, so it's treatd in a different way from others.
  (\label -> liftIO $ labelSetText label ("NumLock" :: Text)) =<< addButton tab 0 1 0 1
  descript_action_getter <- fmap mconcat $ sequence $ [
    getter NumLDivide $ addButton tab 1 2 0 1,
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
  let description_updater = mapM_ $ \(input, desc) -> case descript_action_getter $ toNumPad input of
        First (Just act) -> act desc
        First Nothing -> return ()
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
