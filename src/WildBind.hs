-- |
-- Module: WildBind
-- Description: WildBind main module
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind (
  -- * Common types
  ActionDescription,
  -- * Front-end
  FrontState,
  FrontInput,
  FrontInputDevice(..),
  FrontEvent(..),
  FrontEventSource(..),
  FrontDescriber(..),
  -- * Back-end
  Action(..),
  Binding,
  -- * Entry point
  wildBind
) where

import Data.Monoid (Monoid(..))
import Data.List ((\\))
import Data.Text (Text)
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as State
import Control.Monad.IO.Class (liftIO)

-- | The state of the front-end. It can change with no regard with WildBind.
class FrontState s

-- | The type of input symbols, like key buttons.
class Ord i => FrontInput i

-- | Human-readable description of an action.
type ActionDescription = Text

-- | Something that knows about and controls the input device.
class (FrontInput i) => FrontInputDevice f i where
  defaultActionDescription :: f -> i -> ActionDescription
  -- | Grab (or capture) the specified input symbol. 
  setGrab :: f -> i -> IO ()
  -- | Release the grab for the input symbol.
  unsetGrab :: f -> i -> IO ()

-- | Event from the front-end. @s@ is the state of the front-end. @i@ is the input.
data FrontEvent s i = FEInput s i -- ^ An event that a new input is made.
                    | FEChange s  -- ^ An event that the front-end state is changed.
                    deriving (Show)

-- | Something that brings stream of events from the front-end.
class (FrontState s, FrontInput i) => FrontEventSource f s i where
  nextEvent :: f -> IO (FrontEvent s i)  -- should it be a streaming I/F, like conduit?

-- | Action done by WildBind
data Action a = Action {
  actDescription :: ActionDescription, -- ^ Human-readable description of the action.
  actDo :: IO a -- ^ The actual job.
}

-- | Something that describes current bindings for the user.
class (FrontInput i) => FrontDescriber f i where
  describeAction :: f -> i -> ActionDescription -> IO ()

-- | WildBind back-end binding between inputs and actions.
newtype Binding s i = Binding {
  bindingFor :: s -> M.Map i (Action (Binding s i))
                -- ^ The result of the 'Action' is the new state of
                -- the 'Binding'.
}

instance Monoid (Binding s i) where
  mempty = undefined
  mappend = undefined

type GrabSet i = [i]

getGrabSet :: (FrontState s) => Binding s i -> s -> GrabSet i
getGrabSet binding state = M.keys $ bindingFor binding state

updateGrab :: (FrontInputDevice f i) => f -> GrabSet i -> GrabSet i -> IO ()
updateGrab front before after = do
  mapM_ (unsetGrab front) (before \\ after)
  mapM_ (setGrab front) (after \\ before)

-- | Combines the front-end and the 'Binding' and returns the executable.
wildBind :: (FrontInputDevice fi i, FrontEventSource fe s i) => fi -> fe -> Binding s i -> IO ()
wildBind front_input front_event binding =
  State.evalStateT (wildBindWithState front_input front_event) (binding, Nothing)

---

type WBState s i = State.StateT (Binding s i, Maybe s) IO

updateWBState :: (FrontInputDevice f i, FrontState s) => f -> Binding s i -> s -> WBState s i ()
updateWBState front after_binding after_state = do
  (before_binding, before_mstate) <- State.get
  let before_grabset = maybe [] (getGrabSet before_binding) before_mstate
  State.put $ (after_binding, Just after_state)
  liftIO $ updateGrab front before_grabset (getGrabSet after_binding after_state)

updateFrontState :: (FrontInputDevice f i, FrontState s) => f -> s -> WBState s i ()
updateFrontState front after_state = do
  (cur_binding, _) <- State.get
  updateWBState front cur_binding after_state

updateBinding :: (FrontInputDevice f i, FrontState s) => f -> Binding s i -> WBState s i ()
updateBinding front after_binding = do
  (_, mstate) <- State.get
  case mstate of
    Nothing -> return ()
    Just state -> updateWBState front after_binding state

wildBindWithState :: (FrontInputDevice fi i, FrontEventSource fe s i) => fi -> fe -> WBState s i ()
wildBindWithState front_input front_event = do
  event <- liftIO $ nextEvent front_event
  case event of
    FEChange state ->
      updateFrontState front_input state
    FEInput state input -> do
      updateFrontState front_input state
      (cur_binding, _) <- State.get
      case M.lookup input $ bindingFor cur_binding state of
        Nothing -> return ()
        Just action -> do
          next_binding <- liftIO $ actDo action
          updateBinding front_input next_binding
  wildBindWithState front_input front_event


