-- |
-- Module: WildBind.Binding
-- Description: Functions to build Binding
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.Binding (
  -- * Types
  Action(Action,actDescription,actDo),
  Binding,
  -- * Execution
  boundAction,
  boundInputs,
  -- * Construction
  bindList,
  on',
  -- * Condition
  whenS,
  -- * Conversion
  contramapFrontState,
  mapInput,
  invmapBackState,
  -- * Explicitly Stateful Bindings
  Binding',
  stateful
) where

import qualified Data.Map as M
import Data.Monoid (Monoid(..))

import WildBind.Internal.FrontEnd (ActionDescription)

-- | Action done by WildBind
data Action a = Action {
  actDescription :: ActionDescription, -- ^ Human-readable description of the action.
  actDo :: IO a -- ^ The actual job.
}

instance Functor Action where
  fmap f a = a { actDo = fmap f (actDo a) }

-- | WildBind back-end binding with both explicit and implicit
-- states. @bs@ is the explicit back-end state, @fs@ is the front-end
-- state, and @i@ is the input type.
newtype Binding' bs fs i = Binding' {
  unBinding' :: bs -> fs -> M.Map i (Action (Binding' bs fs i, bs))
}

-- | WildBind back-end binding between inputs and actions. @s@ is the
-- front-end state type, and @i@ is the input type.
type Binding s i = Binding' () s i

instance Monoid (Binding' bs fs i) where
  mempty = undefined
  mappend = undefined


-- | Get the 'Action' bound to the specified state @s@ and input @i@.
boundAction :: (Ord i) => Binding s i -> s -> i -> Maybe (Action (Binding s i))
boundAction binding state input = (fmap . fmap) fst $ M.lookup input $ unBinding' binding () state

-- | Get the list of all inputs @i@ bound to the specified state @s@.
boundInputs :: Binding s i -> s -> [i]
boundInputs binding state = M.keys $ unBinding' binding () state

-- | Build a 'Binding' from a list.
bindList :: [(i, Action (Binding s i))] -- ^ Bound pairs of input symbol and 'Action'
         -> Binding s i -- ^ Result Binding. The given bindings are activated regardless of front-end state.
bindList = undefined

-- | Build a single pair of binding.
on' :: i -> ActionDescription -> IO a -> (i, Action a)
on' = undefined

-- | Add a condition to 'Binding'.
whenS :: (fs -> Bool) -- ^ Condition about the front-end state.
      -> Binding' bs fs i -- ^ Original Binding.
      -> Binding' bs fs i -- ^ Result Binding where the original
                          -- Binding is activated only when the given
                          -- condition is 'True'.
whenS cond orig_bind = Binding' $ \bs fs ->
  if cond fs
  then unBinding' orig_bind bs fs
  else M.empty

-- infixr version of 'whenS' may be useful, too.

mapResult :: (a -> a') -> (b -> b') -> M.Map i (Action (a, b)) -> M.Map i (Action (a',b'))
mapResult amapper bmapper = (fmap . fmap) (\(a, b) -> (amapper a, bmapper b))

-- | Contramap the front-end state.
contramapFrontState :: (fs -> fs') -> Binding' bs fs' i -> Binding' bs fs i
contramapFrontState cmapper orig_bind = Binding' $ \bs fs ->
  mapResult (contramapFrontState cmapper) id $ unBinding' orig_bind bs (cmapper fs)

-- | Map the front-end input.
mapInput :: Ord i' => (i -> i') -> Binding' bs fs i -> Binding' bs fs i'
mapInput mapper orig_bind = Binding' $ \bs fs ->
  mapResult (mapInput mapper) id $ M.mapKeys mapper $ unBinding' orig_bind bs fs

-- | Invariant-map the back-end state.
invmapBackState :: (bs -> bs') -> (bs' -> bs) -> Binding' bs fs i -> Binding' bs' fs i
invmapBackState mapper cmapper orig_bind = Binding' $ \bs fs ->
  mapResult (invmapBackState mapper cmapper) mapper $ unBinding' orig_bind (cmapper bs) fs

-- | Convert 'Binding'' to 'Binding' by hiding the explicit state
-- @bs@.
stateful :: bs -- ^ Initial state
         -> Binding' bs fs i -- ^ Binding' with explicit state
         -> Binding fs i -- ^ Binding containing the state inside
stateful init_state b' = Binding' $ \() front_state ->
  (fmap . fmap) toB $ unBinding' b' init_state front_state
  where
    toB (next_b', next_state) = (stateful next_state next_b', ())
