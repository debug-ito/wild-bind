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
  mapS,
  mapI
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

-- | WildBind back-end binding between inputs and actions. @s@ is the
-- front-end state type, and @i@ is the input type.
newtype Binding s i = Binding {
  unBinding :: s -> M.Map i (Action (Binding s i))
                -- ^ The result of the 'Action' is the new state of
                -- the 'Binding'.
}

instance Monoid (Binding s i) where
  mempty = undefined
  mappend = undefined


-- | Get the 'Action' bound to the specified state @s@ and input @i@.
boundAction :: (Ord i) => Binding s i -> s -> i -> Maybe (Action (Binding s i))
boundAction binding state input = M.lookup input $ unBinding binding state

-- | Get the list of all inputs @i@ bound to the specified state @s@.
boundInputs :: Binding s i -> s -> [i]
boundInputs binding state = M.keys $ unBinding binding state

-- | Build a 'Binding' from a list.
bindList :: [(i, Action (Binding s i))] -- ^ Bound pairs of input symbol and 'Action'
         -> Binding s i -- ^ Result Binding. The given bindings are activated regardless of front-end state.
bindList = undefined

-- | Build a single pair of binding.
on' :: i -> ActionDescription -> IO a -> (i, Action a)
on' = undefined

-- | Add a condition to 'Binding'.
whenS :: (s -> Bool) -- ^ Condition about the front-end state.
      -> Binding s i -- ^ Original Binding.
      -> Binding s i -- ^ Result Binding where the original Binding is
                     -- activated only when the given condition is
                     -- 'True'.
whenS = undefined

-- infixr version of 'whenS' may be useful, too.


-- | Contramap the front-end state.
mapS :: (s -> s') -> Binding s' i -> Binding s i
mapS = undefined

-- | contramap the front-end input.
mapI :: (i -> i') -> Binding s i' -> Binding s i
mapI = undefined
