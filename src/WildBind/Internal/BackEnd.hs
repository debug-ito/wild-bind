-- |
-- Module: WildBind.Internal.BackEnd
-- Description: Data types and type classes about back-ends.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not use this module
-- directly. Use "WildBind" instead.
module WildBind.Internal.BackEnd (
  Action(..),
  Binding(..),
  boundAction,
  boundInputs
) where

import qualified Data.Map as M
import Data.Monoid (Monoid(..))

import WildBind.Internal.Common (ActionDescription)

-- | Action done by WildBind
data Action a = Action {
  actDescription :: ActionDescription, -- ^ Human-readable description of the action.
  actDo :: IO a -- ^ The actual job.
}

-- | WildBind back-end binding between inputs and actions.
newtype Binding s i = Binding {
  unBinding :: s -> M.Map i (Action (Binding s i))
                -- ^ The result of the 'Action' is the new state of
                -- the 'Binding'.
}

-- | Get the 'Action' bound to the specified state @s@ and input @i@.
boundAction :: (Ord i) => Binding s i -> s -> i -> Maybe (Action (Binding s i))
boundAction binding state input = M.lookup input $ unBinding binding state

-- | Get the list of all inputs @i@ bound to the specified state @s@.
boundInputs :: Binding s i -> s -> [i]
boundInputs binding state = M.keys $ unBinding binding state

instance Monoid (Binding s i) where
  mempty = undefined
  mappend = undefined
