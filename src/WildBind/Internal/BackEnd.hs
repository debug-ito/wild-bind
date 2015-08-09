-- |
-- Module: WildBind.Internal.BackEnd
-- Description: Data types and type classes about back-ends.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not use this module
-- directly. Use "WildBind" instead.
module WildBind.Internal.BackEnd (
  Action(..),
  Binding(..)
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

instance Monoid (Binding s i) where
  mempty = undefined
  mappend = undefined
