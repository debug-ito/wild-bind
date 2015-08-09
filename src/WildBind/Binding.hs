-- |
-- Module: WildBind.Binding
-- Description: Functions to build Binding
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.Binding (
  Binding,
  -- * Construction
  bindList,
  on',
  -- * Condition
  whenS,
  -- * Conversion
  mapS,
  mapI
) where

import WildBind.Internal.Common (ActionDescription)
import WildBind.Internal.BackEnd (
  Binding(Binding), Action(Action)
  )

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
