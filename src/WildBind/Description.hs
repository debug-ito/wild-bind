-- |
-- Module: WildBind.Description
-- Description: Types about ActionDescription
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.Description (
  ActionDescription,
  Describable(..)
) where

import Data.Text (Text)

-- | Human-readable description of an action.
type ActionDescription = Text

-- | Class for something describable
class Describable d where
  describe :: d -> ActionDescription
