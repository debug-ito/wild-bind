-- |
-- Module: WildBind.Internal.Common
-- Description: Data types and type classes used commonly
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not use this module
-- directly. Use "WildBind" instead.
module WildBind.Internal.Common (
  ActionDescription
) where

import Data.Text (Text)

-- | Human-readable description of an action.
type ActionDescription = Text

