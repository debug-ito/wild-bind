-- |
-- Module: WildBind.Seq
-- Description: Support for binding sequence of input events.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module WildBind.Seq
       ( prefix,
         SeqBinding,
         prefix',
         toSeq,
         fromSeq
       ) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Monoid ((<>))

import WildBind.Binding
  ( Binding, Binding', binds', whenBack, on, run
  )

newtype SeqBinding fs i = SeqBinding { unSeqBinding :: [i] -> Binding' [i] fs i }

-- TODO: instance Monoid

prefix' :: [i] -> SeqBinding fs i -> SeqBinding fs i
prefix' = undefined

toSeq :: Binding fs i -> SeqBinding fs i
toSeq = undefined

fromSeq :: SeqBinding fs i -> Binding fs i
fromSeq = undefined

prefix :: [i] -> Binding fs i -> Binding fs i
prefix ps = fromSeq . prefix' ps . toSeq


-- type SeqBinding fs i = State [i] (Binding' [i] fs i)
-- 
-- 
-- prefix :: Ord i => [i] -> SeqBinding fs i -> SeqBinding fs i
-- prefix [] sb = sb
-- prefix (p : rest) sb = do
--   cur_prefix <- State.get
--   let next_prefix = cur_prefix ++ [p]
--   State.put next_prefix
--   next_b <- prefix rest sb
--   return $ catchPrefix cur_prefix <> prefixedBinding next_prefix next_b
--   where
--     catchPrefix cur_prefix = whenBack (== cur_prefix) $ binds' $ do
--       on p `run` State.modify (++ [p])
--     prefixedBinding next_prefix next_b = whenBack (== next_prefix) next_b
  
