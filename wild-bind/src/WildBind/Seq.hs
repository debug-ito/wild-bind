-- |
-- Module: WildBind.Seq
-- Description: Support for binding sequence of input events.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module WildBind.Seq
       ( prefix,
         SeqBinding,
         withPrefix,
         cancelOn,
         withCancel,
         toSeq,
         fromSeq
       ) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Monoid (Monoid(..), (<>), mconcat)

import WildBind.Binding
  ( Binding, Binding', binds', whenBack, on, as, run, extend,
    startFrom
  )

newtype SeqBinding fs i = SeqBinding ([i] -> Binding' [i] fs i)

instance Ord i => Monoid (SeqBinding fs i) where
  mempty = SeqBinding $ const mempty
  mappend (SeqBinding a) (SeqBinding b) =
    SeqBinding $ \ps -> mappend (a ps) (b ps)

withPrefix :: Ord i => [i] -> SeqBinding fs i -> SeqBinding fs i
withPrefix ps sb = foldr withPrefixSingle sb ps

withPrefixSingle :: Ord i => i -> SeqBinding fs i -> SeqBinding fs i
withPrefixSingle p (SeqBinding fb) = 
  SeqBinding $ \cur_prefix -> prefixBinding cur_prefix <> nextBinding cur_prefix
  where
    prefixBinding cur_prefix = whenBack (== cur_prefix) $ binds' $ do
      on p `as` "prefix" `run` State.modify (++ [p])
    nextBinding cur_prefix = fb (cur_prefix ++ [p])

toSeq :: Eq i => Binding fs i -> SeqBinding fs i
toSeq b = SeqBinding $ \ps -> whenBack (== ps) $ extend b -- TODO: ここで既存のコマンドをreviseしてcancelをかけないといけない。beforeでcancelするか。

fromSeq :: SeqBinding fs i -> Binding fs i
fromSeq (SeqBinding fb) = startFrom [] $ fb []

cancelOn :: Ord i => i -> SeqBinding fs i
cancelOn c = SeqBinding $ const $ whenBack (not . null) $ binds' $ on c `as` "cancel" `run` State.put []

withCancel :: Ord i => [i] -> SeqBinding fs i -> SeqBinding fs i
withCancel cs sb = cancelBindings <> sb
  where
    cancelBindings = mconcat $ map cancelOn cs

prefix :: Ord i => [i] -> [i] -> Binding fs i -> Binding fs i
prefix rs ps = fromSeq . withCancel rs . withPrefix ps . toSeq

