-- |
-- Module: WildBind.Seq
-- Description: Support for binding sequence of input events.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines convenient functions to build 'Binding's that
-- bind actions to key sequences.
--
-- Synopsis
--
-- > -- Example
-- > import Data.Monoid ((<>))
-- > import WildBind (Binding, wildBind, binds, on, run)
-- > import WildBind.X11 -- from wild-bind-x11 package
-- >   ( XKeyEvent, ActiveWindow,
-- >     withFrontEnd, ctrl, press
-- >   )
-- > import qualified WildBind.X11.KeySym as Sym
-- > import WildBind.Seq
-- >   ( SeqBinding,
-- >     prefix, fromSeq, toSeq, withCancel, withPrefix
-- >   )
-- > 
-- > main :: IO ()
-- > main = withFrontEnd $ wildBind myBinding_simple
-- > 
-- > -- 'prefix' is a simple API to build a binding for key sequence.
-- > myBinding_simple :: Binding ActiveWindow XKeyEvent
-- > myBinding_simple = prefix [ctrl Sym.xK_g] [ctrl Sym.xK_x] $ binds $ do
-- >   on (ctrl Sym.xK_f) `run` putStrLn "C-x C-f"
-- >   on (ctrl Sym.xK_o) `run` putStrLn "C-x C-o"
-- >   on (ctrl Sym.xK_c) `run` putStrLn "C-x C-c"
-- > 
-- > -- You can combine 'SeqBinding' objects together to build a complex
-- > -- binding for key sequence.
-- > myBinding_complex :: Binding ActiveWindow XKeyEvent
-- > myBinding_complex = fromSeq $ withCancel [ctrl Sym.xK_g] seq_binding
-- >   where
-- >     seq_binding :: SeqBinding ActiveWindow XKeyEvent
-- >     seq_binding = (withPrefix [ctrl Sym.xK_c] c_binding)
-- >                   <> (withPrefix [ctrl Sym.xK_x] x_binding)
-- >     c_binding :: SeqBinding ActiveWindow XKeyEvent
-- >     c_binding = toSeq $ binds $ do
-- >       on (ctrl Sym.xK_n) `run` putStrLn "C-c C-n"
-- >       on (ctrl Sym.xK_p) `run` putStrLn "C-c C-p"
-- >     x_binding :: SeqBinding ActiveWindow XKeyEvent
-- >     x_binding = (withPrefix [ctrl Sym.xK_Return] xret_binding)
-- >                 <> plain_x_binding
-- >     plain_x_binding :: SeqBinding ActiveWindow XKeyEvent
-- >     plain_x_binding = toSeq $ binds $ do
-- >       on (ctrl Sym.xK_f) `run` putStrLn "C-x C-f"
-- >     xret_binding :: SeqBinding ActiveWindow XKeyEvent
-- >     xret_binding = toSeq $ binds $ do
-- >       on (press Sym.xK_f) `run` putStrLn "C-x RET f"
-- >       on (press Sym.xK_c) `run` putStrLn "C-x RET c"
    
module WildBind.Seq
       ( -- * Simple API
         prefix,
         -- * Advanced API
         SeqBinding,
         toSeq,
         fromSeq,
         withPrefix,
         withCancel,
         reviseSeq
       ) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Monoid (Monoid(..), (<>), mconcat)

import WildBind.Binding
  ( Binding, Binding', binds', whenBack, on, as, run, extend,
    startFrom, revise', justBefore, revise,
    Action
  )

-- | Intermediate type of building a 'Binding' for key sequences.
newtype SeqBinding fs i = SeqBinding ([i] -> Binding' [i] fs i)

-- | Follows the same rule as 'Binding'.
instance Ord i => Monoid (SeqBinding fs i) where
  mempty = SeqBinding $ const mempty
  mappend (SeqBinding a) (SeqBinding b) =
    SeqBinding $ \ps -> mappend (a ps) (b ps)

-- | Prepend prefix keys to the 'SeqBinding'.
--
-- 'SeqBinding' is composable in terms of prefixes, that is,
--
-- > (withPrefix [key1, key2] seq_b) == (withPrefix [key1] $ withPrefix [key2] seq_b)
withPrefix :: Ord i
           => [i] -- ^ prefix keys
           -> SeqBinding fs i
           -> SeqBinding fs i
withPrefix ps sb = foldr withPrefixSingle sb ps

withPrefixSingle :: Ord i => i -> SeqBinding fs i -> SeqBinding fs i
withPrefixSingle p (SeqBinding fb) = 
  SeqBinding $ \cur_prefix -> nextBinding cur_prefix <> prefixBinding cur_prefix
  where
    prefixBinding cur_prefix = whenBack (== cur_prefix) $ binds' $ do
      on p `as` "prefix" `run` State.modify (++ [p])
    nextBinding cur_prefix = fb (cur_prefix ++ [p])

-- | Create a 'SeqBinding' from 'Binding'. The result 'SeqBinding' has
-- no prefixes yet.
toSeq :: Eq i => Binding fs i -> SeqBinding fs i
toSeq b = SeqBinding $ \ps -> whenBack (== ps) $ revise' cancelBefore $ extend b
  where
    cancelBefore _ _ _ = justBefore $ State.put []

-- | Resolve 'SeqBinding' to build a 'Binding' for key sequences.
fromSeq :: SeqBinding fs i -> Binding fs i
fromSeq (SeqBinding fb) = startFrom [] $ fb []

-- | A 'SeqBinding' that binds the given key for canceling the key
-- sequence.
cancelOn :: Ord i
         => i -- ^ cancel key
         -> SeqBinding fs i
cancelOn c = SeqBinding $ const $ whenBack (not . null) $ binds' $ on c `as` "cancel" `run` State.put []

-- | Add cancel keys to the 'SeqBinding'.
withCancel :: Ord i
           => [i] -- ^ cancel keys
           -> SeqBinding fs i
           -> SeqBinding fs i
withCancel cs sb = cancelBindings <> sb
  where
    cancelBindings = mconcat $ map cancelOn cs

-- | Prepend prefix keys to a 'Binding'. In the result 'Binding', the
-- original 'Binding' is enabled only after you input the prefix input
-- symbols in the same order.
--
-- During typing prefix keys, you can cancel and reset the key
-- sequence by typing the \"cancel keys\". This is analogous to @C-g@
-- in Emacs. The binding of cancel keys are weak, that is, they are
-- overridden by the original binding and prefix keys.
--
-- Note that this function creates an independent implicit state to
-- memorize prefix keys input so far. This means,
--
-- > (prefix [] [key1, key2] b) /= (prefix [] [key1] $ prefix [] [key2] b)
--
-- If you want a more composable way of building a sequence binding,
-- try 'SeqBinding'.
prefix :: Ord i
       => [i] -- ^ The cancel keys (input symbols for canceling the current key sequence.)
       -> [i] -- ^ list of prefix input symbols
       -> Binding fs i -- ^ the original binding.
       -> Binding fs i -- ^ the result binding.
prefix cs ps = fromSeq . withCancel cs . withPrefix ps . toSeq

-- | Revise actions in 'SeqBinding'. See 'WildBind.Binding.revise'.
reviseSeq :: (forall a . [i] -> fs -> i -> Action IO a -> Maybe (Action IO a))
             -- ^ Revising function. @[i]@ is the prefix keys input so far.
          -> SeqBinding fs i
          -> SeqBinding fs i
reviseSeq f (SeqBinding orig) = SeqBinding $ fmap (revise f) orig
