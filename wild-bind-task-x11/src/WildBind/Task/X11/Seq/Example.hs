-- |
-- Module: WildBind.Task.X11.Seq.Example
-- Description: An example of WildBind.Seq
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an example of using "WildBind.Seq" module to build bindings
-- for key sequences. See the source.
--
-- @since 0.2.0.0
module WildBind.Task.X11.Seq.Example where

import           Data.Monoid         ((<>))

-- Following is from wild-bind package
import           WildBind            (Binding, binds, on, run, wildBind)
import           WildBind.Seq        (SeqBinding, fromSeq, prefix, toSeq, withCancel, withPrefix)

-- Following is from wild-bind-x11 package
import           WildBind.X11        (ActiveWindow, XKeyEvent, ctrl, press, withFrontEnd)
import qualified WildBind.X11.KeySym as Sym

main :: IO ()
main = withFrontEnd $ wildBind myBinding_simple

-- | 'prefix' is a simple API to build a binding for key sequence.
myBinding_simple :: Binding ActiveWindow XKeyEvent
myBinding_simple = prefix [ctrl Sym.xK_g] [ctrl Sym.xK_x] $ binds $ do
  on (ctrl Sym.xK_f) `run` putStrLn "C-x C-f"
  on (ctrl Sym.xK_o) `run` putStrLn "C-x C-o"
  on (ctrl Sym.xK_c) `run` putStrLn "C-x C-c"

-- | You can combine 'SeqBinding' objects together to build a complex
-- binding for key sequence.
myBinding_complex :: Binding ActiveWindow XKeyEvent
myBinding_complex = fromSeq $ withCancel [ctrl Sym.xK_g] seq_binding
  where
    seq_binding :: SeqBinding ActiveWindow XKeyEvent
    seq_binding = (withPrefix [ctrl Sym.xK_c] c_binding)
                  <> (withPrefix [ctrl Sym.xK_x] x_binding)
    c_binding :: SeqBinding ActiveWindow XKeyEvent
    c_binding = toSeq $ binds $ do
      on (ctrl Sym.xK_n) `run` putStrLn "C-c C-n"
      on (ctrl Sym.xK_p) `run` putStrLn "C-c C-p"
    x_binding :: SeqBinding ActiveWindow XKeyEvent
    x_binding = (withPrefix [ctrl Sym.xK_Return] xret_binding)
                <> plain_x_binding
    plain_x_binding :: SeqBinding ActiveWindow XKeyEvent
    plain_x_binding = toSeq $ binds $ do
      on (ctrl Sym.xK_f) `run` putStrLn "C-x C-f"
    xret_binding :: SeqBinding ActiveWindow XKeyEvent
    xret_binding = toSeq $ binds $ do
      on (press Sym.xK_f) `run` putStrLn "C-x RET f"
      on (press Sym.xK_c) `run` putStrLn "C-x RET c"
