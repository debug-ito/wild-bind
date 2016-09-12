module Main (main) where

import WildBind.Task.X11
  ( wildNumPad, wildNumPad',
    Binding, Binding',
    NumPadUnlocked(..), NumPadLocked(..),
    ActiveWindow,
    Indicator,
    (<>)
  )

main :: IO ()
main = putStrLn "If it compiles, it's ok."

check_wildNumPad_unlocked :: Binding ActiveWindow NumPadUnlocked -> IO ()
check_wildNumPad_unlocked = wildNumPad

check_wildNumPad_locked :: Binding ActiveWindow NumPadLocked -> IO ()
check_wildNumPad_locked = wildNumPad

