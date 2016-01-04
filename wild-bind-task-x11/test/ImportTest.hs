module Main (main) where

import WildBind.Task.X11 (
  wildNumPad, wildNumPad',
  Binding, Binding',
  NumPadUnlockedInput(..), NumPadLockedInput(..),
  ActiveWindow,
  Indicator,
  (<>)
  )

main :: IO ()
main = putStrLn "If it compiles, it's ok."

check_wildNumPad_unlocked :: Binding ActiveWindow NumPadUnlockedInput -> IO ()
check_wildNumPad_unlocked = wildNumPad

check_wildNumPad_locked :: Binding ActiveWindow NumPadLockedInput -> IO ()
check_wildNumPad_locked = wildNumPad

