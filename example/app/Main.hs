{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import WildBind.Task.X11

main = wildNumPad myBinding

myBinding = binds $ do
  on NumCenter `run` putStrLn "Hello, world!"
