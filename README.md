# WildBind

WildBind is a dynamic and programmable key binding framework for number pads.

Features:

- It supports X11 desktop environments.
- It binds any action to number pad keys.
- Key bindings are written in Haskell (actually, it's just a bunch of Haskell modules).
- Key bindings can be **dynamic**, i.e. you can use different key bindings for different active windows.
- Key bindings can be **stateful**, e.g. you can bind actions to **sequences** of keys.


## Getting Started

We recommend `stack` to build and install WildBind. First, follow the instruction in https://docs.haskellstack.org/ to install `stack` command.

Second, install development packages for libx11 and libgtk. In Ubuntu, you can install them by

    $ sudo apt-get install libx11-dev libgtk2.0-dev

After that, clone this repository and install WildBind.

    $ git clone https://github.com/debug-ito/wild-bind.git
    $ cd wild-bind
    $ stack --install-ghc build

The last command triggers a lot of downloading and building. Be patient.

## Simplest Binding

Let's start with the simplest "Hello, world" binding. Save the following text as `simplest.hs` in the cloned Git directory.

```haskell
#/usr/bin/env stack
-- stack runghc

{-# LANGUAGE OverloadedStrings #-}
import WildBind.Task.X11

main = wildNumPad $ binds $ do
  on NumCenter `run` putStrLn "Hello, world!"

```

This binds an action to the 5 key (NumLock disabled) on a num pad.

To activate, make it executable and run.

    $ chmod a+x simplest.hs
    $ ./simplest.hs

When activated, it shows an icon on the status bar.

Then hit 5 key with NumLock disabled and it shows "Hello, world!" on the console.

To deactivate the binding, right-click the icon and select "Quit" item.


## Combine Bindings

## Dynamic Binding Based on ActiveWindow

## Stateful Binding

## Bind to Keys with NumLock Enabled

## External Tools

TBW.

- xdotool
- wmctrl
- boring-window-switcher

## For Programmers

## Author

Toshio Ito <debug.ito@gmail.com>
