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
#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE OverloadedStrings #-}
import WildBind.Task.X11

main = wildNumPad myBinding

myBinding = binds $ do
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

Of course, you can bind actions to more than one keys. To do that, just repeat `on` statements.

```haskell
#!/usr/bin/env stack
-- stack runghc --package process

{-# LANGUAGE OverloadedStrings #-}
import WildBind.Task.X11
import System.Process (spawnCommand)

main = wildNumPad myBinding

myBinding = binds $ do
  on NumCenter `run` putStrLn "Hello, world!"
  on NumPageUp `run` spawnCommand "firefox"
```

This script uses [System.Process](http://hackage.haskell.org/package/process/docs/System-Process.html) module and binds to the PageUp key an action to launch Firefox.

Or, you can combine independent bindings to get a complex binding by `<>` operator.

```haskell
myBinding = simplestBinding <> firefoxBinding

simplestBinding = binds $ do
  on NumCenter `run` putStrLn "Hello, world!"

firefoxBinding = binds $ do
  on NumPageUp `run` spawnCommand "firefox"
```

This is possible because a Binding object is a [Monoid](http://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Monoid).



## Dynamic Binding Based on ActiveWindow

Now let's make our binding more interesting.

WildBind allows you to create a binding that **changes dynamically according to the active window (the window having keyboard focus)**.

```haskell
pushKey key = spawnCommand ("xdotool key " <> key)  ----------------- (1)

myBinding = forFirefox <> forVLC  ----------------------------------- (2)

forFirefox = whenFront isFirefox $ binds $ do  ---------------------- (3)
  on NumRight `run` pushKey "Ctrl+Tab"  ----------------------------- (4)
  on NumLeft `run` pushKey "Ctrl+Shift+Tab"
  where
    isFirefox active_window = winClass active_window == "Firefox" --- (5)

forVLC = whenFront isVLC $ binds $ do  ------------------------------ (6)
  on NumRight `run` pushKey "Ctrl+Right"
  on NumLeft `run` pushKey "Ctrl+Left"
  where
    isVLC active_window = winClass active_window == "vlc"
```

The above script makes binding on → and ← keys on a num pad, but they behave differently for different applications.

First, we define a support function `pushKey` **(1)**. It generates a fake keyboard input specified by the argument. It uses an external tool called [xdotool](https://github.com/jordansissel/xdotool).

Then we define `myBinding` **(2)**, which is combination of one for Firefox and one for VLC media player.

In the definition of `forFirefox`, we use `whenFront` function **(3)**. `whenFront` function adds a condition to the binding. The binding is active only when the predicate (`isFirefox`, in this case) returns `True`. The predicate `isFirefox` is defined later at **(5)**. It takes an `ActiveWindow` object, and checks if the active window is Firefox or not.

Binding definitions of `forFirefox` are just like previous examples **(4)**. Here we bind "Right tab" action to → key and "Left tab" action to ← key. However, these bindings are enabled only when a window for Firefox is active.

Definition of `forVLC` is similar to `forFirefox` **(6)**. This time we bind "Jump forward" action to → key and "Jump backward" action to ← key.

Using `whenFront` and `<>` functions like the above example, you can build bindings that adapt to the currently active window and Do What You Mean&trade;.

## Binding Override by `<>`

Binding combination operator `<>` prefers the right-hand binding. That is, if left-hand and right-hand bindings both bind actions to the same key, the right-hand binding wins.

This feature is often useful in combination of `whenFront`.

```haskell
myBinding = defaultBinding <> forFirefox

defaultBinding = binds $ do
  on NumRight `run` putStrLn "right is pushed."
  on NumLeft `run` putStrLn "left is pushed."

forFirefox = whenFront isFirefox $ binds $ do
  on NumRight `run` pushKey "Ctrl+Tab"
  on NumLeft `run` pushKey "Ctrl+Shift+Tab"
  where
    isFirefox active_window = winClass active_window == "Firefox"
```

Here, `defaultBinding` is enabled in most cases because it doesn't have any `whenFront` condition. When a window for Firefox becomes active, `forFirefox` binding is enabled. Because `forFirefox` is the right-hand, it overrides bindings by `defaultBinding`, and provides actions specific to Firefox.


## Binding Description


When you build a complex binding, sometimes you have no idea what actions are bound to which keys. To help understand behavior of a binding, you can set descriptions to bound actions using `as` function.

```haskell
forFirefox = whenFront isFirefox $ binds $ do
  on NumRight `as` "Go to right tab" `run` pushKey "Ctrl+Tab"
  on NumLeft `as` "Go to left tab" `run` pushKey "Ctrl+Shift+Tab"
  where
    isFirefox active_window = winClass active_window == "Firefox"
```

Notice the `as` function is inserted between `on` and `run` functions.

To see the description of current binding, press Enter key on a num pad. It shows a window like this:

![(screen shot)](https://raw.githubusercontent.com/debug-ito/wild-bind/master/resource/shot_description_window.png)

Press Enter key again to hide the window.


## Types and Modules

Before entering the most complicated part of WildBind, let's introduce data types used in the previous examples.

The most important type is `Binding`, which is the type of `myBinding`.

```haskell
myBinding :: Binding ActiveWindow NumPadUnlockedInput
```

The above type means that `myBinding` binds actions to the input key type of `NumPadUnlockedInput`, and that it changes binding based on `ActiveWindow`.

`Binding` type is defined in WildBind.Binding module. This module also defines a lot of functions to build `Binding`, such as `binds`, `on`, `run`, `as` and `whenFront`.

`NumPadUnlockedInput` is the type for keys on a num pad when NumLock is disabled. It is defined in WildBind.Input.NumPad module. `NumCenter` and `NumLeft` are its data constructors. If you want to use WildBind with NumLock enabled, use `NumPadLockedInput`.

`ActiveWindow` is the type for an active window. It is defined in WildBind.X11. You can inspect the window by accessor functions such as `winClass`.


## Stateful Binding


So far, bound actions are just plain `IO ()`, and `Binding` has no internal state.

```haskell
myBinding :: Binding ActiveWindow NumPadUnlockedInput
myBinding = binds $ do
  on NumCenter `run` myAction

myAction :: IO ()
myAction = putStrLn "Hello, world!"
```

WildBind has a built-in support for **stateful keybindings**. A binding object can have its own state of arbitrary type, and behave differently according to the state.

```haskell
#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE OverloadedStrings #-}
import WildBind.Task.X11

main = wildNumPad myBinding

myBinding' :: Binding' Int ActiveWindow NumPadUnlockedInput  -------- (1)
myBinding' = binds' $ do  ------------------------------------------- (2)
  on NumUp `run` upAction
  on NumDown `run` downAction
  on NumCenter `run` centerAction

upAction, downAction, centerAction :: StateT Int IO ()  ------------- (3)
upAction = modify (+ 1)
downAction = modify (subtract 1)
centerAction = do
  current_state <- get
  liftIO $ putStrLn ("Current state is = " ++ show current_state)

myBinding :: Binding ActiveWindow NumPadUnlockedInput
myBinding = startFrom 0 myBinding'  --------------------------------- (4)
```

Here, we have `myBinding'` of type `Binding'` **(1)**. The first type argument for `Binding'` (in this case, `Int`) is the type of this binding's state. To create a `Binding'` we use `binds'` function **(2)** instead of `binds`.

When you use `binds'` to create a stateful binding, you have to specify stateful actions. In this case, the actions are of the type `StateT Int IO ()` **(3)**. Functions for `StateT`, such as `modify`, `get` and `put`, are re-exported by WildBind.Task.X11, so you can use them to write stateful actions.

Finally, you have to convert `Binding'` into `Binding` with `startFrom` function **(4)**. This function gives a `Binding'` its initial state. In the above script, the state starts from zero.

Converting `Binding'` into `Binding` may seem that it discards the binding's state. Don't worry. The state is just hidden inside `Binding`. The state still exists, but it's not accessible anymore.

You can completely change keybinding based on the state of the `Binding'`. `ifBack` function is useful for that.

```haskell
myBinding' :: Binding' Int ActiveWindow NumPadUnlockedInput
myBinding' = ifBack (>= 10) forTooBigState
           $ forOtherCase
  where
    forTooBigState = binds' $ do
      on NumCenter `run` centerAction
      on NumDown `run` downAction
    upBinding = binds' $ do
      on NumUp `run` upAction
    forOtherCase = forTooBigState <> upBinding
```

The above script sets upper bound to the state. If the state reaches 10, the `upAction` is no longer bound.

`ifBack p b1 b2` creates a `Binding'` that chooses between `b1` and `b2`. `p` is a predicate for the state of the `Binding'`. If `p` is `True`, `b1` is enabled. Otherwise, `b2` is enabled.


## External Tools

TBW.

- xdotool
- wmctrl
- boring-window-switcher

## For Programmers

TBW

- wild-bind package family

## Author

Toshio Ito <debug.ito@gmail.com>
