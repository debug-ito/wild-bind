# WildBind

![icon](https://raw.githubusercontent.com/debug-ito/wild-bind/master/resource/icon.png) [![Haskell CI](https://github.com/debug-ito/wild-bind/actions/workflows/haskell.yml/badge.svg)](https://github.com/debug-ito/wild-bind/actions/workflows/haskell.yml)

WildBind is a dynamic and programmable key binding framework.

Features:

- It supports X11 desktop environments.
- It binds any action to keyboard events.
- Key bindings are written in Haskell (actually, it's just a bunch of Haskell modules).
- Key bindings can be **dynamic**, i.e. you can use different key bindings for different active windows.
- Key bindings can be **stateful**, e.g. you can bind actions to **sequences** of keys.

WildBind started as a binding framework for **number pad keys**, but now it supports more generic keyboard events.


## Contents

- [Getting Started](#getting-started)
- [Simplest Binding](#simplest-binding)
- [Combine Bindings](#combine-bindings)
- [Dynamic Binding Based on ActiveWindow](#dynamic-binding-based-on-activewindow)
- [Binding Override by <>](#binding-override-by-)
- [Binding Description](#binding-description)
- [Types and Modules](#types-and-modules)
- [Stateful Binding](#stateful-binding)
- [Binding for Generic Keys](#binding-for-generic-keys)
- [Key Event Emulation](#key-event-emulation)
- [Binding for Key Sequences](#binding-for-key-sequences)
- [External Tools](#external-tools)
- [See Also](#see-also)
- [Advanced Topics](#advanced-topics)


## Getting Started

We recommend `ghcup` to build WildBind.

1. Follow the instruction in https://www.haskell.org/ghcup/ to install `ghcup` command.
2. Install and enable ghc-9.2.5.

        $ ghcup install ghc 9.2.5
        $ ghcup set ghc 9.2.5

3. Install development packages for libx11 and GTK+. In Ubuntu, you can install them by

        $ sudo apt-get install libx11-dev libxss-dev libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev

4. Clone this repository and enter it.

        $ git clone https://github.com/debug-ito/wild-bind.git
        $ cd wild-bind

5. Build this repository.

        $ cabal build all

The last command triggers a lot of downloading and building. Be patient.

## Simplest Binding

Let's start with the simplest "Hello, world" binding. Save the following text as `simplest.hs` in the cloned Git directory.

```haskell simplest
{- cabal:
build-depends: base, wild-bind-task-x11
ghc-options: -threaded
-}
import WildBind.Task.X11

main = wildNumPad myBinding

myBinding = binds $ do
  on NumCenter `run` putStrLn "Hello, world!"
```

This binds an action to the 5 key (NumLock disabled) on a num pad.

To activate, run with `cabal run`.

    $ cabal run simplest.hs

When activated, it shows an icon on the status bar.

Then hit 5 key with NumLock disabled and it shows "Hello, world!" on the console.

To deactivate the binding, right-click the icon and select "Quit" item.

## Combine Bindings

Of course, you can bind actions to more than one keys. To do that, just repeat `on` statements.

```haskell head_process
{- cabal:
build-depends: base, wild-bind-task-x11
ghc-options: -threaded
-}
import WildBind.Task.X11
import System.Process (spawnCommand)

main = wildNumPad myBinding
```

```haskell combine
myBinding = binds $ do
  on NumCenter `run` putStrLn "Hello, world!"
  on NumPageUp `run` spawnCommand "firefox"
```

This script uses [System.Process](http://hackage.haskell.org/package/process/docs/System-Process.html) module and binds to the PageUp key an action to launch Firefox.

Or, you can combine independent bindings to get a complex binding by `<>` operator.

```haskell append
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

```haskell pushKey
pushKey key = spawnCommand ("xdotool key " <> key)  ----------------- (1)
```

```haskell dynamic
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

First, we define a support function `pushKey` **(1)**. It generates a fake keyboard input specified by the argument. For now we use an external tool called [xdotool](https://github.com/jordansissel/xdotool), but WildBind also supports generating keyboard input. See ["Key Event Emulation"](#key-event-emulation) below if interested.

Then we define `myBinding` **(2)**, which is combination of one for Firefox and one for VLC media player.

In the definition of `forFirefox`, we use `whenFront` function **(3)**. `whenFront` function adds a condition to the binding. The binding is active only when the predicate (`isFirefox`, in this case) returns `True`. The predicate `isFirefox` is defined later at **(5)**. It takes an `ActiveWindow` object, and checks if the active window is Firefox or not.

Binding definitions of `forFirefox` are just like previous examples **(4)**. Here we bind "Right tab" action to → key and "Left tab" action to ← key. However, these bindings are enabled only when a window for Firefox is active.

Definition of `forVLC` is similar to `forFirefox` **(6)**. This time we bind "Jump forward" action to → key and "Jump backward" action to ← key.

Using `whenFront` and `<>` functions like the above example, you can build bindings that adapt to the currently active window and Do What You Mean&trade;.

## Binding Override by `<>`

Binding combination operator `<>` prefers the right-hand binding. That is, if left-hand and right-hand bindings both bind actions to the same key, the right-hand binding wins.

This feature is often useful in combination of `whenFront`.

```haskell override
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

```haskell desc
myBinding = whenFront isFirefox $ binds $ do
  on NumRight `as` "Go to right tab" `run` pushKey "Ctrl+Tab"
  on NumLeft `as` "Go to left tab" `run` pushKey "Ctrl+Shift+Tab"
  where
    isFirefox active_window = winClass active_window == "Firefox"
```

Notice the `as` function is inserted between `on` and `run` functions.

To see the description of current binding, press "/" (divide) key on a num pad. It shows a window like this:

![(screen shot)](https://raw.githubusercontent.com/debug-ito/wild-bind/master/resource/shot_description_window.png)

Press "/" key again to hide the window.


## Types and Modules

Before entering the most complicated part of WildBind, let's introduce data types used in the previous examples.

The most important type is `Binding`, which is the type of `myBinding`.

```haskell
myBinding :: Binding ActiveWindow NumPadUnlocked
```

The above type means that `myBinding` binds actions to the input key type of `NumPadUnlocked`, and that it changes binding based on `ActiveWindow`.

`Binding` type is defined in [WildBind.Binding](https://hackage.haskell.org/package/wild-bind/docs/WildBind-Binding.html) module. This module also defines a lot of functions to build `Binding`, such as `binds`, `on`, `run`, `as` and `whenFront`.

`NumPadUnlocked` is the type for keys on a num pad when NumLock is disabled. It is defined in [WildBind.Input.NumPad](https://hackage.haskell.org/package/wild-bind/docs/WildBind-Input-NumPad.html) module. `NumCenter` and `NumLeft` are its data constructors. If you want to use WildBind with NumLock enabled, use `NumPadLocked`.

`ActiveWindow` is the type for an active window. It is defined in [WildBind.X11](https://hackage.haskell.org/package/wild-bind-x11/docs/WildBind-X11.html). You can inspect the window by accessor functions such as `winClass`.


## Stateful Binding


So far, bound actions are just plain `IO ()`, and `Binding` has no internal state.

```haskell stateful_top
myBinding :: Binding ActiveWindow NumPadUnlocked
myBinding = binds $ do
  on NumCenter `run` myAction

myAction :: IO ()
myAction = putStrLn "Hello, world!"
```

WildBind has a built-in support for **stateful keybindings**. A binding object can have its own state of arbitrary type, and behave differently according to the state.

```haskell stateful_full
{- cabal:
build-depends: base, wild-bind-task-x11
ghc-options: -threaded
-}
import WildBind.Task.X11

main = wildNumPad myBinding

myBinding' :: Binding' Int ActiveWindow NumPadUnlocked  ------------- (1)
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

myBinding :: Binding ActiveWindow NumPadUnlocked
myBinding = startFrom 0 myBinding'  --------------------------------- (4)
```

Here, we have `myBinding'` of type `Binding'` **(1)**. The first type argument for `Binding'` (in this case, `Int`) is the type of this binding's state. To create a `Binding'` we use `binds'` function **(2)** instead of `binds`.

When you use `binds'` to create a stateful binding, you have to specify stateful actions. In this case, the actions are of the type `StateT Int IO ()` **(3)**. Functions for `StateT`, such as `modify`, `get` and `put`, are re-exported by [WildBind.Task.X11](https://hackage.haskell.org/package/wild-bind-task-x11/docs/WildBind-Task-X11.html), so you can use them to write stateful actions.

Finally, you have to convert `Binding'` into `Binding` with `startFrom` function **(4)**. This function gives a `Binding'` its initial state. In the above script, the state starts from zero.

Converting `Binding'` into `Binding` may seem that it discards the binding's state. Don't worry. The state is just hidden inside `Binding`. The state still exists, but it's not accessible anymore.

You can completely change keybinding based on the state of the `Binding'`. `ifBack` function is useful for that.

```haskell stateful_condition
myBinding'' :: Binding' Int ActiveWindow NumPadUnlocked
myBinding'' = ifBack (>= 10) forTooBigState
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


## Binding for Generic Keys

So far we have made some bindings for number pad keys. WildBind also supports bindings for generic keys, such as `Ctrl + C` and `Alt + F1`. In this case, we cannot use the indicator window to show binding descriptions.

```haskell generic_keys
{- cabal:
build-depends: base, wild-bind-task-x11
ghc-options: -threaded
-}
import WildBind.Binding
  ( Binding, binds, on, as, run
  )
import WildBind.Exec (wildBind)
import WildBind.X11
  ( withFrontEnd, ActiveWindow, XKeyEvent,
    ctrl, alt
  )
import WildBind.X11.KeySym (xK_c, xK_F1)

main = withFrontEnd $ wildBind myBinding

myBinding :: Binding ActiveWindow XKeyEvent
myBinding = binds $ do
  on (ctrl xK_c) `as` "Ctrl + C" `run` putStrLn "Pushed Ctrl + C"
  on (alt xK_F1) `as` "Alt + F1" `run` putStrLn "Pushed Alt + F1"
```

Instead of `wildNumPad` function used so far, we use `withFrontEnd` and `wildBind` functions to implement `main`. `withFrontEnd` function creates the X11 FrontEnd object, and `wildBind` function combines the FrontEnd and `myBinding`.

This time the input key type of `myBinding` is `XKeyEvent`, which has the following three fields:

- X11 KeySym for the key. They are exported from [WildBind.X11.KeySym](https://hackage.haskell.org/package/wild-bind-x11/docs/WildBind-X11-KeySym.html).
- Whether the event is `press` or `release`. By default, it's `press`.
- Optional modifier keys such as `ctrl` and `alt`.

The above example makes binding to `Ctrl + C` and `Alt + F1`. This means you can use WildBind for a generic key binding tool such as [XBindKeys](http://www.nongnu.org/xbindkeys/xbindkeys.html). WildBind, however, supports dynamic binding and Haskell programming.


## Key Event Emulation

WildBind can emulate key events, that is, generate fake (synthetic) keyboard input events.

See [WildBind.X11.Emulate](https://hackage.haskell.org/package/wild-bind-x11/docs/WildBind-X11-Emulate.html) module and [its example](https://hackage.haskell.org/package/wild-bind-x11/docs/WildBind-X11-Emulate-Example.html) for detail.

## Binding for Key Sequences

The module [WildBind.Seq](https://hackage.haskell.org/package/wild-bind/docs/WildBind-Seq.html) exports some functions to build binding for key sequences. Check out [the module documentation](https://hackage.haskell.org/package/wild-bind/docs/WildBind-Seq.html) and [its example](https://hackage.haskell.org/package/wild-bind-task-x11/docs/WildBind-Task-X11-Seq-Example.html) for detail.

## External Tools

There are some tools that are pretty useful in combination with WildBind. You can use those tools via  [System.Process](http://hackage.haskell.org/package/process/docs/System-Process.html) module.

- [xdotool](https://github.com/jordansissel/xdotool): We have already introduced this module above. It is an automation tool for X11. It inspects windows, manipulates windows and generates keyboard/mouse events.
- [xautomation](https://www.hoopajoo.net/projects/xautomation.html): It is similar to xdotool, but it also contains a program called `visgrep`.  `visgrep` is a simple image matching tool. It searches an image for an image pattern, and returns its location.
- [wmctrl](https://sites.google.com/site/tstyblo/wmctrl): It is a tool to interact with window managers for X11. It inspects and manipulates windows.
- [xprop](https://www.x.org/archive/X11R7.5/doc/man/man1/xprop.1.html): A tool to inspect X11 windows. You can inspect `WM_CLASS` property of a window.
- [boring-window-switcher](https://github.com/debug-ito/boring-window-switcher): It is a pretty simple window switcher for X11. It is useful when you want to switch windows ONLY WITH YOUR NUMBER PAD.

## See Also

Other tools similar to WildBind.

- [XBindKeys](http://www.nongnu.org/xbindkeys/): A famous key binding daemon. Like wild-bind-x11, it uses X11 events to capture key inputs.
- [xremap](https://github.com/k0kubun/xremap): A key remapper that supports Ruby DSL. Like wild-bind-x11, it uses X11 events to capture key inputs.
- [rbindkeys](https://github.com/kui/rbindkeys): Like xremap, but this uses Linux Input Subsystem to capture key inputs.
- [xkeysnail](https://github.com/mooz/xkeysnail): A key remmaper and binding tool that supports Python 3 DSL. It uses Linux Input Subsystem.

## Advanced Topics

Advanced topics, such as wild-bind package architecture, can be found in [wild-bind's package README](https://github.com/debug-ito/wild-bind/tree/master/wild-bind).


## Author

Toshio Ito <debug.ito@gmail.com>
