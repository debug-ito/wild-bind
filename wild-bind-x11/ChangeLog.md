# Revision history for wild-bind-x11

## 0.2.0.11  -- ?

* Call [XInitThreads](https://linux.die.net/man/3/xinitthreads) at the start of `withX11Front'`.

## 0.2.0.10  -- 2020-06-21

* Confirm test with `base-4.14.0.0`

## 0.2.0.9  -- 2019-12-30

* Confirm test with `base-4.13.0.0`

## 0.2.0.8  -- 2019-10-04

* Confirm test with `time-1.9.3`.

## 0.2.0.7  -- 2019-05-11

* Confirm test with `semigroups-0.19`.


## 0.2.0.6  -- 2018-10-01

* Confirm test with `base-4.12.0.0`


## 0.2.0.5  -- 2018-09-23

* Confirmed test with `stm-2.5.0.0`.


## 0.2.0.4  -- 2018-06-19

* Confirmed test with `containers-0.6.0.1`.


## 0.2.0.3  -- 2018-04-22

* Confirmed test with `X11-1.9`.
* Now it requires XScreenSaver extension (in Debian/Ubuntu, `libxss-dev` package).


## 0.2.0.2  -- 2018-04-10

* Confirmed test with `base-4.11`.
* Stop using `ListT`, because it's deprecated since `transformers-0.5.3.0`.


## 0.2.0.1  -- 2018-02-06

* Confirmed test with `async-2.2.1`.


## 0.2.0.0  -- 2018-01-01

* Fix crash when some kind of window (e.g. xdvi) is active. 
* Remove `KeySymLike` and `ModifierLike` classes.
* Add `XMod`, `KeyEventType`, `XKeyEvent`, `X11Front`  types.
* Add `XKeyInput`, `ToXKeyEvent` classes.
* Add `withX11Front`, `makeFrontEnd`, `press`, `release`,
  `addXMod`, `shift`, `alt`, `ctrl`, `super`, `defaultRootWindow` functions.
* Add `WildBind.X11.Emulate`, `WildBind.X11.Emulate.Example` and `WildBind.X11.KeySym` modules.
* Changed signature of `xGrabKey` and `xUngrabKey` functions.


## 0.1.0.7  -- 2017-07-21

* Confirmed build with `time-1.8`.

## 0.1.0.6  -- 2017-02-10

* Confirmed build with `X11-1.8`.


## 0.1.0.5  -- 2017-01-24

* Confirmed build with `hspec-2.4.0`.


## 0.1.0.4  -- 2016-12-15

* Confirmed build with `X11-1.7`.


## 0.1.0.3  -- 2016-11-27

* Confirmed test with `time-1.7`.


## 0.1.0.2  -- 2016-10-09

* Confirmed build with `hspec-2.3.0`.


## 0.1.0.1  -- 2016-09-22

* First version. Released on an unsuspecting world.
