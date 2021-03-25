# Revision history for wild-bind-task-x11

## 0.2.0.3  -- 2021-03-25

* Bug fix: Now `wildNumPad` initializes `WildBind.X11` first, and then `WildBind.Indicator`.
  It was the other way around before, and it caused Segmentation Fault when you tried to show the status icon menu or the description dialog.
  See https://github.com/debug-ito/wild-bind/issues/6

## 0.2.0.2  -- 2020-12-30

* Confirm build with `wind-bind-indicator-1.0.0.0`

## 0.2.0.1  -- 2017-01-02

* Fix dependency version bounds for wild-bind family packages.
  (gh#3, thanks to @peti)


## 0.2.0.0  -- 2018-01-01

* Modify constraints of `wildNumPad` and `wildNumPad'` functions
  because of the change in wild-bind-x11-0.2.0.0 package.
* Add `WildBind.Task.X11.Seq.Example` module.


## 0.1.0.1  -- 2016-09-22

* First version. Released on an unsuspecting world.
