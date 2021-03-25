# Revision history for wild-bind-indicator

## 1.0.0.1  -- 2021-03-25

* Documentation update. Now `wildNumPad` is not necessarily the first function to call.
  See https://github.com/debug-ito/wild-bind/issues/6

## 1.0.0.0  -- 2020-12-30

* **MAJOR CHANGE**: Now it uses `gi-gtk` package, instead of `gtk` package.
  Although this doesn't change the API of wild-bind-indicator, it changes the prerequisite packages/libraries significantly.
  Thus we bumped the major version.
* Now it uses `async` package to clean-up the WildBind action when the Gtk+ GUI finishes. Probably fixes #4 and #5a.
* Confirm build with `containers-0.6.0.1`

## 0.2.0.0  -- 2018-01-01

* Modify constraints of `withNumPadIndicator`, `wildBindWithIndicator`, `bindingHook` functions.
* Add `adaptIndicator`, `toggleBinding` functions.

## 0.1.0.1  -- 2016-09-22

* First version. Released on an unsuspecting world.
