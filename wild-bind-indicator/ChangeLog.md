# Revision history for wild-bind-indicator

## 1.0.0.0  -- ?

* Now it uses `gi-gtk` package, instead of `gtk` package. `gi-gtk` package has different prerequisites than `gtk`.
* Now it uses `async` package to clean-up the WildBind action when the Gtk+ GUI finishes. Probably fixes #4 and #5a.
* Confirm build with `containers-0.6.0.1`

## 0.2.0.0  -- 2018-01-01

* Modify constraints of `withNumPadIndicator`, `wildBindWithIndicator`, `bindingHook` functions.
* Add `adaptIndicator`, `toggleBinding` functions.

## 0.1.0.1  -- 2016-09-22

* First version. Released on an unsuspecting world.
