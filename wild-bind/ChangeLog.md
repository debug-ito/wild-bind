# Revision history for wild-bind

## 0.1.2.0  -- 2018-04-10

* Add `Semigroup` instance to `Binding` and `SeqBinding`.
* Confirmed test with `base-4.11`.


## 0.1.1.1  -- 2018-03-14

* Confirmed test with `hspec-2.5.0`.
  Remove its dependency upper bound, because I think it's stable enough.


## 0.1.1.0  -- 2018-01-01

* Description: add `Describable` instance of `Either`.
* Binding: add some functions: 
  `bindsF`, `bindsF'`, `bindingF`, `bindingF'`, `revise`, `revise'`,
  `justBefore`, `justAfter`
* FrontEnd: derive `Eq` and `Ord` for `FrontEvent`.
* Add `WildBind.Seq` module.
* .cabal: use `other-extensions` instead of `default-extensions`.


## 0.1.0.3  -- 2017-01-24

* Confirmed build with `hspec-2.4.0`.


## 0.1.0.2  -- 2016-10-09

* Confirmed build with `hspec-2.3.0`.


## 0.1.0.1  -- 2016-09-22

* Fix URL in package description.


## 0.1.0.0  -- 2016-09-22

* First version. Released on an unsuspecting world.
