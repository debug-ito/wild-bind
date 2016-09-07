# wild-bind

WildBind core module.

See https://github.com/debug-ito/wild-bind for WildBind in general.

## Architecture and Terminology

WildBind consists of `FrontEnd` and `Binding`.

                     +-------------------+
    (user) --input-> |   desktop env.    |---[FrontEnd]---[Binding]
                     |                   |                    |
                     | (front-end state) |             (back-end state)
                     +-------------------+

- A `FrontEnd` interfaces with a desktop environment. It reads input from the user and the state of the desktop environment. The state is called "front-end state". `FrontEnd` passes those two kinds of data to `Binding`.
- A `Binding` binds actions to input symbols. Optionally it has its own state, which is called "back-end state".


## wild-bind Packages

## Author

Toshio Ito <debug.ito at gmail.com>

