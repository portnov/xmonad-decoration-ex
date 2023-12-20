# xmonad-decoration-ex

This repository contains code for advanced window decorations mechanism for XMonad.

![Screenshot_20231206_163349](https://github.com/portnov/xmonad-decoration-ex/assets/284644/98437e3c-5d4e-467d-a667-1c7092038ebe)

## What this is

This set of modules contains a set of type classes and their implementations
which define a flexible and extensible mechanism of window decorations.

Within this mechanism, there are the following entities which define
how decorations will look and work:

* Main object is `DecorationEx` layout modifier. It is from where everyting
  starts. It creates, shows and hides decoration windows (rectangles) when
  needed. It is parametrized with decoration geometry, decoration engine and
  theme. It calls these components to do their part of work.
* `DecorationGeometry` defines where decoration rectangles should be placed.
  For example, standard horizontal bar above each window; or tab bar.
* `DecorationEngine` defines how decorations look and how they react on clicks.
  Different implementations of decoration engine can use different API
  to draw decorations. Within this package, there is one implementation 
  (`TextDecoration`), which uses plain Xlib calls, and displays decoration
  widgets with text fragments, like `[X]` or `[_]`. Other engines can, for
  example, use Cairo library to draw nice gradients and image-based widgets.
* Decoration widget is an element placed on window decoration. It defines how
  it looks and how it responds to clicks. Examples include usual window 
  buttons (minimize, maximize, close), window icon, window title.
* Decoration theme defines colors and fonts for decoration engine. It also
  contains a list of decoration widgets and says where to place them (at the
  left, at the right or in the center).

This mechanism makes a huge use of parametrized data types and type families,
in order to make it possible to define different types of decorations, and
easily combine different aspects of decorations. For example, each decoration
engine can be combined with each decoration geometry.

See haddock documentation for more details.

## Current state

This bunch of code is intended to be included into xmonad-contrib eventually.
It may appear that inclusion into xmonad-contrib will take time or will require
further refactoring. That, probably, will break compatibility with configs of
users of xmonad-decorations-ex, if there will be such. We certainly do not want
to break compatibility for users of plain xmonad-contrib.

Until then, xmonad-decorations-ex will stay as a separate package. But since it
is possible that the code will be in xmonad-contrib, I am not in a hurry to put
this package to hackage.

## Usage

You can use it as any other haskell package. Add to your `stack.yaml` in your
xmonad config directory:

```
extra-deps:
  - git: https://github.com/portnov/xmonad-decoration-ex.git
    commit: 78..... (you have to put full SHA1 of last commit here)
```

Or you can clone this repository to your machine and write

```
extra-deps:
    - /home/user/src/xmonad-decoration-ex
```

Then, in your `xmonad.hs`, write something like

```
import XMonad.Layout.DecorationEx

myL = textDecoration shrinkText (layoutHook def)

main = xmonad def { layoutHook = myL }
```

## Cairo decorations 

This whole mechanism was implemented with idea of making decorations more fancy
and nicely looking, that what we currently have in xmonad-contrib. That is hard
to implement with plain Xlib calls. I was considering different graphics
libraries and their Haskell bindings, and for now I ended up with cairo
library.

So, currently there is
[xmonad-decoration-cairo](https://github.com/portnov/xmonad-decoration-cairo)
repository, which uses xmonad-decoration-ex as a base to draw fancy decorations
with cairo library.

## License

BSD-3, see LICENSE.

