# Slay: a layouting engine

[![Build Status](https://img.shields.io/travis/int-index/slay.svg)](https://travis-ci.org/int-index/slay)

## Building instructions

```
./nix-cabal v2-build all
```

To achieve reproducible builds the versions of Haskell dependencies are
pinned in the `cabal.project.freeze` file.

Specify additional build options in `cabal.project.local`:

```
profiling: True
documentation: True
```

## Project structure

The main package is `slay-core`, it contains the layouting engine.
The only job of the layouting engine is to position primitives on a
2-dimensional plane. It doesn't matter whether the primitives are for a
pixel-based canvas or for a character-based terminal. Therefore, there are many
possible backends:

* `slay-cairo` implements a few primitives on top of the `cairo` drawing engine
* `slay-vty` implements a thin layer over `vty` for terminal pseudographics
* `slay-web` (not implemented yet) will support browsers

The `slay-combinators` package defines common positioning strategies, such as
horizontal and vertical juxtaposition, and various helper functions for
positioning. For now it's quite simplistic, but sophisticated positioning
algorithms can be added there in the future.

There are also packages that serve as a testing ground for development and provide
examples of using the library. For now there's only one such package, `slay-gtk`.
