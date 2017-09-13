# Slay: a layouting engine

[![Build Status](https://img.shields.io/travis/int-index/slay.svg)](https://travis-ci.org/int-index/slay)

## Building instructions

The supported operating systems are Ubuntu and macOS. Building on Windows
should be possible, but it's not tested (AppVeyor integration would be welcome).

### Ubuntu

Using `apt-get`, install the following packages:

* `ghc-8.2.1` (from `ppa:hvr/ghc`)
* `cabal-install-2.0` (from `ppa:hvr/ghc`)
* `libgtk-3-dev`

### macOS

Using `homebrew`, install the following packages:

* `ghc`
* `cabal-install`
* `gtk+3`

### Common

Building:

```
cabal new-build all
```

To achieve reproducible builds the versions of Haskell dependencies are
pinned in the `cabal.project.freeze` file.

### Troubleshooting

Verify that you have the appropriate versions of GHC and Cabal in your `PATH`:

```
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.2.1

$ cabal --version
cabal-install version 2.0.0.0
compiled using version 2.0.0.2 of the Cabal library 
```

Verify that you have the development headers for GTK+ 3 installed:

```
$ pkg-config --modversion gtk+-3.0
3.18.9
```

## Project structure

The main package is `slay-core`, it contains the layouting engine. The engine
is abstracted over coordinate types using Backpack. There are two supplemental
packages to instantiate coordinates: `slay-number-double` and
`slay-number-integer`.

In `slay-core` there are two modules:

* `Slay.Number` is an abstract signature for coordinate types
* `Slay.Core` is the layouting engine itself

The only job of the layouting engine is to position primitives on a
2-dimensional plane. It doesn't matter whether the primitives are for a
pixel-based canvas or for a character-based terminal. Therefore, there are many
possible backends:

* `slay-cairo` implements a few primitives on top of the `cairo` drawing engine
* `slay-vty` (not implemented yet) will support terminals
* `slay-web` (not implemented yet) will support browsers

There are also packages that serve as a testing ground for development and provide
examples of using the library. For now there's only one such package, `slay-gtk`.
