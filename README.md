Pygmalion
=========

A code indexing tool based on libclang for C, C++, and Objective C. The
intention is to deliver the same kind of high quality code navigation that
expensive commercial IDEs give you, accessible in a command-line form that fits
the UNIX mindset and is easy to integrate into your editor of choice. Pygmalion
consists of several utilities that work together:

* `pygd` is the indexing daemon. It needs to be running for the other tools to
  work. It will automatically index files that are touched in the current
  directory, so you just need to save for Pygmalion to see your changes. You
  should start `pygd` at the root of the project you want it to index.

* `pygmake` is a wrapper for `make` that assists with indexing. In particular,
  `pygmake` triggers indexing of source files compiled by `make` and records the
  compiler flags they're compiled with. This ensures that the index of each file
  is as accurate as possible.

* `pygmalion` is the utility that users are intended to interact with. Currently
  it can print metadata like compiler flags used for a file, generate a clang
  compilation database, and give you the definition for the identifier at a
  particular location in a file. It will gain more features with time.

Check the `examples/` directory to see how to integrate Pygmalion with other
tools, including `vim` and
[YouCompleteMe](https://github.com/Valloric/YouCompleteMe).

Requirements
============

- Up-to-date libclang. Use `brew install --with-clang --all-targets --rtti
  --universal --jit llvm` on OS X.

- Up-to-date Haskell LibClang bindings. For now you'll need the `safer-api`
  branch at [my fork](https://github.com/sfowler/LibClang/tree/safer-api), but I
  intend to merge that branch into the main repo soon.

Build Instructions
==================

Since this project is in the very early stages, no effort has been made to make
it easy to install. You can try `cabal install` if you dare. (It should work
without issue, though.)
