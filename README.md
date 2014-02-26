Pygmalion
=========

Pygmalion is a code indexing tool based on libclang for C and C++ (and
eventually other languages). The intention is to deliver the same kind of high
quality code navigation that expensive commercial IDEs give you, accessible in a
command-line form that fits the UNIX mindset and is easy to integrate into your
editor of choice.

Pygmalion will work with almost no configuration and no maintenance on any
project. It gathers information about your build system automatically when you
build with `pyg make`, and it detects changes to the files in your project and
updates its index automatically. Pygmalion is intended to Just Work.

`pyg` is the primary command-line interface to Pygmalion. Given a location in
the source code, it can print information about the item found there. This
includes definitions, references, the inheritance hierachy, callers and callees
of functions, and other useful data.  It can also provide metadata like the
compiler flags used for compiling a file, or generate a clang compilation
database or TAGS file for use with external tools.

Check the `examples/` directory to see how to integrate Pygmalion with
other tools, including `emacs` with
[irony-mode](https://github.com/Sarcasm/irony-mode) and `vim` with
[YouCompleteMe](https://github.com/Valloric/YouCompleteMe).

Requirements
============

- The Haskell Platform 2013.2.0. On OS X, you can install it via homebrew with
  `brew install haskell-platform`. On Ubuntu 13.10, `sudo apt-get install
  haskell-platform` should suffice. On other platforms, check
  [here](http://www.haskell.org/platform/).

- Up-to-date Haskell LibClang bindings. For now you'll need to build from source
  from the [Github repo](https://github.com/chetant/LibClang/tree/master).

- hslibvoyeur, which you can build from source from the
  [Github repo](https://github.com/sethfowler/hslibvoyeur/tree/master).

Build Instructions
==================

Run `make`. This will build the project and install it to your cabal executables
directory, by default usually `~/.cabal/bin`. You may need to add this to your
path.

Usage
=====

Change to the root of the project you want to index and run `pyg init` to
initialize a `.pygmalion` directory there. Pygmalion stores all of the
information about the project in this directory, including the configuration
file `pygmalion.yaml` which you can customize if needed. (See the next section.)
For simple `make`-based projects, the default configuration should be enough.

To actually index the project, run `pyg start-server` to start the indexing
daemon. Pygmalion will immediately start indexing your project. (FIXME: Except
default compiler flags are not implemented right now, so just use `pyg make`.)
For most projects, the default compiler flags will result in a poor quality
index, so if you didn't specify appropriate flags in the configuration file it's
advisable to immediately run a complete rebuild using `pyg make`. This will
gather accurate compiler flags and greatly improve the quality of the index.

You can start using `pyg` queries to navigate the code in your project
immediately. As new indexing results come in, results will become available for
more and more of your project. For very large projects, this may take a while,
but once you have the initial index the indexing daemon will automatically keep
it up to date as you make changes to the source, check out branches, and pull in
others' changes.

It's wise to always use `pyg make` to build so that Pygmalion picks up any
changes to your build system or compilation settings. Since you can configure
`pyg make` to run any command you want, this can actually be quite convenient:
no matter which project you're working on, you can always use the same command
to build.

Configuring Pygmalion
=====================

`pyg init` will create a default configuration file in
`.pygmalion/pygmalion.yaml` in your project root. You can customize this as
needed.

You will probably want to customize at least the value of `make`, which controls
which command `pyg make` runs. You can set this to the command you normally use
to build your project. There are two variables you can use in the value of `make`:

- `$(args)` will be set to whichever arguments you provide to `pyg make`. If you
  don't include this, by default the arguments will just be appended.

- `$(projectroot)` will be set to your project root, the directory which
  contains your `.pygmalion` directory. This can be convenient to make it
  possible to run `pyg make` from any directory in your project.

The command `pyg make` runs is evaluated by the shell after the variables are
substituted.

There are more options; the best source of information on them is currently
`examples/pygmalion.yaml.default` in the Pygmalion source tree, but these docs
will be improved soon.

Frequently Asked Questions
==========================

On Linux I get the error 'addWatch: resource exhausted (No space left on device)'.
----------------------------------------------------------------------------------

The default inotify watch limit is too small for your project. Unfortunately, on
many Linux installations the limit is unreasonably low by default. Try again
after running
`sudo sh -c 'echo 1048576 > /proc/sys/fs/inotify/max_user_watches'`.

You can make this fix permanent by adding `fs.inotify.max_user_watches=1048576`
to `/etc/sysctl.conf`.
