Pygmalion
=========

A code analysis tool based on libclang. Pygmalion can be used wherever you'd use
make; just invoke `pygmake` instead of `make`. This will automatically create
`.pygmalion.sqlite` and `compile_commands.json` in the current directory.
`compile_commands.json` is a [clang compilation
database](http://clang.llvm.org/docs/JSONCompilationDatabase.html). You can use
it with several libclang-based tools - for example, you can get high quality
code completion for vim using
[YouCompleteMe](https://github.com/Valloric/YouCompleteMe). `.pygmalion.sqlite`
is a database of information about your project. In time you'll be able to use
the `pygmalion` utility to make a number of queries useful for code analysis and
navigation; right now the only thing you can do with `pygmalion` is manually
generate the `compile_commands.json` database.

Requirements:

- Up-to-date libclang. Use `brew install --with-clang --all-targets --rtti
  --universal --jit llvm` on OS X.
- Up-to-date Haskell LibClang bindings. Check out the `llvm3.2` branch at
  `https://github.com/chetant/LibClang/tree/llvm3.2`.
