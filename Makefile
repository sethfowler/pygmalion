default:
	cabal install -fbuild-pygindex-clang

test:
	cabal configure --enable-tests -fbuild-pygindex-clang
	cabal build
	cabal test

clean:
	cabal clean
