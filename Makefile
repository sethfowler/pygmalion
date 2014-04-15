default:
	cabal build --disable-documentation -fbuild-pygindex-clang

install:
	cabal install --disable-documentation -fbuild-pygindex-clang

install-full:
	cabal install -fbuild-pygindex-clang

test:
	cabal configure --enable-tests -fbuild-pygindex-clang
	cabal build --disable-documentation
	cabal test --show-details=always

clean:
	cabal clean
