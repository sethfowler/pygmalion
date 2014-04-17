default:
	cabal configure -fbuild-pygindex-clang
	cabal build

install:
	cabal install --disable-documentation -fbuild-pygindex-clang

install-full:
	cabal install -fbuild-pygindex-clang

test:
	cabal configure --enable-tests -fbuild-pygindex-clang
	cabal build
	cabal test --show-details=always

clean:
	cabal clean
