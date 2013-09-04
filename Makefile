default:
	cabal install -fbuild-pygclangindex

test:
	cabal configure --enable-tests -fbuild-pygclangindex
	cabal build
	cabal test

clean:
	cabal clean
