default:
	cabal install -fbuild-pygclangindex

test:
	cabal configure --enable-tests
	cabal install -fbuild-pygclangindex
	cabal test

clean:
	cabal clean
