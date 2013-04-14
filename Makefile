default:
	cabal-dev install
	cp cabal-dev/bin/pyg* ~/.cabal/bin/

test:
	cabal-dev configure --enable-tests
	cabal-dev install
	cabal-dev test

clean:
	cabal-dev clean
