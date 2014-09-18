all:
	cabal sandbox init
	cabal install

install:
	cp .cabal-sandbox/bin/ztail /usr/local/bin/ztail
