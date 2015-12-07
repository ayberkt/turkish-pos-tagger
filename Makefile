all:
	cabal configure && cabal build

fresh:
	cabal clean && cabal configure && cabal build

clean:
	cabal clean && rm -rf model && *.html
