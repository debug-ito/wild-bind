
JustWindowMain: JustWindowMain.hs
	ghc -threaded -Wall -package-db $(wildcard ../.cabal-sandbox/*packages.conf.d) -package-db ../dist/package.conf.inplace -o $@ $^

clean:
	rm -f JustWindowMain JustWindowMain.hi JustWindowMain.o
