all: interp

interp: Hw6.hs
	ghc -o interp -main-is Hw6.main Hw6.hs

clean: 
	rm -rf *.o *.hi interp

