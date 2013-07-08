all:
	ghc -O2 redditmonad.hs

clean:
	rm -rf *.o *.hi Network/*.o Network/*.hi Network/Reddit/*.o Network/Reddit/*.hi