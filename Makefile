## Config
#########

GHC6 = ghc-6.12.3
GHC7 = ghc-7.0.2

GHC = $(GHC7)

GHCI = ghci-6.12.3

##############################################################################
## Benchmarks
##############################################################################

## All benchmarks
################# 

bench-all:
	$(GHC) --make -O2 -fforce-recomp -main-is BenchAll bench/BenchAll.hs
	./bench/BenchAll --resamples 10000

core-all:
	ghc-core -- --make -O2 -fforce-recomp -main-is BenchAll bench/BenchAll.hs

clean-bench-all:
	rm -f bench/*.o bench/*.hi
	rm -f test/BenchAll


##############################################################################
## Testing
##############################################################################

clean-test-all:
	rm -f test/*.o test/*.hi
	rm -f test/TestAll

test-all:
	$(GHC) --make -O2 -threaded -rtsopts -fforce-recomp -main-is TestAll test/TestAll.hs
	./test/TestAll -j2 -a 1000 +RTS -N2


##############################################################################
## Utilities
##############################################################################

clean: clean-bench-all clean-test-all

