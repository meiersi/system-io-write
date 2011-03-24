
##############################################################################
## Benchmarks
##############################################################################

## Config
#########

GHC6 = ghc-6.12.3
GHC7 = ghc-7.0.2

GHC = $(GHC7)

GHCI = ghci-6.12.3


## All benchmarks
#################

bench-all: bench-hex

clean-bench-all:
	rm -f benchmarks/*.o benchmarks/*.hi

## Individual benchmarks
########################

# hexadeximal encoding
bench-hex:
	$(GHC) --make -O2 -fforce-recomp -main-is Hex benchmarks/Hex.hs
	./benchmarks/Hex --resamples 10000

core-hex:
	ghc-core -- --make -O2 -fforce-recomp -main-is Hex benchmarks/Hex.hs
