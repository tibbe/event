ghc-opt-flags = -O2

include ../tests/common.mk

simple: $(lib) Args.o Simple.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

%.o: %.hs
	$(ghc) $(ghc-flags) $(ghc-opt-flags) -c -o $@ $<

clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -print0 | xargs -0r rm
	-rm -f simple
