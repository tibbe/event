ghc-opt-flags = -O2

include ../tests/common.mk

simple: $(lib) Args.o Simple.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

timers: ../dist/build/HSevent-0.1.o Args.o Timers.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ Args.o Timers.o $(lib)

%.o: %.hs
	$(ghc) $(ghc-flags) $(ghc-opt-flags) -c -o $@ $<

clean:
	-find . \( -name '*.o' -o -name '*.hi' \) | xargs rm
	-rm -f simple
