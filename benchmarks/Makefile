ghc-opt-flags = -O2

include ../tests/common.mk

ghc-bench-flags := -package network -package network-bytestring

ifdef USE_GHC_IO_MANAGER
	ghc-bench-flags += -DUSE_GHC_IO_MANAGER
endif

.PHONY: all
all: pong simple thread-delay timers

pong: ghc-flags += $(ghc-bench-flags)
pong: $(lib) Args.o EventSocket.o PongServer.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

simple: $(lib) Args.o Simple.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

thread-delay: ghc-flags += $(ghc-bench-flags)
thread-delay: $(lib) Args.o ThreadDelay.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

timers: $(lib) Args.o Timers.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

%.o: %.hs
	$(ghc) $(ghc-flags) $(ghc-opt-flags) -c -o $@ $<

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f pong simple thread-delay timers
