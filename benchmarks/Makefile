ghc-opt-flags = -O2
cc = gcc
cc-opt-flags = -O2

include ../tests/common.mk

ghc-bench-flags := -package network -package network-bytestring

ifdef USE_GHC_IO_MANAGER
	ghc-bench-flags += -DUSE_GHC_IO_MANAGER
endif

ifdef USE_EVENTLOG
	ghc-bench-flags += -eventlog
endif

programs := dead-conn deadconn pong-server signal simple thread-delay timers

.PHONY: all
all: $(programs)

dead-conn: ghc-flags += $(ghc-bench-flags)
dead-conn: $(lib) Args.o EventSocket.o DeadConn.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

# A C version of the above tool.
deadconn: deadconn.o
	$(cc) $(cc-opt-flags) -o $@ $(filter %.o,$^)

pong-server: ghc-flags += $(ghc-bench-flags)
pong-server: $(lib) Args.o EventSocket.o PongServer.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

signal: $(lib) Signal.o
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

%.o: %.c
	$(cc) $(cc-opt-flags) -c -o $@ $<

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)
