include ../tests/common.mk

ghc-bench-flags := -package network -package network-bytestring \
	-package parsec-3.1.0 -package attoparsec -package bytestring-show \
	-package mtl

ifdef USE_GHC_IO_MANAGER
	ghc-bench-flags += -DUSE_GHC_IO_MANAGER
endif

ghc-opt-flags = -O2
cc = gcc
cc-opt-flags = -O2
programs := dead-conn deadconn pong-server signal simple static-http \
	thread-delay timers http-client

.PHONY: all
all: $(programs)

dead-conn: ghc-flags += $(ghc-bench-flags)
dead-conn: $(lib) Args.o EventUtil.o EventSocket.o DeadConn.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

# A C version of the above tool.
deadconn: deadconn.o
	$(cc) $(cc-opt-flags) -o $@ $(filter %.o,$^)

pong-server: ghc-flags += $(ghc-bench-flags)
pong-server: $(lib) Args.o EventUtil.o EventSocket.o PongServer.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

event-http: ghc-flags += $(ghc-bench-flags)
event-http: $(lib) EventUtil.o EventFile.o EventSocket.o NoPush.o RFC2616.o EventHttp.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

static-http: ghc-flags += $(ghc-bench-flags)
static-http: $(lib) EventUtil.o EventFile.o EventSocket.o NoPush.o RFC2616.o StaticHttp.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

http-client: ghc-flags += $(ghc-bench-flags)
http-client: $(lib) Args.o EventUtil.o EventSocket.o RFC2616.o HttpClient.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

signal: $(lib) Signal.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

simple: $(lib) Args.o Simple.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

thread-delay: ghc-flags += -package stm
thread-delay: $(lib) Args.o ThreadDelay.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

timers: $(lib) Args.o Timers.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

%.o: %.c
	$(cc) $(cc-opt-flags) -c -o $@ $<

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)
