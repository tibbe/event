# To enable profiling you must pass the --enable-library-profiling
# flag to "cabal configure".
ghc-prof-flags :=
ifdef ENABLE_PROFILING
	ghc-prof-flags += -prof -hisuf p_hi -osuf p_o -auto-all
	lib-suffix := _p
else
	lib-suffix :=
endif

# To enable event logging you must pass the --ghc-option=-eventlog
# flag to "cabal configure".
ifdef ENABLE_EVENTLOG
	ghc-prof-flags += -eventlog
endif

package := event
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
ghc := ghc
ghc-opt-flags ?= -O0
ghc-base-flags := -funbox-strict-fields \
	-package bytestring -ignore-package $(package) \
	-fno-ignore-asserts
ghc-base-flags += -Wall -fno-warn-orphans -fno-warn-missing-signatures
ghc-flags := $(ghc-base-flags) -i../dist/build \
	-package-name $(package)-$(version) $(ghc-prof-flags)
ghc-hpc-flags := $(ghc-base-flags) -fhpc -fno-ignore-asserts -odir hpcdir \
	-hidir hpcdir -i..
lib := ../dist/build/libHS$(package)-$(version)$(lib-suffix).a
lib-srcs := $(shell grep '^    *System' ../$(package).cabal | \
                    sed -e 's,\.,/,g' -e 's,$$,.hs,')

cabal := $(shell which cabal 2>/dev/null)

%.o: %.hs
	$(ghc) $(ghc-flags) $(ghc-opt-flags) -c -o $@ $<

%.hs: %.hsc
	hsc2hs $<
