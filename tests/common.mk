package := event
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
ghc := ghc
ghc-opt-flags ?= -O0
ghc-base-flags := -funbox-strict-fields -package criterion \
	-package bytestring -ignore-package $(package) \
	-fno-ignore-asserts
ghc-base-flags += -Wall -fno-warn-orphans -fno-warn-missing-signatures
ghc-flags := $(ghc-base-flags) -i../dist/build -package-name $(package)-$(version)
ghc-hpc-flags := $(ghc-base-flags) -fhpc -fno-ignore-asserts -odir hpcdir \
	-hidir hpcdir -i..
lib := ../dist/build/libHS$(package)-$(version).a
lib-srcs := $(shell grep '^    *System' ../$(package).cabal | \
                    sed -e 's,\.,/,g' -e 's,$$,.hs,')

cabal := $(shell which cabal 2>/dev/null)
