include common.mk

ghc-test-flags := -package QuickCheck -package test-framework \
	-package test-framework-quickcheck -package HUnit \
	-package test-framework-hunit

.PHONY: run-tests
run-tests: tests
	./tests

tests: ghc-flags += $(ghc-test-flags)
tests: $(lib) Array.o PSQ.o RunTests.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

%.o: %.hs
	$(ghc) $(ghc-flags) $(ghc-opt-flags) -c -o $@ $<

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f tests
