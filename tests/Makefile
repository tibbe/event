include common.mk

ghc-test-flags := -package QuickCheck-1.2.0.0 -package test-framework \
	-package test-framework-quickcheck -package HUnit \
	-package test-framework-hunit -package network

.PHONY: run-tests
run-tests: tests
	./tests

tests: ghc-flags += $(ghc-test-flags)
tests: $(lib) Array.o Manager.o PSQ.o Regression.o RunTests.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f tests
