GHC=ghc
GHCOPTS=-Wall

all:
	@echo "c:clean"
	@echo "b:uild"
	@echo "t:est"

clean:
	\rm -rf bin *.o *.hi Tests.tix Tests Aldus/*.o Aldus/*.hi Aldus/Main

bin:
	mkdir bin

Aldus/Main: Aldus/Main.hs
	$(GHC) $(GHCOPTS) --make Aldus/Main.hs

bin/aldus: bin Aldus/Main
	cp Aldus/Main bin/aldus

build: bin/aldus

Tests: Tests.hs
	$(GHC) $(GHCOPTS) -fhpc --make Tests

test: aldus Tests
	./Tests

# aliases
a: all
c: clean
b: build
t: test

