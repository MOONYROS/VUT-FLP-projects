EXECUTABLE = flp-fun

all: build

build:
	cabal install --install-method=copy --installdir=.

clean:
	rm -f $(EXECUTABLE)
	cabal clean
