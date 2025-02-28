EXECUTABLE = flp-fun

all: build

build:
	cabal install --install-method=copy --installdir=. --overwrite-policy=always

clean:
	rm -f $(EXECUTABLE)
	cabal clean
