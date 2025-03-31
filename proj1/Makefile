GHC = ghc
SRC = Main.hs
TARGET = flp-fun
FLAGS = -Wall

all: $(TARGET)

$(TARGET): $(SRC)
	$(GHC) $(FLAGS) -o $(TARGET) $(SRC)

clean:
	rm -f $(TARGET) *.hi *.o

.PHONY: all clean run
