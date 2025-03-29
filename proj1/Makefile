GHC = ghc
SRC = Main.hs
TARGET = flp-fun
FLAGS = -Wall -O2

all: $(TARGET)

$(TARGET): $(SRC)
	$(GHC) $(FLAGS) -o $(TARGET) $(SRC)

clean:
	rm -f $(TARGET) *.hi *.o

run: $(TARGET)
	./$(TARGET)

.PHONY: all clean run
