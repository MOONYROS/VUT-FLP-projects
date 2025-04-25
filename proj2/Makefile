TARGET=flp24-log
SOURCE=Main.pl
ZIP=flp-log-xlukas15.zip

.PHONY: all pack test clean

all:
	swipl -t halt -q -g start -o $(TARGET) -c $(SOURCE)

pack:
	zip -r $(ZIP) Makefile $(SOURCE) README.md test/

test:
	cd test && ./run.sh

clean:
	rm -f $(TARGET) $(ZIP)
