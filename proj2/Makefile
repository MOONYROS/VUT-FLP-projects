TARGET=flp24-log
SOURCE=main.pl
ZIP=flp-log-xlukas15.zip

.PHONY: all pack clean

all:
	swipl -t halt -q -g start -o $(TARGET) -c $(SOURCE)

pack:
	zip -r $(ZIP) Makefile $(SOURCE) README.md test/

clean:
	rm -f $(TARGET) $(ZIP)
