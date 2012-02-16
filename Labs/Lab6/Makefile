TEXFILE="lab6-prelab"
prelab:
	pdflatex $(TEXFILE).tex
	rm $(TEXFILE).log
	rm $(TEXFILE).aux
	evince $(TEXFILE).pdf

FILE="ece375-L6"
build:
	../../avra-1.3.0/src/avra $(FILE).asm
	rm $(FILE).obj
	rm $(FILE).cof
	rm $(FILE).eep.hex

install:
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev0 -p m128 -e -U flash:w:$(FILE).hex -v

clean:
	rm *.hex
