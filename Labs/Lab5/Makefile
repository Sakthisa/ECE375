TEXFILE="lab5-prelab"
lab4-prelab:
	pdflatex $(TEXFILE).tex
	rm $(TEXFILE).log
	rm $(TEXFILE).aux
	evince $(TEXFILE).pdf

FILE="ece375-L1_v2"
build:
	../../avra-1.3.0/src/avra $(FILE).asm
	rm $(FILE).obj
	rm $(FILE).cof
	rm $(FILE).eep.hex

install:
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev0 -p m128 -e -U flash:w:$(FILE).hex -v

FILE1="ece375-LCD_Test"
build-test:
	../../avra-1.3.0/src/avra $(FILE1).asm
	rm $(FILE1).obj
	rm $(FILE1).cof
	rm $(FILE1).eep.hex

install-test:
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev0 -p m128 -e -U flash:w:$(FILE1).hex -v

clean:
	rm $(FILE1).hex
	rm $(FILE1).hex

FILE2="ece375-L3_skeleton"
lab3-build:
	../../avra-1.3.0/src/avra $(FILE2).asm
	rm $(FILE2).obj
	rm $(FILE2).cof
	rm $(FILE2).eep.hex

lab3-install:
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev0 -p m128 -e -U flash:w:$(FILE2).hex -v

clean:
	rm $(FILE2).hex
	rm $(FILE2).hex
