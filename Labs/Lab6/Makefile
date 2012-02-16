TEXFILE_MASTER="lab6-prelab"
prelab:
	pdflatex $(TEXFILE_MASTER).tex
	rm $(TEXFILE_MASTER).log
	rm $(TEXFILE_MASTER).aux
	evince $(TEXFILE_MASTER).pdf

FILE_MASTER="ece375-L6_MASTER"
FILE_SLAVE="ece375-L6_SLAVE"
build:
	../../avra-1.3.0/src/avra $(FILE_MASTER).asm
	../../avra-1.3.0/src/avra $(FILE_SLAVE).asm
	rm $(FILE_MASTER).obj
	rm $(FILE_MASTER).cof
	rm $(FILE_MASTER).eep.hex
	rm $(FILE_SLAVE).obj
	rm $(FILE_SLAVE).cof
	rm $(FILE_SLAVE).eep.hex

install:
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev0 -p m128 -e -U flash:w:$(FILE_MASTER).hex -v
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev1 -p m128 -e -U flash:w:$(FILE_SLAVE).hex -v

clean:
	rm *.hex
