FILE="player"
pbuild:
	../../avra-1.3.0/src/avra $(FILE).asm
	rm $(FILE).obj
	rm $(FILE).cof
	rm $(FILE).eep.hex

pinstall:
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev0 -p m128 -e -U flash:w:$(FILE).hex -v

FILE1="master"
mbuild:
	../../avra-1.3.0/src/avra $(FILE1).asm
	rm $(FILE1).obj
	rm $(FILE1).cof
	rm $(FILE1).eep.hex

minstall:
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev0 -p m128 -e -U flash:w:$(FILE1).hex -v

wu:
	pdflatex l7.tex
	evince l7.pdf

clean:
	rm $(FILE).hex
