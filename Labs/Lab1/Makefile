FILE="ece375-L1_v2"
build:
	../../avra-1.3.0/src/avra $(FILE).asm
	rm $(FILE).obj
	rm $(FILE).cof
	rm $(FILE).eep.hex

install:
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev0 -p m128 -e -U flash:w:$(FILE).hex -v

clean:
	rm $(FILE).hex
