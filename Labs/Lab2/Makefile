FILE="ece375-L2_skeleton"
build:
	avr-gcc -Wall -mmcu=atmega128 -o $(FILE).o $(FILE).c
	avr-objcopy -j .text -O ihex $(FILE).o $(FILE).hex
	rm $(FILE).o

install:
	/usr/local/bin/avrdude -c osuisp2 -P /dev/usb/hiddev0 -p m128 -e -U flash:w:$(FILE).hex -v

clean:
	rm *.o
	rm *.hex
