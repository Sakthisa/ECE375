#define F_CPU 16000000
#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

int main(void)
{

    DDRB =0b11110000;
    PORTB=0b11110000;

    while (1) {
        PORTB=0b01100000;
        // If we get hit on left
        if ( !(PIND & 1) ){
            // Go back
            PORTB=0b00000000;	//Reverse
            _delay_ms(500);
            // turn right
            PORTB=0b00100000;	//Turn left
            _delay_ms(1000);
        }
        // If we get hit on right
        else if ( !(PIND & 2) ) {
            // Go back
            PORTB=0b00000000;	//Reverse
            _delay_ms(500);
            // turn right
            PORTB=0b01000000;	//Turn Right
            _delay_ms(1000);
        }
        // If both are touched
        else if ( !(PIND & 3) ){
            // Go back
            PORTB=0b00000000;	//Reverse
            _delay_ms(500);
            // turn right
            PORTB=0b00100000;	//Turn Left
            _delay_ms(1000);
        }
    };
}
