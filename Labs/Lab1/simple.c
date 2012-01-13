#include <inttypes.h>
#include <avr/io.h>

int main(void)
{
    DDRB = 0xFF;         // Set Port B to outputs
    uint8_t data = 0x00;

    while(1){
        data++;
        PORTB = data;
    }
}

