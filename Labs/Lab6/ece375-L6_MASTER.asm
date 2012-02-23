;***********************************************************
;*
;*	Enter Name of file here
;*
;*	Enter the description of the program here
;*
;*	This is the RECEIVE skeleton file for Lab 6 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Enter your name
;*	   Date: Enter Date
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************

.def    mpr = r16               ; Multi-Purpose Register
.def    waitcnt = r17               ; Wait Loop Counter
.def    ilcnt = r18             ; Inner Loop Counter
.def    olcnt = r19             ; Outer Loop Counter
.def    tmp = r30
.def	rec = r22				; Multi-Purpose Register


.equ    WTime = 50             ; Time to wait in wait loop

.equ	B0 = 0b11111110				; Right Whisker Input Bit
.equ	B1 = 0b11111101
.equ	FROZEN = 0b01010101
.equ	FREEZE = 0b11111000
.equ	B3 = 0b11110111
.equ	B4 = 0b11101111
.equ	B5 = 0b11011111
.equ	B6 = 0b10111111

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit
.equ	EngEnR = 4				; Right Engine Enable Bit
.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit

;.equ	BotID = ;(Enter you group ID here (8bits)); Unique XD ID (MSB = 0)
.equ	BotID = 0b00010001 ;(Enter you group ID here (8bits)); Unique XD ID (MSB = 0)

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MovFwd =  (1<<EngDirR|1<<EngDirL)	;0b01100000 Move Forwards Command
.equ	MovBck =  $00						;0b00000000 Move Backwards Command
.equ	TurnR =   (1<<EngDirL)				;0b01000000 Turn Right Command
.equ	TurnL =   (1<<EngDirR)				;0b00100000 Turn Left Command
.equ	Halt =    (1<<EngEnR|1<<EngEnL)		;0b10010000 Halt Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

;Should have Interrupt vectors for:
;.org    $003C
;    rcall USART_Receive
;    reti
;- Left wisker
;- Right wisker
;- USART receive


;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:
        ;Stack Pointer (VERY IMPORTANT!!!!)
        ldi     mpr, low(RAMEND)
        out     SPL, mpr    ; Load SPL with low byte of RAMEND
        ldi     mpr, high(RAMEND)
        out     SPH, mpr    ; Load SPH with high byte of RAMEND
        ;I/O Ports
        ; Initialize Port B for output
        ldi     mpr, $00        ; Initialize Port B for outputs
        out     PORTB, mpr      ; Port B outputs low
        ldi     mpr, $ff        ; Set Port B Directional Register
        out     DDRB, mpr       ; for output

        ; Initialize Port D for inputs
        ldi     mpr, $FF        ; Initialize Port D for inputs
        out     PORTD, mpr      ; with Tri-State
        ldi     mpr, $00        ; Set Port D Directional Register
        out     DDRD, mpr       ; for inputs

        ;USART1
USART_INIT:
        ;Set double data rate
        ldi r16, (1<<U2X1)
        ; UCSR1A control register -- Bit 1 – U2Xn: Double the USART Transmission Speed
        sts UCSR1A, r16

        ;Set baudrate at 2400bps
        ; UBRR1H Bod rate control register
        ldi r16, high(832)
        sts UBRR1H, r16
        ldi r16, low(832)
        sts UBRR1L, r16


		;Set frame format: 8data bits, 2 stop bit
        ldi r16, (0<<UMSEL1|1<<USBS1|1<<UCSZ11|1<<UCSZ10)
        sts UCSR1C, r16

        ;Enable both receiver and transmitter -- needed for Lab 2
        ldi r16, (1<<RXCIE1|1<<RXEN1|1<<TXEN1) ; RXEN (Receiver enable) TXEN (Transmit enable)
        sts UCSR1B, r16

	;External Interrupts
        ; Turn on interrupts
        sei ; This may be redundant

		;Enable receiver and enable receive interrupts
		;Set the External Interrupt Mask

        ; Set the Interrupt Sense Control to falling edge
        ldi mpr, (1 <<ISC41)|(0 <<ISC40)|(1 <<ISC51)|(0 <<ISC50)
        out EICRB, mpr

	;Other


;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:
        ldi   mpr, $00
        in    mpr, PIND       ; Get whisker input from Port D
        cpi   mpr, B0
        breq   BUTTON0

        in    mpr, PIND       ; Get whisker input from Port D
        cpi   mpr, B1
        breq   BUTTON1

        in    mpr, PIND       ; Get whisker input from Port D
        cpi   mpr, FREEZE
        breq   FREEZE

        in    mpr, PIND       ; Get whisker input from Port D
        cpi   mpr, B3
        breq   BUTTON3

        in    mpr, PIND       ; Get whisker input from Port D
        cpi   mpr, B4
        breq   BUTTON4


        in    mpr, PIND       ; Get whisker input from Port D
        cpi   mpr, B5
        breq   BUTTON5

        in    mpr, PIND       ; Get whisker input from Port D
        cpi   mpr, B6
        breq   BUTTON6

        rjmp MAIN



BUTTON0:
        ; Load bot id
        ldi mpr, BotID
        ; Send bot id
        call USART_Transmit

        ldi   mpr, 0b00000001
        out PORTB, mpr
        call USART_Transmit
        jmp MAIN
BUTTON1:
        ; Load bot id
        ldi mpr, BotID
        ; Send bot id
        call USART_Transmit

        ldi   mpr, 0b00000010
        out PORTB, mpr
        call USART_Transmit
        jmp MAIN
FREEZE:
        ; Load bot id
        ldi mpr, BotID
        ; Send bot id
        call USART_Transmit

        ldi   mpr, FREEZE
        out PORTB, mpr
        call USART_Transmit
        jmp MAIN
BUTTON3:
        ; Load bot id
        ldi mpr, BotID
        ; Send bot id
        call USART_Transmit

        ldi   mpr, 0b00001000
        out PORTB, mpr
        call USART_Transmit
        jmp MAIN
BUTTON4:
        ; Load bot id
        ldi mpr, BotID
        ; Send bot id
        call USART_Transmit

        ldi   mpr, 0b00010000
        out PORTB, mpr
        call USART_Transmit
        jmp MAIN
BUTTON5:
        ; Load bot id
        ldi mpr, BotID
        ; Send bot id
        call USART_Transmit

        ldi   mpr, 0b00100000
        out PORTB, mpr
        call USART_Transmit
        jmp MAIN
BUTTON6:
        ; Load bot id
        ldi mpr, BotID
        ; Send bot id
        call USART_Transmit

        ldi   mpr, 0b01000000
        out PORTB, mpr
        call USART_Transmit
        rjmp MAIN


        ; Load bot id
        ldi mpr, BotID
        ; Send bot id
        call USART_Transmit

        ; Load tmp + 1
        adiw tmp, $1
        mov mpr, tmp
        ; Send bot id
        call USART_Transmit
        out  PORTB, tmp

        ldi waitcnt, WTime ; Wait for 1 second
; -------------------------------
        push    waitcnt         ; Save wait register
        push    ilcnt           ; Save ilcnt register
        push    olcnt           ; Save olcnt register

Loop1:   ldi     olcnt, 224      ; load olcnt register
OLoop1:  ldi     ilcnt, 237      ; load ilcnt register
ILoop1:  dec     ilcnt           ; decrement ilcnt
        brne    ILoop1           ; Continue Inner Loop
        dec     olcnt       ; decrement olcnt
        brne    OLoop1           ; Continue Outer Loop
        dec     waitcnt     ; Decrement wait
        brne    Loop1            ; Continue Wait loop

        pop     olcnt       ; Restore olcnt register
        pop     ilcnt       ; Restore ilcnt register
        pop     waitcnt     ; Restore wait register
; -------------------------------

rjmp MAIN


;***********************************************************
;*	Functions and Subroutines
;***********************************************************
; USART Receive
USART_Receive:
    ; Wait for data to be received
    lds rec, UCSR1A
    sbrs rec, RXC1
    rjmp USART_Receive

    ; Get and return receive data from receive buffer
    lds rec, UDR1
    ret

USART_Transmit:
    lds r23, UCSR1A
    sbrs r23, UDRE1
    ; Load status of USART1
    ; Loop until transmit data buffer is ready
    rjmp USART_Transmit

    ; Send data
    sts UDR1, mpr
    ; Move data to transmit data buffer
    ret





;***********************************************************
;*	Stored Program Data
;***********************************************************



;***********************************************************
;*	Additional Program Includes
;***********************************************************

;----------------------------------------------------------------
; Sub:  Wait
; Desc: A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;       waitcnt*10ms.  Just initialize wait for the specific amount 
;       of time in 10ms intervals. Here is the general eqaution
;       for the number of clock cycles in the wait loop:
;           ((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;----------------------------------------------------------------
Wait:
        push    waitcnt         ; Save wait register
        push    ilcnt           ; Save ilcnt register
        push    olcnt           ; Save olcnt register

Loop:   ldi     olcnt, 224      ; load olcnt register
OLoop:  ldi     ilcnt, 237      ; load ilcnt register
ILoop:  dec     ilcnt           ; decrement ilcnt
        brne    ILoop           ; Continue Inner Loop
        dec     olcnt       ; decrement olcnt
        brne    OLoop           ; Continue Outer Loop
        dec     waitcnt     ; Decrement wait 
        brne    Loop            ; Continue Wait loop    

        pop     olcnt       ; Restore olcnt register
        pop     ilcnt       ; Restore ilcnt register
        pop     waitcnt     ; Restore wait register
        ret             ; Return from subroutine
