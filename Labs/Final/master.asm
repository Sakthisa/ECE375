; Riley Hickman & Jacques Uber
; ECE375 Final Lab
; master.asm

.include "m128def.inc"			; Include the ATMega128 Definition Doc
.def	mpr = r16				; Multi-purpose register defined for LCDDV2
.def	game_state = r5			; Game State
.def    best_botId = r7
.def    best_score = r8
.def    tmp_botid        = r9
.def    tmp2        = r10
                                ; State 0 = Waiting For players to send scores
.def    players_active = r11         ; Used to tell if all players have reported in their hands.
.def	ReadCnt = r23			; Counter used to read data from Program Memory
.equ	CountAddr = $0130		; Address of ASCII counter text
.def	counter = r4			; Counter used for Bin2ASCII
.def	rec = r24			; Counter used for Bin2ASCII
.equ	Round = $0200		    ; Address Of Round Count
.equ	BestBot  = $0202		    ; Address of BotId with the best score
.equ	BestScore  = $0206		    ; Address of Best Score

; Controls
.equ	BStartGame     = 0b11111110				; Right Whisker Input Bit
.equ	BStay          = 0b11111101
.equ    NewGame        = 0b10000111
.equ    NewRound       = 0b10011111

;BotId
.equ    BotID = 0b10101010
.equ    WinID = 0b10101011



.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

.org    $003C
rcall USART_Receive
reti

;***********************************************************
;*	Program Initialization
;***********************************************************
.org	$0046					; Origin of the Program, after IVs
INIT:							; Initialize Stack Pointer
		ldi		mpr, HIGH(RAMEND)
		out		SPH, mpr
		ldi		mpr, LOW(RAMEND)
		out		SPL, mpr

		rcall	LCDInit			; INITIALIZE THE LCD DISPLAY

        ; Initialize Port D for inputs
        ldi     mpr, $FF        ; Initialize Port D for inputs
        out     PORTD, mpr      ; with Tri-State
        ldi     mpr, $00        ; Set Port D Directional Register
        out     DDRD, mpr       ; for inputs

USART_INIT:
        ;Set double data rate
        ldi r16, (1<<U2X1)
        ; UCSR1A control register -- Bit 1 – U2Xn: Double the USART Transmission Speed
        sts UCSR1A, r16

        ; Initialize Port B for output
        ldi     mpr, $00        ; Initialize Port B for outputs
        out     PORTB, mpr      ; Port B outputs low
        ldi     mpr, $ff        ; Set Port B Directional Register
        out     DDRB, mpr       ; for output

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

		; Activate interrupts
		sei						; Turn on interrupts
        ldi     mpr, 1
        mov     players_active, mpr
        clr     game_state
MAIN:
        ; Loop until all players have checked in.
        mov     mpr, players_active
        cpi     mpr, 0
        breq    START_NEWROUND
        rjmp MAIN

START_NEWROUND:
        ; All players have checked in.
        ; Start new round

        ldi     rec, 100
        call    Do_Wait
        mov     mpr, best_score
        cpi     mpr, 0
        ; Make sure everyone didn't bust.
        breq    EVERYONE_BUSTS
        rjmp    SEND_WINNER
EVERYONE_BUSTS:
        clr     best_botId ; If everyone busted, set best_botId to zero
SEND_WINNER:

        mov     mpr, best_botId
        inc     mpr
        call    USART_Transmit ; Send winning botId
        ldi     rec, 200
        call    Do_Wait

        ldi     mpr, 1
        mov     players_active, mpr

        clr     best_score
        clr     game_state
        ldi     mpr, NewRound
        call    USART_Transmit ; Send new round command
rjmp MAIN

.include "LCDDriver.asm"		; Include the LCD Driver
;----------------------------------------------------------------
; Sub: SetBestScore
; Desc: Store mpr into address at BestScore
;----------------------------------------------------------------
SetBestScore:
        push XL
        push XH
		ldi		XL, low(BestScore) ; Get Score
		ldi		XH, high(BestScore)
        st      X, mpr
        pop XH
        pop XL
        ret

;----------------------------------------------------------------
; Sub: USART_Receive
; Desc: Receive data over IR Calculate Bot with best score
;   Make sure it's not one of our commands
;   if game_state == 0:
;       receive BotId
;       tmp_botid = BotId
;   if game_state == 1:
;       if rec > BestHand:
;           mov  tmp_botid, best_botId
;           mov  rec, best_score
;           clr  tmp_botid
;           ldi  game_state, 0
;----------------------------------------------------------------
USART_Receive:
    ; Wait for data to be received
    lds mpr, UCSR1A
    sbrs mpr, RXC1
    rjmp USART_Receive

    ; Get and return receive data from receive buffer
    lds rec, UDR1 ; rec has what was received

    cpi     rec, NewRound
    breq    DONE_Rec
    mov     r23, best_botId
    inc     r23
    cp      rec, r23
    breq    DONE_Rec

    mov     mpr, game_state ; rec is tmp_botid
    cpi     mpr, 0
    breq    REC_BOTID  ; jump to State 0
    rjmp    REC_SCORE  ; Goto to State 1
REC_BOTID: ; State is 0
    ldi     mpr, 16
    out     PORTB, mpr
    mov     tmp_botid, rec ; tmp_botid not has BotId
    inc     game_state ; set state to 1
    rjmp    DONE_Rec

REC_SCORE: ; State is 1
    ldi     mpr, 1
    out     PORTB, mpr
    mov     mpr, best_score
    sub     mpr, rec        ; 0 =< |rec (recieved) - mpr (best_score) | =< 71
            ; If mpr > rec Then After sub mpr > 0   recieved is the better score
            ; If mpr < rec Then After sub mpr < 0   best_score is still best score
    cpi     mpr, 0
    brge    STILL_BEST      ; TODO, is this right?

    ; Update best score
    mov     best_botId, tmp_botid
    mov     best_score, rec
    clr     tmp_botid
    clr     game_state
    dec     players_active
    rjmp    DONE_Rec

STILL_BEST:
    clr     game_state
    dec     players_active

DONE_Rec:
    ret
;----------------------------------------------------------------
; Sub: USART_Transmit
; Desc: Send data over IR
;       Sends value found in mpr and then returns
;----------------------------------------------------------------
USART_Transmit:
    lds rec, UCSR1A
    sbrs rec, UDRE1
    ; Load status of USART1
    ; Loop until transmit data buffer is ready
    rjmp USART_Transmit
    ; Send data
    sts UDR1, mpr
    ret

;----------------------------------------------------------------
; Sub:	Wait
; Desc:	A wait loop that is 16 + 159975*waitcount cycles or roughly
;		waitcount*10ms.  Just initialize wait for the specific amount
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * line + 3) * type + 3) * waitcount + 13 + call
;----------------------------------------------------------------
Do_Wait:
		push	rec			; Save wait register
		push	line			; Save line register
		push	type			; Save type register

Loop:	ldi		type, 224		; load type register
OLoop:	ldi		line, 237		; load line register
ILoop:	dec		line			; decrement line
		brne	ILoop			; Continue Inner Loop
		dec		type		    ; decrement type
		brne	OLoop			; Continue Outer Loop
		dec		rec		; Decrement wait
		brne	Loop			; Continue Wait loop

		pop		type		    ; Restore type register
		pop		line		    ; Restore line register
		pop		rec		; Restore wait register
		ret				; Return from subroutine
