.include "m128def.inc"			; Include the ATMega128 Definition Doc
.def	mpr = r16				; Multi-purpose register defined for LCDDV2
.def	game_state = r5			; Game State
                                ; State 0 = Player can hit or stay.
                                ; State 1 = Player Has hit waiting for results.
                                ; State 2 = Player Has recieved either Win or
                                ;           Loss GOTO state 0
.def    rec = r6
.def    tmp = r7
.def	ReadCnt = r23			; Counter used to read data from Program Memory
.equ	CountAddr = $0130		; Address of ASCII counter text
.def	counter = r4			; Counter used for Bin2ASCII
.def	waitcount = r24			; Counter used for Bin2ASCII
.equ	Round = $0200		    ; Address Of Round Count
.equ	Score = $0202		    ; Address of Score Count
.equ	Hand  = $0206		    ; Address of Hand Count


.equ    BustVal = 71
; Controls
.equ	BHit     = 0b11111110				; Right Whisker Input Bit
.equ	BStay    = 0b11111101

.equ    NewGame  = 0b10000111
.equ    NewRound = 0b10011111

;BotId
.equ    BotID = 0b11101010
.equ    WinID = 0b11101011

; Random Number Stuff
.equ    Seed  = $0208               ; Address of Random Seed
.equ    rndmul = 0b01010111
.equ    rndinc = 0b11010101



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

        ldi		XL, low(Round) ; Get Round
        ldi		XH, high(Round)
        ldi     mpr, 0 ; Initialize round
        st      X, mpr

        ldi		XL, low(Score) ; Get Score
        ldi		XH, high(Score)
        ldi     mpr, 0 ; Initialize score
        st      X, mpr
        ; Generate random seed.
        ldi     mpr, 9
        sts     Seed, mpr
        lds     R0, Seed
        clr     mpr

        ; Set state to 0
        ldi     mpr, 0
        mov     game_state, mpr

        clr     mpr
        call    SetHand

MAIN:
;        ldi     waitcount, 75
;        call    Do_Wait
;        call    Playing_LN1
;        call    IncRound
;        call    IncScore

;        call    SetHand
;        inc     mpr
;        call    Playing_LN2
;
;        ldi     waitcount, 75
;        call    Do_Wait
;        call    PrintStay
;
;        ldi     waitcount, 75
;        call    Do_Wait
;        call    PrintWinRound
;
;        ldi     waitcount, 75
;        call    Do_Wait
;        call    PrintLooseRound
;
;        ldi     waitcount, 75
;        call    Do_Wait
;        call    PrintNewRound

PLAY_GAME:
        call    Playing_LN1
        ldi     waitcount, 10   ; Refresh the LED
        call    Do_Wait

        in      mpr, PIND       ; Get PIND
        cpi     mpr, BHit
        breq    PLAYER_HIT        ; Did the player hit?
        rjmp    CHECK_STAY
PLAYER_HIT:
        call    DoHit
        ldi     waitcount, 20 ; Give some delay so we don't over do it on the buttons
        call    Do_Wait

CHECK_STAY:
        in      mpr, PIND       ; Get whisker input from Port D
        cpi     mpr, BStay
        breq    PLAYER_STAY        ; Right
        rjmp    PLAYER_DONE
PLAYER_STAY:
        call    DoStay
PLAYER_DONE:

rjmp PLAY_GAME


.include "LCDDriver.asm"		; Include the LCD Driver

;***********************************************************
;*	 LCD Code
;***********************************************************
;----------------------------------------------------------------
; Sub: NewRound:
; Desc: Print winning message
;----------------------------------------------------------------
PrintNewRound:
        push mpr
        push line
        push count
		ldi		ZL, low(NEW_ROUND<<1); Init variable registers
		ldi		ZH, high(NEW_ROUND<<1)
		ldi		YL, low(LCDLn2Addr)
		ldi		YH, high(LCDLn2Addr)
		ldi		ReadCnt, LCDMaxCnt
        rjmp    INIT_LINE2meta
;----------------------------------------------------------------
; Sub: LooseRound:
; Desc: Print loosing message
;----------------------------------------------------------------
PrintLooseRound:
        push mpr
        push line
        push count
		ldi		ZL, low(LOSE<<1); Init variable registers
		ldi		ZH, high(LOSE<<1)
		ldi		YL, low(LCDLn2Addr)
		ldi		YH, high(LCDLn2Addr)
		ldi		ReadCnt, LCDMaxCnt
        rjmp    INIT_LINE2meta
;----------------------------------------------------------------
; Sub: WinRound:
; Desc: Print winning message
;----------------------------------------------------------------
PrintWinRound:
        push mpr
        push line
        push count
		ldi		ZL, low(WIN<<1); Init variable registers
		ldi		ZH, high(WIN<<1)
		ldi		YL, low(LCDLn2Addr)
		ldi		YH, high(LCDLn2Addr)
		ldi		ReadCnt, LCDMaxCnt
        rjmp    INIT_LINE2meta
;----------------------------------------------------------------
; Sub: Stay:
; Desc: Print winning message
;----------------------------------------------------------------
PrintStay:
        push mpr
        push line
        push count
		ldi		ZL, low(SAY_STAY<<1); Init variable registers
		ldi		ZH, high(SAY_STAY<<1)
		ldi		YL, low(LCDLn2Addr)
		ldi		YH, high(LCDLn2Addr)
		ldi		ReadCnt, LCDMaxCnt
        rjmp    INIT_LINE2meta

;----------------------------------------------------------------
; Sub: PrintBust
; Desc: Print winning message
;----------------------------------------------------------------
PrintBust:
        push mpr
        push line
        push count
		ldi		ZL, low(BUST_LINE<<1); Init variable registers
		ldi		ZH, high(BUST_LINE<<1)
		ldi		YL, low(LCDLn2Addr)
		ldi		YH, high(LCDLn2Addr)
		ldi		ReadCnt, LCDMaxCnt
        rjmp    INIT_LINE2meta

INIT_LINE2meta:
        lpm		mpr, Z+			; Read Program memory
        st		Y+, mpr			; Store into memory
        dec		ReadCnt			; Decrement Read Counter
        brne	INIT_LINE2meta	; Continue untill all data is read
        rcall	LCDWrLn2		; WRITE LINE 1 DATA
        pop count
        pop line
        pop mpr
        ret

;----------------------------------------------------------------
; Sub: Playing_LN2:
; +-------------------------------------------+
; | Hit/Stay                        HAND: XX  |
; +-------------------------------------------+
; ^ Write that to line 2 of LCD.
; X -- Hand count stored in `Hand`
;
;----------------------------------------------------------------
playing_ln2:
        push mpr
        push line
        push count

		ldi		ZL, low(SHOW_HAND<<1); Init variable registers
		ldi		ZH, high(SHOW_HAND<<1)
		ldi		YL, low(LCDLn2Addr)
		ldi		YH, high(LCDLn2Addr)
		ldi		ReadCnt, LCDMaxCnt
INIT_LINE2hand:
        lpm		mpr, Z+			; Read Program memory
        st		Y+, mpr			; Store into memory
        dec		ReadCnt			; Decrement Read Counter
        brne	INIT_LINE2hand	; Continue untill all data is read
        rcall	LCDWrLn2		; WRITE LINE 1 DATA

        ; Write current hand
        ; Write Round
        call Clear_data_area
		ldi		XL, low(Hand) ; Get Round
		ldi		XH, high(Hand)
        ld      mpr, X
        ; Convert mpr to ASCII
		ldi		XL, low(CountAddr)
		ldi		XH, high(CountAddr)
		rcall	Bin2ASCII		; CALL BIN2ASCII TO CONVERT DATA
								; NOTE, COUNT REG HOLDS HOW MANY CHARS WRITTEN
		ldi		ReadCnt, 2		; always write three chars to overide existing data in LCD
        ldi     count, 14
        ldi     line, 2
        ; Write hand
T0_L3:	ld		mpr, X+			; LOAD MPR WITH DATA TO WRITE
		rcall	LCDWriteByte	; CALL LCDWRITEBYTE TO WRITE DATA TO LCD DISPLAY
		inc		count			; INCREMENT COUNT TO WRITE TO NEXT LCD INDEX
		dec		ReadCnt			; decrement read counter
		brne	T0_L3			; Countinue untill all data is written

        pop count
        pop line
        pop mpr
        ret

;----------------------------------------------------------------
; Sub: Playing_LN1:
; +-------------------------------------------+
; | Round  X                         (Score)  |
; +-------------------------------------------+
; ^ Write that to line 1 of LCD.
; X -- Round number stored in `round` register
; Score -- Players Score stored in `score` register
;
; Write initial "Round:% Score:%" string to LCD line 1
;----------------------------------------------------------------
Playing_LN1:
        push mpr
        push line
        push count

		ldi		ZL, low(TXT0<<1); Init variable registers
		ldi		ZH, high(TXT0<<1)
		ldi		YL, low(LCDLn1Addr)
		ldi		YH, high(LCDLn1Addr)
		ldi		ReadCnt, LCDMaxCnt
INIT_LINE1:
        lpm		mpr, Z+			; Read Program memory
        st		Y+, mpr			; Store into memory
        dec		ReadCnt			; Decrement Read Counter
        brne	INIT_LINE1		; Continue untill all data is read
        rcall	LCDWrLn1		; WRITE LINE 1 DATA
        ; Write Round and Score
WRITE_ROUND:
        ; Write Round
        call Clear_data_area
		ldi		XL, low(Round) ; Get Round
		ldi		XH, high(Round)
        ld      mpr, X
        ; Convert mpr to ASCII
		ldi		XL, low(CountAddr)
		ldi		XH, high(CountAddr)
		rcall	Bin2ASCII		; CALL BIN2ASCII TO CONVERT DATA
								; NOTE, COUNT REG HOLDS HOW MANY CHARS WRITTEN
		ldi		ReadCnt, 2		; always write three chars to overide existing data in LCD
        ldi     count, 6
        ldi     line, 1
        ; Write Score to LCD
T0_L1:	ld		mpr, X+			; LOAD MPR WITH DATA TO WRITE
		rcall	LCDWriteByte	; CALL LCDWRITEBYTE TO WRITE DATA TO LCD DISPLAY
		inc		count			; INCREMENT COUNT TO WRITE TO NEXT LCD INDEX
		dec		ReadCnt			; decrement read counter
		brne	T0_L1			; Countinue untill all data is written

WRITE_SCORE:
        ; Write Score
        call Clear_data_area
		ldi		XL, low(Score) ; Get Score
		ldi		XH, high(Score)
        ld      mpr, X
        ; Convert mpr to ASCII
		ldi		XL, low(CountAddr)
		ldi		XH, high(CountAddr)
		rcall	Bin2ASCII		; CALL BIN2ASCII TO CONVERT DATA
								; NOTE, COUNT REG HOLDS HOW MANY CHARS WRITTEN
		ldi		ReadCnt, 2		; always write three chars to overide existing data in LCD
        ldi     count, 14
        ldi     line, 1

        ; Write Score to LCD
T0_L2:	ld		mpr, X+			; LOAD MPR WITH DATA TO WRITE
		rcall	LCDWriteByte	; CALL LCDWRITEBYTE TO WRITE DATA TO LCD DISPLAY
		inc		count			; INCREMENT COUNT TO WRITE TO NEXT LCD INDEX
		dec		ReadCnt			; decrement read counter
		brne	T0_L2			; Countinue untill all data is written

        pop count
        pop line
        pop mpr
        ret

;----------------------------------------------------------------
; Sub: Clear_data_area
; Desc: ZERO out 2 words starting at CountAddr
;----------------------------------------------------------------

Clear_data_area:
		ldi		XL, low(CountAddr)
		ldi		XH, high(CountAddr)
		ldi		count, 2		; Init X-ptr and count
		ldi		mpr, ' '		; Load mpr with space char
LABEL1:	st		X+, mpr			; Clear data area
		dec		count			; Decrement count
		brne	LABEL1			; Continue until all data is cleared
        ret

;***********************************************************
;*	Functions and Subroutines
;***********************************************************
;----------------------------------------------------------------
; Sub: DoHit
; Desc: The player wants to hit.
;----------------------------------------------------------------
DoHit:
    clr     mpr
    call    GetHand
    mov     tmp, mpr ;tmp = hand
    call    GetRand  ; mpr = Random number
    add     mpr, tmp ; mpr = Hand + Random number
    call    SetHand  ; Set hand Value
    subi    mpr, BustVal ; Hand = Hand - 70
    cpi     mpr, 0
    brge    BUST    ; mpr > 0 ? Busted: Return
    ; They didn't go over
    call    Playing_LN2
    ret
BUST:
    ; They busted
    call PrintBust
    ldi     waitcount, 200
    call    Do_Wait

    rjmp DoStay


;----------------------------------------------------------------
; Sub: DoStay
; Desc: The player wants to hit.
;----------------------------------------------------------------
DoStay:
    call    PrintStay


    ldi     mpr, 0
    mov     game_state, mpr ; Go to state 0, waiting for reply
    ldi     mpr, BotId
    call    USART_Transmit  ; Send BotId
    call    GetHand         ; Stores hand into mpr
    call    USART_Transmit  ; Send Hand

    ; Let the light fade
    ldi     waitcount, 10
    call    Do_Wait
    ldi     mpr, 1
    mov     game_state, mpr ; Go to state 1, waiting for reply
STATE1:
    mov     mpr, game_state
    cpi     mpr, 1
    breq    STATE1          ; If we are in state 2 Go play game
    ldi     mpr, 0
    mov     game_state, mpr

    ret

;----------------------------------------------------------------
; Sub: IncScore
; Desc: Incriment the score
;----------------------------------------------------------------
IncScore:
        push XL
        push XH
        push mpr
		ldi		XL, low(Score) ; Get Score
		ldi		XH, high(Score)
        ld      mpr, X
        inc     mpr
        st      X, mpr
        pop mpr
        pop XH
        pop XL
        ret
;----------------------------------------------------------------
; Sub: IncScore
; Desc: Incriment the score
;----------------------------------------------------------------
IncRound:
        push XL
        push XH
        push mpr
		ldi		XL, low(Round) ; Get Round
		ldi		XH, high(Round)
        ld      mpr, X
        inc     mpr
        st      X, mpr
        pop mpr
        pop XH
        pop XL
        ret

;----------------------------------------------------------------
; Sub: SetHand
; Desc: Set memory at hand to value of mpr
;----------------------------------------------------------------
SetHand:
        push XL
        push XH
		ldi		XL, low(Hand) ; Get Round
		ldi		XH, high(Hand)
        st      X, mpr
        pop XH
        pop XL
        ret
;----------------------------------------------------------------
; Sub: GetHand
; Desc: Set memory at hand to value of mpr
; Stores into mpr, clobers mpr
;----------------------------------------------------------------
GetHand:
        push XL
        push XH
		ldi		XL, low(Hand) ; Get Round
		ldi		XH, high(Hand)
        ld      mpr, X
        pop XH
        pop XL
        ret
;----------------------------------------------------------------
; Sub: USART_Receive
; Desc: Receive data over IR
;       This is where checking to see if you won happens.
;       if state != 1:
;           ret
;       if rec == NewGame
;           call IncRound
;           // Other stuff?
;       if rec == NewRound
;           call PrintLoose
;           call IncRound
;           Reset hand
;           state = 2
;       if rec == WinID
;           call PrintWin
;           call IncRound
;           call IncScore
;           Reset hand
;           state = 2
;----------------------------------------------------------------
USART_Receive:
    ; Wait for data to be received
    lds mpr, UCSR1A
    sbrs mpr, RXC1
    rjmp USART_Receive

    ; Get and return receive data from receive buffer
    lds mpr, UDR1
    ; mpr === rec

    ldi     waitcount, 1
    cp      waitcount, game_state
    brne    DONE_REC


    cpi     mpr, NewGame     ;   if rec == NewGame
    breq    NEW_GAME

    cpi     mpr, WinId       ;   if rec == WinId
    breq    WIN_ROUND

    cpi     mpr, NewRound    ;   if rec == NewRound
    breq    NEXT_ROUND

    rjmp DONE_REC            ; ???

NEXT_ROUND:
    call    PrintLooseRound  ; We lost
    call    IncRound         ; Round = Round + 1
    clr     mpr
    call    SetHand          ; Reset Hand

    ldi     mpr, 0
    mov     game_state, mpr  ; Set game state to 0

    call    PrintNewRound
    ldi     waitcount, 150
    call    Do_Wait
    rjmp    DONE_REC
WIN_ROUND:
    call    PrintWinRound    ; We Won
    call    IncRound         ; Round = Round + 1
    clr     mpr
    call    SetHand          ; Reset Hand

    ldi     mpr, 0
    mov     game_state, mpr  ; Set game state to 0

    call    PrintNewRound
    ldi     waitcount, 150
    call    Do_Wait
    rjmp    DONE_REC

NEW_GAME:
    rjmp    DONE_REC

DONE_REC:
    ret

;----------------------------------------------------------------
; Sub: USART_Transmit
; Desc: Send data over IR
;       Sends value found in mpr and then returns
;----------------------------------------------------------------

USART_Transmit:
    push waitcount ; Use waitcount as garbage variable.
    lds waitcount, UCSR1A
    sbrs waitcount, UDRE1
    ; Load status of USART1
    ; Loop until transmit data buffer is ready
    rjmp USART_Transmit
    ; Send data
    sts UDR1, mpr
    pop waitcount
    ret
;----------------------------------------------------------------
; Sub:	GetRand
; Desc:	Generage a random number and store it in mpr
;----------------------------------------------------------------
GetRand:
        ldi     mpr, rndmul
        lds     R0, Seed
        mul     R0, mpr
        ldi     mpr, rndinc
        add     R0, mpr
        sts     Seed, R0
        ldi     mpr, 0b00011101
        and     R0, mpr
        inc     R0      ;Should now have a random number from 1-30 in R0
        mov     mpr, R0
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
		push	waitcount			; Save wait register
		push	line			; Save line register
		push	type			; Save type register

Loop:	ldi		type, 224		; load type register
OLoop:	ldi		line, 237		; load line register
ILoop:	dec		line			; decrement line
		brne	ILoop			; Continue Inner Loop
		dec		type		    ; decrement type
		brne	OLoop			; Continue Outer Loop
		dec		waitcount		; Decrement wait
		brne	Loop			; Continue Wait loop

		pop		type		    ; Restore type register
		pop		line		    ; Restore line register
		pop		waitcount		; Restore wait register
		ret				; Return from subroutine
TXT0:
.DB "Round:% Score:% " ; Values are at 6 and 14 (starting at zero offset)
WIN:
.DB "  U WIN ROUND!  " ;
LOSE:
.DB " U NO WIN ROUND " ;
BUST_LINE:
.DB "   U WENT OVA   " ;
NEW_ROUND:
.DB "   NEW ROUND!   " ;
SAY_STAY:
.DB " WAIT 4 RESULTS " ;
SHOW_HAND:
.DB "Hit/Stay HAND:%%" ; Values at 14 and 15

