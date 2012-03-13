.include "m128def.inc"			; Include the ATMega128 Definition Doc
.def	mpr = r16				; Multi-purpose register defined for LCDDV2
.def	ReadCnt = r23			; Counter used to read data from Program Memory
.equ	CountAddr = $0130		; Address of ASCII counter text
.def	counter = r4			; Counter used for Bin2ASCII
.def	waitcount = r24			; Counter used for Bin2ASCII
.equ	Round = $0200		    ; Address Of Round Count
.equ	Score = $0202		    ; Address of Score Count
.equ	Hand  = $0206		    ; Address of Hand Count

; Controls
.equ	BHit = 0b11111110				; Right Whisker Input Bit
.equ	BStay = 0b11111101


.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

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

        clr     mpr
        call    SetHand
MAIN:
        ldi     waitcount, 75
        call    Do_Wait
        call    Playing_LN1
        call    IncRound
        call    IncScore

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
    inc     mpr
    call    SetHand
    call    Playing_LN2
    ret
;----------------------------------------------------------------
; Sub: DoStay
; Desc: The player wants to hit.
;----------------------------------------------------------------
DoStay:
    call PrintStay
NOTHING_YET: rjmp NOTHING_YET
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
NEW_ROUND:
.DB "   NEW ROUND!   " ;
SAY_STAY:
.DB " WAIT 4 RESULTS " ;
SHOW_HAND:
.DB "Hit/Stay HAND:%%" ; Values at 14 and 15

