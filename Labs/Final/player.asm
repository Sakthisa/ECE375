.include "m128def.inc"			; Include the ATMega128 Definition Doc
.def	mpr = r16				; Multi-purpose register defined for LCDDV2
.def	ReadCnt = r23			; Counter used to read data from Program Memory
.equ	CountAddr = $0130		; Address of ASCII counter text
.def	counter = r4			; Counter used for Bin2ASCII
.def	waitcount = r24			; Counter used for Bin2ASCII
.equ	Round = $0200		    ; Address Of Round Count
.equ	Score = $0202		    ; Address of Score Count


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
MAIN:
        ldi     waitcount, 20
        call    Do_Wait
        call    Playing_LN1
		ldi		XL, low(Round) ; Get Round
		ldi		XH, high(Round)
        ld      mpr, X
        inc     mpr
        st      X, mpr
		ldi		XL, low(Score) ; Get Score
		ldi		XH, high(Score)
        ld      mpr, X
        inc     mpr
        st      X, mpr
    rjmp MAIN


.include "LCDDriver.asm"		; Include the LCD Driver

;***********************************************************
;*	 LCD Code
;***********************************************************
Playing_LN1:
        ; +-------------------------------------------+
        ; | Round  X                         (Score)  |
        ; +-------------------------------------------+
        ; ^ Write that to line 1 of LCD.
        ; X -- Round number stored in `round` register
        ; Score -- Players Score stored in `score` register
        ;
		; Write initial "Round:% Score:%" string to LCD line 1
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
