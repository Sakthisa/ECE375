;***********************************************************
;*
;*	Enter Name of file here
;*
;*	Enter the description of the program here
;*
;*	This is the skeleton file Lab 3 of ECE 375
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
.def	mpr = r16				; Multipurpose register required for LCD Driver

.def	ReadCnt = r23			; Counter used to read data from Program Memory

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

.org	$0046					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:							; The initialization routine
		; Init the 2 stack pointer registers
		ldi		mpr, HIGH(RAMEND)
		out		SPH, mpr
		ldi		mpr, LOW(RAMEND)
		out		SPL, mpr

		rcall	LCDInit			; INITIALIZE THE LCD DISPLAY
								; An RCALL statement


		; Move strings from Program Memory to Data Memory
								; A while loop will go here

		; NOTE that there is no RET or RJMP from INIT, this is
		; because the next instruction executed is the first for
		; the main program

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program
		; Write initial "Counter: " string to LCD line 1
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


		ldi		ZL, low(TXT1<<1); Load Z pointer with address
		ldi		ZH,high(TXT1<<1); of Text message 1
		ldi		ReadCnt, LCDMaxCnt
		rcall	WriteText		; Write the Text Message
		rjmp	MAIN			; jump back to main and create an infinite

;***********************************************************
;*	Functions and Subroutines
;***********************************************************
WriteText:
		push	mpr				; Save the mpr register
		push	ReadCnt			; Save the ReadCounter
		rcall	LCDClrLn2		; CLEAR LINE 2 OF LCD
								; LOAD THE LCD MAX LINE COUNT (16)
		ldi		ReadCnt, LCDMaxCnt
								; LOAD THE Y POINTER WITH THE DATA
								; ADDRESS FOR LINE 2 DATA
		ldi		YL, low(LCDLn2Addr)
		ldi		YH, high(LCDLn2Addr)
WriteText_lp:					; Loop that reads the data
		lpm		mpr, Z+			; Read program data
		st		Y+, mpr			; Store data to memory
		dec		ReadCnt			; Decrement counter
		brne	WriteText_lp	; Loop untill all data is read
		rcall	LCDWrLn2		; WRITE DATA TO LINE 2
		pop		ReadCnt			; Restore the ReadCounter
		pop		mpr				; Restore the mpr register
		ret						; Return from function
;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variable by pushing them to the stack

		; Execute the function here
		
		; Restore variable by popping them from the stack in reverse order\
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;***********************************************************

;----------------------------------------------------------
; An example of storing a string, note the preceeding and
; appending labels, these help to access the data
;----------------------------------------------------------
STRING_BEG:
.DB		"My Test String"		; Storing the string in Program Memory
STRING_END:


;***********************************************************
;*	Data Definitions
;***********************************************************
TXT0:
.DB "Oh Hai!         "
TXT1:
.DB "Herp Derp!      "
