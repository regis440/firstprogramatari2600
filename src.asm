;red line program atari 2600 Filip Pierscinski
;
;	tel DASM that this is 6502 program

	processor 6502

;	include vcs.h to have in program all 
;	special atari memory locations

	include vcs.h

;	tell DASM where all code of atari 
;	program gose 

	org $F000

;	begin of program with labe "Strat"
;	adress of "Strat" is $F000

Start

;	set inital data for interupt, stack ptr,
;	staus flags

	sei 		; disable interrupts
	cld 		; disable decimal mode

	ldx #$FF 	; load $FF to register X

	txs			; transfer value from X reg 
				; to stack pointer reg
				; (reset stack pointer)

;	clear memory form $00 to $FF
;	atari ram is from $80 to $FF
;	from $00 to $7F are maped TIA regs

	lda #00 		; put $0 to A register, X isset to $FF
ClearMem
	sta 0,X 		; store value of A in page 0 + offset X
	dex				; decrement X reg
	bne ClearMem	; if not Z status flag set to 1 branch
					; to ClearMem

;	set background color to black (black color have val $00)
	lda #$12
	sta COLUBK		; store A value to background color
					; register (COLUBK is addr from vcs.h)

;	now we set color for player 0 and for his missile

	lda #$42
	sta COLUP0		; set color for player 0 to $42

;	starts main loop
;	1) VSYNC
;	2) vertical blank
;	3) horizontal blnk | screen draw
;	4) overscan

Main

;	set bit with index 1 of VSYNC control byte to 1
;	this enable VSYNC (control byte have addr $1D) 

	lda #2 		; binary 00000010
	sta VSYNC	

	sta WSYNC	; cpu wait to first scanline finish
	sta WSYNC	; second scanline
	sta WSYNC	; thrid scanline

;	waint for end of vertical blank (if we have logic 
;	we can put under sta TIM64T some code). One scanline take 
;	76 cpu cycles and vertical blank tak 37 scanlines
;	76*37 = 2812 cpu cycles. We must subtract 5 cycles
;	for set timer and 3 cycles to tak sta WSYNC to
;	take next line plus 6 cycles for checking loop
;	2812 - 5 - 3 - 6 = 2798 

	lda #43		; floor( 2798/64 ) = 43
	sta TIM64T	; couter is decrement every 64 cycles

;	from now to WaitForVblank we hane 2798 cycles to use
;	===================BEGIN LOGIC===================

;	turn of VSYNC 

	lda #0
	sta VSYNC

;	HMM0 is horizontal movment register it use two part 
;	with two's complement notation ($X0 left $0X right nibble)

	lda #$F0	; $F is -1 in two's complement notation
	sta HMM0

	sta HMOVE	; turn on horizontal motion

;	load to Y reg number of scanlines to draw

	ldy #192	; TIA H resolution is 192 

;	====================END LOGIC====================
;	wait for Vblank end if we have spere cycles

WaitForVblank
	lda INTIM			; load TIM64T counter
	bne WaitForVblank	; if TIM64T == 0 couter 
						; branch to WaitForVblank

	sta WSYNC	; atari have somethig like 44 cycles to
				; end scanline so we wait for finish

	sta VBLANK  ; set end of vblank

;	scanline loop
;	horizonatl blank have 68 cycles but we must subtract
;	sta WSYNC instruction (3 cycles), dey instruction(2 cycles) 
;	and bne instruction (4 cycles), so every H blank have
;	68 - 3 - 2 - 4 = 59 cycles except first scanline
;	68 - (sta VBLANK (3 cycles) + sta WSYNC (3 cycles)) = 62 
ScanLoop	
	lda #2 			; to turn on visible for missile 0 in this
					; scanline we must set 1 on index 1
					; of control byte of missile 0 
	sta ENAM0

	sta WSYNC

	dey				; decrement scan line counter
	bne	ScanLoop	; if couter not 0 repeat

; 	now finish last scan line with sta WSYNC and turn on
;	VBLANK

	lda #2
	sta WSYNC
	sta VBLANK

;	count down 30 scanlines of overscan we can put
;	here logic code 

;	beter then emply loop put here some code 
;	2280 - 3(jmp instruction) cycles
;	===================BEGIN LOGIC===================
	
	ldx #30
OverScanWait
	sta WSYNC
	dex
	bne OverScanWait
	
;	====================END LOGIC====================

	jmp Main

; OK, last little bit of crap to take care of.
; there are two special memory locations, $FFFC and $FFFE
; When the atari starts up, a "reset" is done (which has nothing to do with
; the reset switch on the console!) When this happens, the 6502 looks at
; memory location $FFFC (and by extension its neighbor $FFFD, since it's 
; seaching for both bytes of a full memory address)  and then goes to the 
; location pointed to by $FFFC/$FFFD...so our first .word Start tells DASM
; to put the binary data that we labeled "Start" at the location we established
; with org.  And then we do it again for $FFFE/$FFFF, which is for a special
; event called a BRK which you don't have to worry about now.
 
	org $FFFC
	.word Start
	.word Start