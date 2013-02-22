;	move red face program atari 2600 Filip Pierscinski
;
;	tel DASM that this is 6502 program

	processor 6502

;	include vcs.h to have in program all 
;	special atari memory locations


	include vcs.h

;	tell DASM where all code of atari 
;	program gose 


	org $F000


;	here we can set variables, atari have 128 byte of RAM
;	in address $80 to $FF
FaceYStartPopsition 		= $81	; variable is stored on address $81
FaceHeightLinecsCunter 	= $82
BackgroundColorWhileCollision 	= $83

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

	lda #$0E
	sta BackgroundColorWhileCollision

;	now we set color for player 0 and for his missile
	lda #$42
	sta COLUP0		; set color for player 0 to $42

	lda #$0C
	sta COLUP1		; set color for player 1 to $0C

;	set width of line
	
	lda #$20 	; look to stell programing guide http://pl.scribd.com/doc/4740283/STELLA-Programmers-Guide
	sta NUSIZ1	; NUSIZ0 sets the size and multiplying

;	HMM1 is horizontal movment register it use two part 
;	with two's complement notation ($X0 left $0X right nibble)
	
	lda #$F0 	; -1 move left
	sta HMM1

;	trurn on missile 1
	lda #2 			
	sta ENAM1

;	set default position of DOT
	lda #80
	sta FaceYStartPopsition

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

	ldx #$00	; slow move right

;	check is P0 joy is in left position

	lda #%10000000
	bit SWCHA 			; SWCHA controll reg for joys
						; hi 4 bits for player 0
						; low 4 bits for player 1
	bne SkipMoveLeft	; if not qual 0 it menas that on 3 bit
						; of controll reg P0 joy is 1 so it is not left
	
	ldx #$F0			; -1 -> slow move right
	lda #%00001000		; D3 on REFP0 will mirror sprite
	sta REFP0

SkipMoveLeft

;	check is P0 joy is in right position

	lda #%01000000
	bit SWCHA 
	bne SkipMoveRight
	
	ldx #$10			; 1 -> slow move left
	lda #%00000000		; D3 on REFP0 will mirror sprite
	sta REFP0
SkipMoveRight

;	HMP0 is horizontal movment register it use two part 
;	with two's complement notation ($X0 left $0X right nibble)
	stx HMP0

;	load to Y reg number of scanlines to draw

	ldy #192	; TIA H resolution is 192 

;	set position of DOT

;	check is P0 joy is in down position
	lda #%00010000
	bit SWCHA 			
	bne SkipMoveDown	
	lda FaceYStartPopsition
	cmp #191
	beq SkipMoveDown		; skip if DOTYStartPopsition == 192
	inc FaceYStartPopsition

SkipMoveDown 

;	check is P0 joy is in down position
	lda #%00100000
	bit SWCHA 		
	bne SkipMoveUp
	lda FaceYStartPopsition
	cmp #7
	beq SkipMoveUp		; skip if FaceYStartPopsition == face_height
	dec FaceYStartPopsition
SkipMoveUp 

;	after set HMM0 we must wait minimum 24 cycles
;	to can set HMOVE

WaitForHMM0Set
	lda INTIM			; load TIM64T counter
	cmp #42
	bpl WaitForHMM0Set

	sta HMOVE	; turn on horizontal motion

;	check is collision between M1 an P1 
	lda #%11000000 
	bit CXM1P
	beq NoCollision
	
;	if it is set background color to BackgroundColorWhileCollision
	lda BackgroundColorWhileCollision
	sta COLUBK		

	jmp SkipCollision

NoCollision

;	set background color to black (black color have val $00)
	lda #$00
	sta COLUBK		; store A value to background color
					; register (COLUBK is addr from vcs.h)

SkipCollision
	sta CXCLR
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


	lda FaceHeightLinecsCunter
	beq	SkipDrawFace			; if  DOTHeightLinecsCunter == 0
								; not set missile draw for this scanline
	ldx FaceHeightLinecsCunter
	
	lda BigHeadGraphic-1,x  			; to turn on visible for player 0 in this
										; scanline we must set byte of data whic handle pixel info for this line
	sta GRP0

	dec FaceHeightLinecsCunter	; decrement couter
	bpl FinishScanLine

SkipDrawFace
	lda #0
	sta GRP0	 	; turn off visible for player 0 

	cpy FaceYStartPopsition	; compare y reg with FaceYStartPopsition 
							; value
	bne FinishScanLine	; if not equal, not set dot drow counter
	
	lda #8
	sta FaceHeightLinecsCunter ; set height couter for dot to 8 lines

FinishScanLine

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

; here's the actual graphic! 
BigHeadGraphic
	.byte #%00111100
	.byte #%01111110
	.byte #%11000001
	.byte #%10111111
	.byte #%11111111
	.byte #%11101011
	.byte #%01111110
	.byte #%00111100

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