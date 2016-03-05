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

;	defines
NtscSystem  = 1
PalSystem   = 0
SecamSystem = 0

#if NtscSystem

VisableScanLinesCount = 192
P0Color = $42
P1Color	= $0C
BackgroundColor = $82
BackgroundColorWhileCollision = P1Color

#endif

#if PalSystem

VisableScanLinesCount = 242
P0Color = $62
P1Color	= $0E
BackgroundColor = #$D2
BackgroundColorWhileCollision = P1Color

#endif

#if SecamSystem

VisableScanLinesCount = 242
P0Color = $4
P1Color	= $E
BackgroundColor = $2
BackgroundColorWhileCollision = P1Color

#endif

PlayfiledSize = 1
Player0Height = 8
LastScanlineIndex = VisableScanLinesCount - 1

;	here we can set variables, atari have 128 byte of RAM
;	in address $80 to $FF
Player0YStartPopsition 		= $81	; variable is stored on address $81
;Player0HeightLinecsCunter 	= $82

;	draw buffers
Player0Buffer 		 		= $82
PlayfileldPatternBuffer0	= $83
PlayfileldPatternBuffer1	= $84
PlayfileldPatternBuffer2	= $85

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

;	now we set color for player 0 and for his missile
	lda #P0Color
	sta COLUP0		; set color for player 0 to $42

	lda #P1Color
	sta COLUP1		; set color for player 1 to $0C

;	now we set color for playfiels 0,1,2 and for his missile

	lda #P0Color
	sta COLUPF

;	now we set control flags for playfiels

	lda #%00000001 ; bit on index 0 -> reflect playfield
	sta CTRLPF

;;	set width of line
;	
;	lda #$20 	; look to stell programing guide http://pl.scribd.com/doc/4740283/STELLA-Programmers-Guide
;	sta NUSIZ1	; NUSIZ0 sets the size and multiplying
;
;;	HMM1 is horizontal movment register it use two part 
;;	with two's complement notation ($X0 left $0X right nibble)
;	
;	lda #$F0 	; -1 move left
;	sta HMM1
;
;;	trurn on missile 1
;	lda #2 			
;	sta ENAM1

;	set default position of player0
	lda #80
	sta Player0YStartPopsition

;	starts main loop
;	1) VSYNC
;	2) vertical blank
;	3) horizontal blank | screen draw
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
;	76 cpu cycles and vertical blank has 37 scanlines
;	76*37 = 2812 cpu cycles. We must subtract 5 cycles
;	for set timer and 3 cycles to call sta WSYNC to
;	take next line plus 6 cycles for checking loop
;	2812 - 5 - 3 - 6 = 2798 

	lda #43		; floor( 2798/64 ) = 43
	sta TIM64T	; couter is decrement every 64 cycles

;	from now to WaitForVblank we have 2798 cycles to use
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

;	set position of DOT

;	check is P0 joy is in up position
	lda #%00010000
	bit SWCHA 			
	bne SkipMoveDown	
	lda Player0YStartPopsition
	cmp #0
	beq SkipMoveDown		; skip if Player0YStartPopsition == VisableScanLinesCount
	dec Player0YStartPopsition

SkipMoveDown 

;	check is P0 joy is in down position
	lda #%00100000
	bit SWCHA 		
	bne SkipMoveUp
	lda Player0YStartPopsition
	cmp #LastScanlineIndex - Player0Height
	beq SkipMoveUp		; skip if Player0YStartPopsition == Player0Height
	inc Player0YStartPopsition
SkipMoveUp 

;	after set HMP0 we must wait minimum 24 cycles
;	to can set HMOVE

WaitForHMM0Set
	lda INTIM			; load TIM64T counter
	cmp #42
	bpl WaitForHMM0Set

	sta HMOVE	; turn on horizontal motion

;;	check is collision between M1 an P1 
;	lda #%11000000 
;	bit CXM1P
;	beq NoCollision
;	
;;	if it is set background color to BackgroundColorWhileCollision
;	lda #BackgroundColorWhileCollision
;	sta COLUBK		
;
;	jmp SkipCollision
;
;NoCollision

;	set background color to black (black color have val $00)
	lda #BackgroundColor
	sta COLUBK		; store A value to background color
					; register (COLUBK is addr from vcs.h)
;
;SkipCollision
;	sta CXCLR
 
;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	!!!!!!!!!FILL DRAW BUFFERS AND SET!!!!!!!!!!!!!
;	!!!!!!TIA REGISTERS FOR FIRST SCANLINE!!!!!!!!!
;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;	fill playfild buffers and set TIA playfild registers

	lda PlayfiledPF0 
	sta PlayfileldPatternBuffer0
	sta PF0

	lda PlayfiledPF1
	sta PlayfileldPatternBuffer1
	sta PF1
	lda PlayfiledPF2
	sta PlayfileldPatternBuffer2
	sta PF2

;	fill player0 buffer and set TIA player0 register
;	check is player0 on first scanline index
	ldy #0 							; load to Y reg index 0 of scanline to draw
	cpy Player0YStartPopsition		; compare y reg with Player0YStartPopsition 
	bne FirstScanlineSkipDrawPlayer0

;	if on first scanline set player0 scanline number counter
    ldx #Player0Height 				; (x height couter for player0) set index x register to height couter for player0

	lda BigHeadGraphic - 1,x    	; (x height couter for player0) load first data row for player0
	
	dex								; (x height couter for player0) decrement height couter for player0

	jmp FirstScanlineFillPlayer0Buffer

FirstScanlineSkipDrawPlayer0
    ldx #0 							; (x height couter for player0) reset to 0 height couter for player0

FirstScanlineFillPlayer0Buffer
	sta Player0Buffer
	sta GRP0

;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;	====================END LOGIC====================
;	wait for Vblank end if we have spere cycles

WaitForVblank
	lda INTIM			; load TIM64T counter
	bne WaitForVblank	; if TIM64T == 0 couter 
						; branch to WaitForVblank

	sta WSYNC	; atari have somethig like 44 cycles to
				; end scanline so we wait for finish

	sta VBLANK  ; set end of vblank
	
;	scanlines loop
FillBuffersForNextScanline	

;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	!!!BEGIN OF SCANLINE AFTER SET TIA REGISTER!!!!
;	!!!!!!!!53 cycle for scanline logic!!!!!!!!!!!!
;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	iny 	; next scanline index to draw 			; iny 2 		= 2

;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	!!!!!!FILL DRAW BUFFERS FOR NEXT SCANLINE!!!!!!
;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;	fill playfild buffers for next scanline 
	lda PlayfiledPF0,y 								; lda_abs 4-5 	= 7
	sta PlayfileldPatternBuffer0					; sta_zp 3 		= 10

	lda PlayfiledPF1,y 								; lda_abs 4-5 	= 15
	sta PlayfileldPatternBuffer1 					; sta_zp 3 		= 18
	lda PlayfiledPF1,y 								; lda_abs 4-5 	= 23
	sta PlayfileldPatternBuffer2 					; sta_zp 3 		= 26

;	player 0 draw buffer setup
	cpx #0 						 					; cpx_im 2 		= 28
;	(x height couter for player0) if height couter for player0 == 0
;	not set player0 draw for this scanline
	beq	SkipDrawPlayer0								
													; beq 2 		= 30

	lda BigHeadGraphic-1,x  						; lda_abs 4-5 	= 35	
; 	to turn on visible for player 0 in this
;	scanline we must set byte of data which handle pixel info for this line
	sta Player0Buffer 								; sta_zp 3 		= 38

;	(x height couter for player0) decrement height couter for player0
	dex 						 					; dex 2 		= 41
	jmp EndOfScanline 								; jmp 3 		= 44
													; lda_zp 3 		= 47
													; sta_zp 3 		= 50
													;---------------->HORIZONTAL BLANK
SkipDrawPlayer0
													; beq 3-4 		= 32
	lda #0 											; lda_im 2 		= 34
;	turn off visible for player 0
	sta Player0Buffer								; sta_zp 3		= 37 	 

	cpy Player0YStartPopsition						; cpy_zp 3 		= 40
;	compare y reg with Player0YStartPopsition 
;	value
													; bne 3-4 		= 44
													; lda_zp 3 		= 47
													; sta_zp 3 		= 50
													;---------------->HORIZONTAL BLANK
	bne EndOfScanline	
;	if not equal, not set player0 draw counter
													; bne 2 		= 42
; 	(x height couter for player0) set height couter for player0 to Player0Height lines
	ldx #Player0Height 								; ldx_im 2 		= 44

													; lda_zp 3 		= 47
													; sta_zp 3 		= 50
													;---------------->HORIZONTAL BLANK

EndOfScanline

	lda PlayfileldPatternBuffer0 ; to reduce instr count in vblank first playfield patern is stored directly in accumulator

;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	!!!!!!!!!!!!!!!END OF SCANLINE!!!!!!!!!!!!!!!!!
;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	sta WSYNC

;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	!!!!!!!!!!!!!!!BEGIN OF SCANLINE!!!!!!!!!!!!!!!
;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


;	!!!!!!!!!!!!!!HORIZONTAL BLANK!!!!!!!!!!!!!!!!!
;	!!!horizonatl blank have 22 cycles in which!!!!
;	!!we have to set all necessary TIA registers!!!
;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	!!!!SET TIA REGISTERS FOR CURRENT SCANLINE!!!!!
;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	sta PF0 						; sta_zp 3

	lda PlayfileldPatternBuffer1	; lda_zp 3 = 6
	sta PF1 						; sta_zp 3 = 9
	lda PlayfileldPatternBuffer2	; lda_zp 3 = 12
	sta PF2 						; sta_zp 3 = 15

	lda Player0Buffer 	 			; lda_zp 3 = 18
	sta GRP0 						; sta_zp 3 = 21

;	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	cpy #LastScanlineIndex 	; last scanline index to calculate is VisableScanLinesCount-1 
	bne	FillBuffersForNextScanline	; if couter not 1 repeat

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

;	playfield data is fliped vertical (upsidedown)
PlayfiledPF0
	REPEAT PlayfiledSize
	.byte #%11111111
	REPEND
	REPEAT [VisableScanLinesCount - 2*PlayfiledSize]
	.byte #%00010000
	REPEND
	REPEAT PlayfiledSize
	.byte #%11111111
	REPEND

PlayfiledPF1
	REPEAT PlayfiledSize
	.byte #%11111111
	REPEND
	REPEAT [VisableScanLinesCount - 2*PlayfiledSize]
	.byte #%00000000
	REPEND
	REPEAT PlayfiledSize
	.byte #%11111111
	REPEND

PlayfiledPF2
	REPEAT PlayfiledSize
	.byte #%11111111
	REPEND
	REPEAT [VisableScanLinesCount - 2*PlayfiledSize]
	.byte #%00000000
	REPEND
	REPEAT PlayfiledSize
	.byte #%11111111
	REPEND
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