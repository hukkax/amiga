********************************************************************************
* Versus 9 hype intro for Revision 2019
* by hukka of void/exec/svenonacid
* vasm, OCS/AGA
********************************************************************************

	machine	68000

	opt     a+,o+,l+				; optimize
	opt	ob+,oc+,od+,og+,oj+,ol+,om+,on+,ox+	; vasm specific
	;opt	ow+					; show optimizations

	xdef	Main

********************************************************************************
* Defines
********************************************************************************

VBLANK			; use vblank interrupt
;NOAUDIO		; disable audio
;DEBUG			; effect routines modify background color

********************************************************************************
* Constants
********************************************************************************

SCREENWIDTH_T	= 320
SCREENHEIGHT_T	= 256
LINEWIDTH	= SCREENWIDTH_T/8
BPLSIZE		= SCREENWIDTH_T*SCREENHEIGHT_T/8

; animation

TRISIZE		= 13056/8
ANIMSCATTER	= 8*4
COLORSPEED	= 1

; textwriter

TW_INITX	= 0	; initial line X
TW_INITY	= 4	; initial line Y
TW_YSIZE	= 22	; line spacing
DELAY_GLYPH	= 1	; frames between chars
DELAY_SCREEN	= 250	; delay (in updates) between screens

; copperlist

BGCOL	= $CB9		; initial background color
TRIY	= 45		; cubes start Y
TRIH	= 35		; cube slice height in scanlines

********************************************************************************
* Macros
********************************************************************************

	macro	DEBUGCOLOR
		ifd	DEBUG
		move.w	#\1, COLOR00(a6)
		endc
	endm

	macro	WAITY
		move.l	VPOSR(a6), d0
		and.l	#$1ff00, d0
		cmp.l	\1<<8, d0
	endm

********************************************************************************
* Initialization
********************************************************************************

	SECTION CODE

	include	"startup.asm"
	include	"custom.i"
	include	"util.i"

;	include	"blitter.asm"

	ifnd	NOAUDIO
	include	"player.asm"
	endc

Main:	bsr	ProgramStartup
	bra	InitAll

********************************************************************************
* Init routines
********************************************************************************

InitAll:
	move.l	#0, a0
	lea	CubeCtrs, a1
	moveq	#7-1, d0
.fcp	move.l	a0, (a1)+
	add.w	#ANIMSCATTER, a0	; anim frame for this row
	dbf	d0, .fcp

	lea	CubePtrs, a1
	lea	trib1, a0
	move.l	a0, (a1)+
	lea	trib2, a0
	move.l	a0, (a1)+
	lea	trib3, a0
	move.l	a0, (a1)+
	lea	trib4, a0
	move.l	a0, (a1)+
	lea	trib5, a0
	move.l	a0, (a1)+
	lea	trib6, a0
	move.l	a0, (a1)+
	lea	trib7, a0
	move.l	a0, (a1)+


	; convert text to lowercase
	lea	Texts, a0
.gc	move.b	(a0)+, d0
	cmp.b	#-1, d0
	beq	.done
	cmp.b	#95, d0
	bls	.gc
	sub.b	#32, d0
	move.b	d0, -1(a0)
	bra	.gc

.done	; draw Versus logo
	lea	Logo, a0
	lea	BufferFore, a1
	add.l	#97*80, a1	; Y
	move.l	#48*80/4-1, d0	; Height
.cpy	move.l	(a0)+, (a1)+
	dbf	d0, .cpy


InitDone: ; -------------------------------------------------------------------

	lea	CUSTOM, a6

	move.l	#CopperList, COP1LCH(a6)	; init Copperlist
	;move.w	#32768|DMAF_BLITHOG, DMACON(a6)	; blitter hog mode on

	ifnd	NOAUDIO
	bsr	InitMusic
	endc

	ifd	VBLANK
	lea	VBlank(pc), a0			; init VBL interrupt
	bsr	SystemAddVBlankRoutine
	endc

********************************************************************************
* Main loop
********************************************************************************

SCANLINE = 224-10

MainLoop:
	ifnd	VBLANK

.wait1		WAITY	#SCANLINE
		bmi.b	.wait1

		bsr	AnimateCubes
		bsr	TextWriter

.wait2		WAITY	#SCANLINE
		bpl.b	.wait2

	endc

	btst	#6, CIA_A		; left mouse clicked?
	bne.b	MainLoop		; no, loop

Quit:	lea	CUSTOM, a6
	bra	ProgramShutdown

********************************************************************************
* VBlank interrupt
********************************************************************************

	ifd	VBLANK

VBlank:
	pushall
	lea	CUSTOM, a6

	DEBUGCOLOR $F0F
	bsr	AnimateCubes
	DEBUGCOLOR $00F
	bsr	InitMode
	DEBUGCOLOR $0F0
	bsr	TextWriter
	DEBUGCOLOR $000

.done	popall
	zero	d0
.out	rts

	endc

********************************************************************************
* Text writer
********************************************************************************

TwIdx:	dc.w	1		; text char index
Initing:dc.b	1
TwYCtr:	dc.b	0		; Y pos for screen change
TwX:	dc.b	TW_INITX	; x loc (in chars)
TwY:	dc.b	TW_INITY	; y loc (scanline)
TwCtr:	dc.b	0		; Counter  - frames between updates
TwCCr:	dc.b	0		; ClearCtr - updates between text screens
TwClr:	dc.b	0		; Counter for clearing screen after transition

	ALIGN32

DrawGlyph:
	; d1 = char
	; TwX, TwY: coords

	movez.b	#64, d3		; glyph size in bytes (for all planes)

	lea	Font, a0
	and.w	#$FF, d1
	mulu.w	d3, d1
	add.w	d1, a0		; glyph bitmap location

	lea	BufferFore, a1	; dest
	movez.b	TwY, d2
	movez.b	TwX, d4
	mulu.w	#80, d2
	add.b	d4, d4
	add.w	d4, d2
	add.w	d2, a1		; destination offset for glyph

	divu.w	#2, d3
	subq	#1, d3		; loop counter

.copy	move.w	(a0)+, (a1)
	add.w	#40, a1
	dbf	d3, .copy
	rts

TextWriter:
	movez.b	Initing, d0
	bne	.out

	movez.b	TwClr, d0
	bne	.clear

	move.b	TwYCtr, d0
	beq	.tw		; 0=not changing screens

	add.b	#1, TwYCtr	; increase lut index
	subq	#1, d0		; 1-based to 0-based lut index
	lea	YTab, a1
	add.w	d0, a1		; YTab[TwYCtr-1]
	movez.b	(a1), d0	; get new screen Y offset
	beq	.ydone		; 0 = stop transition
	mulu.w	#80*1, d0	; byte offset of scanline
.moi	ifnd	DEBUG
	lea	BufferFore, a0
	add.w	d0, a0
	lea	bpl_Fg1, a1
	bsr	SetBPLPtrs
	add.w	#40, a0
	lea	bpl_Fg2, a1
	bsr	SetBPLPtrs
	endc
	rts

.ydone	move.b	#0, TwYCtr
	moveq	#0, d0
	move.b	#1, TwClr
	rts

.tw	movez.b	TwCtr, d0	; don't update every frame
	beq	.ok1		; TwCtr = 0: time to update writer
	subq	#1, d0
	move.b	d0, TwCtr
	rts

.ok1	movez.b	TwCCr, d0	; are we okay to start this screen?
	beq	.ok2		; TwCCr = 0: yes

	; screen is full and waiting to timeout
	subq	#1, d0
	move.b	d0, TwCCr
	bne	.oot
	move.b	#1, TwYCtr	; done with delay, start screen transition
.oot	rts

.ok2	lea	Texts, a0	; get next char and draw it!
	move.w	TwIdx, d0
	addq.w	#1, d0
	move.w	d0, TwIdx	; update char index
	move.b	(a0, d0.w), d1	; get char
	cmp.b	#10, d1		; line ends
	beq	.newlne
	cmp.b	#32, d1		; space = skip drawing
	beq	.space
	cmp.b	#'#', d1	; screen ends
	beq	.newscr
	cmp.b	#-1, d1		; end of text data
	beq	.scrend

	; d1 = glyph
	sub.b	#32, d1
.draw	bsr	DrawGlyph

.drawn	add.b	#1, TwX
.out	move.b	#DELAY_GLYPH, TwCtr
	rts

.newlne	; new text line
	move.b	#TW_INITX, TwX
	add.b	#TW_YSIZE, TwY
	
	bsr	.getlen
	bra	.out

.scrend	; text data over, redo from start
	move.w	#0, TwIdx

.newscr	; finished with text for current screen
	move.b	#TW_INITX, TwX
	move.b	#TW_INITY, TwY
	move.b	#DELAY_SCREEN, TwCCr
.ns2	bsr	.getlen
	bra	.out
	
.space	moveq	#0, d1
	bra	.draw

.getlen	DEBUGCOLOR $F00
	lea	Texts, a0
	add.w	TwIdx, a0
	add.w	#1, a0
	moveq	#-1, d1		; count line length

.count	addq	#1, d1
	cmp.b	#20, d1
	bhi	.cd
	cmp.b	#10, (a0)+
	bne	.count
.cd	moveq	#20, d2
	sub.b	d1, d2
	divu.w	#2, d2
	move.b	d2, TwX
	rts
	
.clear	; d0 = counter
	DEBUGCOLOR $FFF
	movez.b	d0, d1
	subq	#1, d1		; 0-based
	addq	#1, d0
	cmp.b	#16, d0
	beq	.clo
.cld	move.b	d0, TwClr
	moveq	#32*2-1, d0	; 16 scanlines, long writes, 2 planes
	lea	BufferFore, a1
	mulu.w	#80*16, d1
	add.w	d1, a1
	moveq	#0, d1
.c	move.l	d1, (a1)+
	move.l	d1, (a1)+
	move.l	d1, (a1)+
	move.l	d1, (a1)+
	move.l	d1, (a1)+
	move.l	d1, (a1)+
	move.l	d1, (a1)+
	move.l	d1, (a1)+
	move.l	d1, (a1)+
	move.l	d1, (a1)+
	dbf	d0, .c
	rts

.clo	moveq	#0, d0		
	move.b	d0, TwClr
	bra	.moi


InitMode:
	ifd	NOAUDIO
	move.b	#0, Initing
	rts
	endif
	
	ifnd	NOAUDIO
	move.w	P61_CRow, d0
	cmp.b	PrevRow, d0
	beq	.out		; row not changed yet

	move.b	d0, PrevRow	; changed

.doflash
	ifd	DEBUG
	move.w	P61_E8, d0
	moveq	#4, d1
	bsr	DebugTxt
	endif

	move.w	P61_E8, d0

	cmp.b	#1, d0
	bne	.nah		; sync #1: snare in fast part?
	move.w	#0, P61_E8	; yep
	move.b	#1, Invert
	bra	.poh

.nah	move.b	#0, Invert

.pah	cmp.b	#2, d0		; sync #2: change logo + add rainbow?
	bne	.s3
	move.w	#0, P61_E8	; yep
	move.b	#2, Initing
	bra	.poh

.s3	cmp.b	#3, d0		; sync #3: remove logos, start texts?
	bne	.poh

	move.w	#0, P61_E8	; yep
	move.b	#0, Initing	; yep; start screen transition
	move.b	#1, TwCCr
	move.b	#1, TwYCtr
	lea	BgModulo, a0
	move.w	#TRISIZE*2+LINEWIDTH, 2(a0) ; bg modulo
	rts

.poh	movez.b	PrevRow, d0
	beq	.newpat		; row 0 = pattern changed
.out	rts

.newpat	movez.b	Pattern, d0
	addq	#1, d0
	move.b	d0, Pattern
	
.pih	ifnd	DEBUG
	rts
	endif
	endif

	ifd	DEBUG
	moveq	#0, d1
	; d0.b=value, d1.b=x
DebugTxt:
	move.w	TwX, d5
	push	d0
	push	d5

	move.b	d1, TwX
	move.b	#0, TwY

	movez.b	d0, d4
	push	d4

	movez.b	d4, d1
	lsr.b	#4, d1
	lea	HexDigits, a0
	move.b	(a0,d1.w), d1

	bsr	DrawGlyph	; d1 = char = val+16

	add.b	#1, TwX
	pop	d4
	move.b	d4, d1
	and.w	#$F, d1
	lea	HexDigits, a0
	move.b	(a0,d1.w), d1
	bsr	DrawGlyph	; d1 = char = val+16

	move.w	d5, TwX
	pop	d5
.out	pop	d0
	rts

HexDigits:	dc.b	16,17,18,19,20,21,22,23,24,25,33,34,35,36,37,38
	endc

PrevRow:	dc.b	0
Pattern:	dc.b	0
Invert:		dc.b	0

********************************************************************************
* Animate background cubes
********************************************************************************

	ALIGN32

AnimateCubes:
	rem>
	movez.b	XCtr, d1
	lea	XTab, a0
	move.b	(a0,d1.w), d0
	addq	#1, d1
	cmp.b	#255, d0
	bne	.x
	moveq	#0, d1
	move.b	(a0), d0
.x	move.b	d1, XCtr
	lea	XScroll, a0
	lsl.b	#4, d0
	move.b	d0, 3(a0)
	<erem

	movez.w	CubeColCtr, d0
	move.b	CubeColFrm, d1
	cmp.b	#COLORSPEED, d1
	bne	.noc

	addq	#1, d0
	move.b	#-1, CubeColFrm

.noc	cmp.b	#1, Initing
	beq	.noc2
	add.b	#1, CubeColFrm
.noc2	move.w	d0, CubeColCtr
	add.w	d0, d0

	lea	CubeCtrs, a2	; a2=byte offsets to CubeData bpls
	lea	CubePtrs, a3	; a3=where to write the new bpl pointers
	lea	CubeClrs, a4	; color palettes for the rows
	add.w	d0, a4

	moveq	#7-1, d0	; d0=current row 0..7
	moveq	#0, d6

.row	move.l	(a2), d1	; read anim frame for row
	add.l	#4, d1
	cmp.l	#TRISIZE, d1
	beq	.rowreset

.nor	move.l	d1, (a2)+	; d1 = value to add to bpl ptrs

	lea	CubeData, a0
	add.l	d1, a0

	cmp.b	#0, Initing
	beq	.ad
	add.l	#(TRISIZE+LINEWIDTH)*16, a0

.ad	move.l	(a3)+, a1
	bsr	SetBPLPtrs
	add.w	#2*4, a1
	add.l	#TRISIZE+LINEWIDTH, a0
	bsr	SetBPLPtrs

	add.w	#10, a1		; write palette
	move.w	(a4)+, d1	; get main color
	bmi	.colreset

.col	cmp.b	#1, Initing
	beq	.next

	lea	ColTabDef, a5
	bsr	ModifyColor

	cmp.b	#1, d6
	beq	.col2

	cmp.b	#1, WantBgCol
	bne	.nobgt
	lea	topbgcol, a5
	and.w	#$FFF, d1
	move.w	d1, 2(a5)

.nobgt	moveq	#1, d6

.col2	cmp.b	#1, WantBgCol
	bne	.nobg
	move.w	d1, (a1)	; main color

.nobg	cmp.b	#1, Invert
	bne	.c
	eor.w	#$FFF, d1

.c	move.w	d1, d4

	lea	ColTabHig, a5
	bsr	ModifyColor
	move.w	d1, 4(a1)	; bg color

	move.w	d4, d1
	lea	ColTabDrk, a5
	bsr	ModifyColor
	move.w	d1, 8(a1)	; dark color

	move.w	d4, d1
	lea	ColTabBri, a5
	bsr	ModifyColor
	move.w	d1, 12(a1)	; bright color

.next	dbf	d0, .row
.out	rts



.rowreset
	zero	d1
	bra	.nor
	
.colreset
	eori.b	#1, WantBgCol

	lea	CubeClrs, a4
	move.w	(a4)+, d1

	cmp.b	#7-1, d0
	bne	.col

	move.w	#0, CubeColCtr
	bra	.col
	
; d1 = RGB, a5 = nybble lookup table
ModifyColor:
	push	d0,d2

	moveq	#0, d2
	moveq	#0, d5
	moveq	#0, d0

	move.w	d1, d0		; get R
	and.w	#$F00, d0
	lsr.w	#8, d0		; = color shr 8
	move.b	(a5,d0.w), d2
	lsl.w	#8, d2		; put R

	move.w	d1, d0		; get G
	and.w	#$0F0, d0
	lsr.w	#4, d0		; = color shr 4
	move.b	(a5,d0.w), d5
	lsl.b	#4, d5
	or.b	d5, d2		; put G

	move.b	d1, d0
	and.w	#$F, d0		; get B
	move.b	(a5,d0.w), d5
	or.b	d5, d2		; put B

	move.w	d2, d1

	;eor.w	#$FFF, d1

	pop	d0,d2
	rts

WantBgCol:	dc.b	1
	ALIGN32

********************************************************************************
* Includes
********************************************************************************

	include	"init.asm"			; misc initialization

********************************************************************************
* Data
********************************************************************************

	SECTION COPPER, DATA_C

BPLBG1	= BPL1PTH
BPLBG2	= BPL3PTH

BPLFG1	= BPL2PTH
BPLFG2	= BPL4PTH

	ifd	DEBUG
COL0	= 15
	else
COL0	= 0
	endc

	macro	CUBE_SLICE

	dc.w	BPLBG1, 0, BPLBG1+2, 0
	dc.w	BPLBG2, 0, BPLBG2+2, 0

	COPPER_SETCOLOR	COL0, $CB9	; bg
	COPPER_SETCOLOR	3, $DCA
	COPPER_SETCOLOR	2, $BA8
	COPPER_SETCOLOR	1, $CB9

	endm


CopperList:	dc.w	FMODE,   $0	; AGA slow fetch mode

		dc.w	DIWSTRT, $2C81	; Display window start (top left)
		dc.w	DIWSTOP, $2CC1	; Display window stop (bottom right)
		dc.w	DDFSTRT, $0038	; Display data fetch start (horiz.)
		dc.w	DDFSTOP, $00D0	; Display data fetch stop  (horiz.)

XScroll:	dc.w	BPLCON1, $0
		dc.w	BPLCON2, $64	; Playfield 2 on top
		dc.w	BPLCON3, $0C00	; AGA bitplane control

BgModulo:	dc.w	BPL1MOD, -LINEWIDTH		; initial bg modulo
;		dc.w	BPL1MOD, TRISIZE*2+LINEWIDTH	; bg modulo
		dc.w	BPL2MOD, 40			; fg modulo

		COPPER_WAIT 10, 0

topbgcol:	COPPER_SETCOLOR	COL0, BGCOL		; bg color

		COPPER_SETCOLOR	09, $444
		COPPER_SETCOLOR	10, $FFF
		COPPER_SETCOLOR	11, $AAA

		COPPER_WAIT	TRIY+(TRIH*0),0

bpl_Fg1:	dc.w 	BPLFG1, 0, BPLFG1+2, 0
bpl_Fg2:	dc.w 	BPLFG2, 0, BPLFG2+2, 0

trib1:		CUBE_SLICE

		dc.w	BPLCON0, PLANES_4|DBLPF|COLOR

		COPPER_WAIT	TRIY+(TRIH*1),0
trib2:		CUBE_SLICE
		COPPER_WAIT	TRIY+(TRIH*2),0
trib3:		CUBE_SLICE
;		COPPER_SETCOLOR	0, $666

;		COPPER_SETCOLOR	09, $FFF	; logo
;		COPPER_SETCOLOR	10, $AAA
;		COPPER_SETCOLOR	11, $666

;		COPPER_SETCOLOR	09, $000
;		COPPER_SETCOLOR	10, $333
;		COPPER_SETCOLOR	11, $555

		COPPER_WAIT	TRIY+(TRIH*3),0
trib4:		CUBE_SLICE
		;COPPER_SETCOLOR	0, $111
		COPPER_WAIT	TRIY+(TRIH*4),0
trib5:		CUBE_SLICE
;		COPPER_SETCOLOR	0, $666
		COPPER_WAIT	TRIY+(TRIH*5),0
trib6:		CUBE_SLICE
;		COPPER_SETCOLOR	0, BGCOL
		COPPER_WAIT	TRIY+(TRIH*6),0
trib7:		CUBE_SLICE
		COPPER_WAIT	TRIY+(TRIH*6),0

		COPPER_BOTTOM			; -----------------------------

		COPPER_WAIT	32,7
		dc.w BPLCON0, 	$0200		; Screen off

		COPPER_END

CopperData_End:	dc.l	0

		; -------------------------------------------------------------

		ALIGN32

CubeFrame:	dc.w	0

		ifnd	NOAUDIO
Song:		incbin	"data/P61.chippy_nr.526.5"
;Song:		incbin	"data/P61.eldorado"
		endc

; ------------------------------------------------------------------------------

	SECTION CHIP, BSS_C

		ALIGN64

Samples:	;blk.b	14102

BufferFore:	blk.b	BPLSIZE*2*2

; ------------------------------------------------------------------------------

	SECTION ANIM, DATA_C

		ALIGN32

CubeData:	incbin	"data/cube.raw"
CubeBuf:	blk.b	(TRISIZE+LINEWIDTH)*16, 0

; ------------------------------------------------------------------------------

	SECTION	TABLES, DATA

CubePtrs:	ds.l	8
CubeCtrs:	ds.l	8
CubeColCtr:	dc.w	0
CubeColFrm:	dc.b	-1

ColTabDef:	dc.b	$3,$3,$3,$3, $4,$5,$6,$7, $8,$9,$A,$B, $C,$C,$C,$C
ColTabHig:	dc.b	$4,$4,$4,$4, $5,$6,$7,$8, $9,$A,$B,$C, $D,$D,$D,$D
;ColTabHig:	dc.b	$6,$6,$6,$7, $8,$9,$A,$B, $C,$D,$E,$F, $F,$F,$F,$F
ColTabBri:	dc.b	$4,$4,$4,$5, $6,$7,$8,$9, $A,$B,$C,$D, $E,$E,$E,$E
;ColTabDrk:	dc.b	$2,$2,$2,$2, $2,$3,$4,$5, $6,$7,$8,$9, $A,$B,$B,$B
ColTabDrk:	dc.b	$3,$3,$3,$3, $3,$4,$5,$6, $7,$8,$9,$A, $B,$B,$B,$B
		even

CubeClrs:	dc.w	$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9
		dc.w	$CA9,$C99,$C88,$C77,$C66,$C55,$C44,$C33,$D22,$E11,$F11

		dc.w	$F00,$F10,$F20,$F30,$F40,$F50,$F60,$F70,$F80,$F90,$FA0
		dc.w	$FB0,$FC0,$FD0,$FE0,$FF0,$EF0,$DF0,$CF0,$BF0,$AF0,$9F0
		dc.w	$8F0,$6F0,$5F0,$3F0,$2F0,$0F0,$0F1,$0F2,$0F4,$0F5,$0F6
		dc.w	$0F7,$0F8,$0F9,$1F9,$1FA,$1FB,$1FC,$1FD,$1DE,$1FF,$1EF
		dc.w	$1DF,$1CF,$1BF,$2BF,$2AF,$39F,$38F,$37F,$47F,$46F,$45F
		dc.w	$44F,$43F,$42F
		dc.w	$52F,$51F,$61F,$71F,$81F,$91F,$A1F,$B1F,$C0F,$D0F,$E0F
		dc.w	$F0F,$F0E,$F0C,$F0A,$F09,$F08,$F06,$F04,$F03,$F02,$F01

		dc.w	$E11,$D22,$C33,$C44,$C55,$C66,$C77,$C88,$C99,$CA9

		dc.w	$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9
		dc.w	$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9
		dc.w	$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9
		dc.w	$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9
		dc.w	$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9,$CB9

		dc.w	-1, -1

		ALIGN32
Logo:		incbin	"data/logo.raw"
		ALIGN32
Font:		blk.b	64,0
		incbin	"data/font.raw"

Texts:		dc.b	0
		incbin	"data/text.txt"
		dc.b	-1

		even
YTab:		dc.b 1,2,3,5,7,9,11,14,17,21,24,28,32,36,41,46
		dc.b 50,56,61,66,72,78,84,89,96,102,108,114,120,127,133,139
		dc.b 145,152,158,164,170,176,181,187,193,198,203,208,213,217,222,226
		dc.b 230,233,237,240,243,245,248,250,251,253,254,255,0

		rem>
XCtr:		dc.b 0
XTab:		dc.b 8,7,6,5,4,4,3,2,2,1,1,0,0,0,0,0
		dc.b 0,0,0,0,0,1,1,2,2,3,3,4,5,6,6,7
		dc.b 8,9,9,10,11,12,12,13,13,14,14,15,15,15,15,15
		dc.b 15,15,15,15,15,14,14,13,13,12,11,11,10,9,8,8,255
		<erem

; ------------------------------------------------------------------------------
