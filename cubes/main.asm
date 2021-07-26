********************************************************************************
* Void Versus Cubes - an intro for Revision 2019
* by hukka of void^exec^svenonacid
* vasm, OCS unexpanded
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
;NOAUDIO			; disable audio
;SUNTRONIC
;DEBUG			; effect routines modify background color

********************************************************************************
* Constants
********************************************************************************

SCREENWIDTH_T	= 320
SCREENHEIGHT_T	= 256
LINEWIDTH	= SCREENWIDTH_T/8
BPLSIZE		= SCREENWIDTH_T*SCREENHEIGHT_T/8
GRADSIZE	= GRADIENTHEIGHT * LINEWIDTH * 2
BPLSIZE2	= (BPLSIZE + (GRADSIZE * 2)) * 2

; animation

TRISIZE		= 13056/8
ANIMSCATTER	= 8*4
COLORSPEED	= 1

; textwriter

TW_YSIZE	= 22	; line spacing
DELAY_SCREEN	= 200	; delay (in updates) between screens
GRADIENTHEIGHT	= 23
GRADIENTLENGTH	= 92

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
	lea	BufferFore, a0
	add.w	#GRADSIZE, a0
	move.l	a0, ForeBuffer
	lea	BufferBack, a0
	add.w	#GRADSIZE, a0
	move.l	a0, BackBuffer

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

.done	bsr	DrawLogo1
	bsr	BlitChar_Init

InitDone: ; -------------------------------------------------------------------

	lea	CUSTOM, a6

	move.l	#CopperList, COP1LCH(a6)	; init Copperlist
	;move.w	#32768|DMAF_BLITHOG, DMACON(a6)	; blitter hog mode on

	ifnd	NOAUDIO
	bsr	InitMusic
	endc
	ifd	SUNTRONIC
	bsr	Player_Init
	endc

	ifd	VBLANK
	lea	VBlank(pc), a0			; init VBL interrupt
	bsr	SystemAddVBlankRoutine
	endc

********************************************************************************
* Main loop
********************************************************************************

SCANLINE = 214

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

Quit:	ifd	SUNTRONIC
	bsr	Player_Exit
	endc

	lea	CUSTOM, a6
	bra	ProgramShutdown

********************************************************************************
* VBlank interrupt
********************************************************************************

	ifd	VBLANK

VBlank:
	pushall
	lea	CUSTOM, a6

	ifnd	SUNTRONIC
	DEBUGCOLOR $F0F
	bsr	AnimateCubes
	DEBUGCOLOR $00F
	bsr	InitMode
	DEBUGCOLOR $0F0
	bsr	TextWriter
	DEBUGCOLOR $000
	endc

	ifd	SUNTRONIC
	lea	CUSTOM, a6
	bsr	SunTronicPlayer
	endc

.done	popall
	zero	d0
.out	rts

	endc

********************************************************************************
* Draw Versus/Void logo                      a0 = logo bitmap, a1 = dest buffer

DrawLogo:
.cpy	move.l	(a0)+, (a1)+
	dbf	d0, .cpy
	rts

DrawLogo1:	; Versus logo
	lea	Logo1, a0
	move.l	#48*80/4-1, d0	; height
	lea	BufferBack, a1
	add.l	#(97+GRADIENTHEIGHT)*80, a1	; scanline
	bra	DrawLogo

DrawLogo2:	; Void logo
	lea	Logo2, a0
	move.l	#75*80/4-1, d0	; height
	lea	BufferBack, a1
	add.l	#(87+GRADIENTHEIGHT)*80, a1	; scanline
	bra	DrawLogo

********************************************************************************
* Draw a 16x16 Glyph
********************************************************************************

DrawGlyph:
	and.w	#127, d0

	movez.b	TwX, d1 ; x

	movez.b	TwY, d2 ; y
	add.b	d2, d2	; *2
	and.w	#31, d2

; d0 = char
; d1 = x (bytes)
; d2 = y (text line)
;
BlitChar:
	move.l	BackBuffer, a1
	lea	YRow, a0
	add.w	(a0,d2.w), a1	; dest y
	add.w	d1, a1		; dest x

	lea	Font, a0
	lea	FontPtrs, a2
	add.b	d0, d0
	add.w	(a2,d0.w), a0	; glyph bitmap location

	movez.b	#16-1, d3	; loop counter
.copy	move.b	(a0)+, 0(a1)	; glyph left  half, plane 0
	move.b	(a0)+, 1(a1)	; glyph right half, plane 0
	move.b	(a0)+, 40(a1)	; glyph left  half, plane 1
	move.b	(a0)+, 41(a1)	; glyph right half, plane 1
	add.w	#80, a1		; next scanline
	dbf	d3, .copy
	rts

BlitChar_Init:
	lea	FontPtrs, a0
	moveq	#0, d1
	movez.b	#128-1, d0	; ctr
.f	move.w	d1, (a0)+	; fill FontPtrs
	add.w	#64, d1
	dbf	d0, .f

	lea	YRow, a0
	moveq	#0, d1
	add.l	#80*4, d1
	moveq	#16-1, d0	; ctr
.y	move.w	d1, (a0)+	; fill YRow
	add.w	#TW_YSIZE*80, d1
	dbf	d0, .y
	rts

********************************************************************************
* Text writer
********************************************************************************

TwMode:	dc.b	1		; 0.wait, 1.write, 2.fade, 3.clear, 4.copy
TwCtr:	dc.b	1		; Counter
TwIdx:	dc.w	0		; text char index

TwX:	dc.b	0		; x loc (in chars)
TwY:	dc.b	0		; y loc (scanline)

Initing:dc.b	1
TwPage:	dc.b	0

; write text to backbuffer		1. write
; fade back+temp to fore buffer		2. fade
; wait					0. wait
; copy front to temp			4. copy
; clear backbuffer			3. clear

	ALIGN32

TextWriter:
;	movez.b	Initing, d0
;	bne	.out

	move.b	TwMode, d0	; get current mode
	beq	.Wait
	cmp.b	#1, d0
	beq	.Write
	cmp.b	#2, d0
	beq	.Fade
	cmp.b	#3, d0
	beq	.Clear
	cmp.b	#4, d0
	beq	.Copy

.Write	; ---------------------	; 1 = writing

	lea	Texts, a0
	move.w	TwIdx, d0	; text[]
	addq.w	#1, d0		; advance to next char
	move.w	d0, TwIdx
	move.b	(a0, d0.w), d0	; get the char
	cmp.b	#10, d0		; line ends?
	beq	.newlne
	cmp.b	#32, d0		; space = skip drawing
	beq	.space
	cmp.b	#'#', d0	; screen ends
	beq	.newscr
	cmp.b	#-1, d0		; end of text data
	beq	.scrend

	sub.b	#32, d0		; d0 = glyph, 0=space
.draw	bsr	DrawGlyph	; draw it!

.drawn	add.b	#2, TwX		; advance X coord
	rts			; done!

.newlne	; new text line
	move.b	#0, TwX
	add.b	#1, TwY
	bra	.getlen		; return

.scrend	; text data over, redo from start
	move.w	#0, TwIdx
	move.b	#-1, TwPage

.newscr	; finished with text for current screen

	add.b	#1, TwPage
	move.w	#0, TwX		; reset TwX and TwY
	move.b	#2, TwMode	; switch mode to fading

	cmp.b	#2, TwPage
	beq	DrawLogo1
	cmp.b	#0, TwPage
	bne	.out
	bra	DrawLogo2

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
.cd	add.b	d1, d1
	moveq	#40, d2
	sub.b	d1, d2
	divu.w	#2, d2
	move.b	d2, TwX
	rts

.Fade	; ---------------------	; 2 = fading

	move.w	TwX, d0
	addq.w	#1, d0
	move.w	d0, TwX
	cmp.w	#255+GRADIENTHEIGHT, d0
	bhs	.fend		; finished fading

	move.b	d0, d1		; alternate between two gradient masks
	and.w	#1, d1
	mulu.w	#GRADIENTLENGTH, d1
	lea	AndGradient1, a0 ; mask bitmap
	add.w	d1, a0

	move.l	BackBuffer, a1	; source buffer
	lea	TempBuffer, a2
	move.l	ForeBuffer, a3	; dest buffer
	move.w	d0, d1
	mulu.w	#80, d1

	add.w	d1, a1		; src  scanline
	add.w	d1, a2		; temp scanline
	add.w	d1, a3		; dest scanline

	move.w	#GRADSIZE, d1
	sub.w	d1, a1
	sub.w	d1, a3
	sub.w	d1, a2

	; apply raster gradient fx
	;
	moveq	#GRADIENTHEIGHT-1, d1
.line	move.l	(a0)+, d2	; get mask
	rept	20
	move.l	(a2)+, d3	; src1
	move.l	(a1)+, d4	; src2
	eor.l	d3, d4		; xor
	and.l	d2, d4		; and mask
	eor.l	d4, d3
	move.l	d3, (a3)+	; dest = src EOR ( (src EOR dest) AND mask)
	endr
	dbf	d1, .line
	rts

.fend	move.w	#0, TwX		; reset TwX and TwY
	move.b	#0, TwMode	; switch mode to waiting
	move.b	#DELAY_SCREEN, TwCtr
	rts

.Clear	; ---------------------	; 3 = clear

	move.w	TwX, d0
	cmp.w	#256-8, d0
	bhs	.nocl

	move.l	BackBuffer, a0	; clear backbuffer
	mulu.w	#80, d0		; Y -> scanline ptr
	add.w	d0, a0
	moveq	#0, d0
	moveq	#8-1, d1
.cl	rept	10
	move.l	d0, (a0)+	; clear a scanline
	move.l	d0, (a0)+	; and the second bitplane
	endr
	dbf	d1, .cl
	add.w	#8, TwX
	rts

.nocl	move.w	#0, TwX		; reset TwX and TwY
	move.b	#1, TwMode	; switch mode to writing
	bra	.getlen

.Copy	; ---------------------	; 4 = copy

	move.w	TwX, d0
	cmp.w	#256-8, d0
	bhs	.nocp

	move.l	ForeBuffer, a0	; copy foreground to temp buffer
	lea	TempBuffer, a1
	mulu.w	#80, d0		; Y -> scanline ptr
	add.w	d0, a0
	add.w	d0, a1
	moveq	#8-1, d1
.cp	rept	10
	move.l	(a0)+, (a1)+	; copy
	move.l	(a0)+, (a1)+
	endr
	dbf	d1, .cp
	add.w	#8, TwX
	rts

.nocp	move.w	#0, TwX		; reset TwX and TwY
	move.b	#3, TwMode	; switch mode to clearing
	rts

.Wait	; ---------------------	; 0 = waiting

	movez.b	TwCtr, d0
	subq	#1, d0
	move.b	d0, TwCtr
	bne	.out		; Counter > 0

	move.w	#0, TwX		; reset TwX and TwY
	move.b	#4, TwMode	; Counter = 0; switch mode to copying
.out	rts


	rem>
SwapBuffers:
	move.l	ForeBuffer, d1	; swap work/display buffers
	move.l	BackBuffer, d0
	move.l	d0, ForeBuffer
	move.l	d1, BackBuffer

	move.l	d0, a0
	lea	bpl_Fg1, a1
	bsr	SetBPLPtrs
	add.w	#40, a0
	lea	bpl_Fg2, a1
	bra	SetBPLPtrs
	<erem

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

.col	cmp.b	#0, Initing	; #1
	bne	.next

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

	ifd	SUNTRONIC
	include	"suntronic.asm"
	endc

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

BufferFore:	blk.b	BPLSIZE2
BufferBack:	blk.b	BPLSIZE2
TempBuffer:	blk.b	BPLSIZE2

; ------------------------------------------------------------------------------

	SECTION ANIM, DATA_C

		ALIGN32

CubeData:	incbin	"data/cube.raw"
CubeBuf:	blk.b	(TRISIZE+LINEWIDTH)*16, 0

; ------------------------------------------------------------------------------

	SECTION	TABLES, DATA

		ALIGN32

ForeBuffer:	dc.l	0
BackBuffer:	dc.l	0

FontPtrs:	ds.w	128	; font glyph ptrs
YRow:		ds.w	16	; byte offsets

AndGradient1:	incbin	"data/grad1.raw"
AndGradient2:	incbin	"data/grad2.raw"

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
Logo1:		incbin	"data/logo.raw"
		ALIGN32
Logo2:		incbin	"data/void.raw"
		ALIGN32
Font:		blk.b	64,0	; space
		incbin	"data/font.raw"

Texts:		dc.b	0
		incbin	"data/text.txt"
		dc.b	-1
		rem>
		even
YTab:		dc.b 1,2,3,5,7,9,11,14,17,21,24,28,32,36,41,46
		dc.b 50,56,61,66,72,78,84,89,96,102,108,114,120,127,133,139
		dc.b 145,152,158,164,170,176,181,187,193,198,203,208,213,217,222,226
		dc.b 230,233,237,240,243,245,248,250,251,253,254,255,0
		<erem
		rem>
XCtr:		dc.b 0
XTab:		dc.b 8,7,6,5,4,4,3,2,2,1,1,0,0,0,0,0
		dc.b 0,0,0,0,0,1,1,2,2,3,3,4,5,6,6,7
		dc.b 8,9,9,10,11,12,12,13,13,14,14,15,15,15,15,15
		dc.b 15,15,15,15,15,14,14,13,13,12,11,11,10,9,8,8,255
		<erem

; ------------------------------------------------------------------------------

