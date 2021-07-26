********************************************************************************
* compofiller for assembly 2019
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

;VBLANK			; use vblank interrupt
;DEBUG			; effect routines modify background color
;NOAUDIO

********************************************************************************
* Constants
********************************************************************************

KEYCODE_ESC	= $75

MODULO		= 160+1 	; number of points
TABCOUNT	= 2		; number of movement tables (1-based)

SCANLINE	= 253+16	; where to start updating if not vblank mode

SCREENWIDTH_T	= 320
SCREENHEIGHT_T	= 256
LINEWIDTH_T	= SCREENWIDTH_T/8
BPLSIZE_T	= SCREENWIDTH_T*SCREENHEIGHT_T/8
BPLSIZE		= BPLSIZE_T

TEXTLINES	= 22

BufferFore	= BufferFore1
DblBuffer1	= BufferFore ;TitlePic   + BPLSIZE_T
DblBuffer2	= BufferFore + BPLSIZE_T ; DblBuffer1 + BPLSIZE_T
ChessBitmap	= TitlePic   + BPLSIZE_T ; DblBuffer2 + BPLSIZE_T

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
	include	"player.asm"

Main:
	bsr	ProgramStartup

	ifnd	NOAUDIO
	lea	Tune, a0
	moveq	#1, d0			; PAL
	move.l	#0, a1			; no external samples
	lea	SampleBuffer, a2
	jsr	P61_Init		; d0=0 on return if ok
	endif


InitIntro:
	lea	DblBuffer1, a0
	move.l	#BPLSIZE/2-1, d0		; 2 bpls, size in .l
	bsr	ClearPlayfield

	lea	BufferFore, a0
	move.l	#BPLSIZE/4-1, d0		; 1 bpl, size in .l
	bsr	ClearPlayfield

ShowPicture:
	lea	TitlePic+(0*10240), a0
	lea	bplTitle1, a1
	bsr	SetBPLPtrs
	lea	TitlePic+(1*10240), a0
	lea	bplTitle2, a1
	bsr	SetBPLPtrs
	lea	TitlePic+(2*10240), a0
	lea	bplTitle3, a1
	bsr	SetBPLPtrs
	lea	TitlePic+(3*10240), a0
	lea	bplTitle4, a1
	bsr	SetBPLPtrs
	lea	TitlePic+(4*10240), a0
	lea	bplTitle5, a1
	bsr	SetBPLPtrs

	move.l	#IntroCopperPic, COP1LCH+CUSTOM	; initialize Copperlist

	bsr	WaitInputRelease

	move.b	#0, ScrollX

	; wait 5 seconds or mouse click
	;
	move.l	#50*7, d0			; delay counter
.wait	WaitForVBL
	btst	#6, CIA_A			; left mouse clicked?
	beq.w	.intro				; yes, done
	dbf	d0, .wait

	; switch from title pic to scroller
	;
.intro	bsr	WaitInputRelease

	lea	ChessPacked, a0			; init chessboard bitmap
	lea	DblBuffer1, a1
	push	a1
	bsr	Decrunch

	lea	ChessBitmap, a0
	push	a0
;	lea	bplChess, a1
;	bsr	SetBPLPtrs

	pop	a0				; dest
	pop	a1				; src
	move.w	#255, d1			; y
.yc	moveq	#80/4-1, d0			; x
.cp	move.l	(a1)+, d2
	move.l	d2, (a0)+
	not.l	d2
	move.l	d2, 80-4(a0)
	dbf	d0, .cp
	add.w	#80, a0
	dbf	d1, .yc

	; clear buffers
	;
	lea	DblBuffer1, a0
	move.l	#BPLSIZE_T/2-1, d0		; 2 bpls, size in .l
	bsr	ClearPlayfield

	lea	BufferFore, a0
	move.l	#BPLSIZE_T/4-1, d0		; 1 bpl, size in .l
	bsr	ClearPlayfield

InitScroller:
	lea	PointsPtrs, a0
	lea	OrPtrs, a3
	lea	Points, a1	; <-     x,y coords
	lea	OrBuf, a2	; -> .b  or values

	movez.w	#TABCOUNT-1, d3
.tab	movez.w	#MODULO*7-1, d5	; counter
	move.l	a1, (a0)+
	move.l	a2, (a3)+
.loop	movez.b	(a1), d0	; pixel x coord
	movez.b	1(a1), d1	; pixel y coord

	moveq	#-$80, d4
	ror.b	d0, d4
	lsr.w	#3, d0
	;lsl.w	#6, d1		; for 64B wide bitplanes
	mulu.w	#40, d1		; for 40B wide bitplanes
	add.w	d0, d1

	move.b	d4, d0		; double pixels horizontally
	asl.b	#1, d0
	or.b	d0, d4

	move.b	d4, (a2)+	; write 'or' value for this pixel
	move.w	d1, (a1)+	; (a2)+ write screen ptr for this pixel

	dbf	d5, .loop
	dbf	d3, .tab

	; generate copperlist
	;
	lea	CopLines, a0
	zero	d0
.yy	move.w	#BPL2PTH, (a0)+		; dc.w BPL1PTH, 0
	move.w	#0, (a0)+
	move.w	#BPL2PTL, (a0)+		; dc.w BPL1PTL, 0
	move.w	#0, (a0)+
	cmp.b	#74, d0
	bpl	.bb
.tt	move.l	#$00DF80FE, (a0)+	; COPPER_WAIT_NEXT
	bra	.nn
.bb	move.l	#$80DF80FE, (a0)+	; COPPER_WAIT_NEXTD
.nn	addq	#1, d0
	cmp.b	#255, d0
	bne	.yy

	; generate pointer table
	;
	lea	ChessTabY, a0
	lea	ChessYPtrs, a1
	move.w	#255, d0		; loop counter
	moveq	#0, d1			; Y counter
.ptrs	move.b	(a0)+, d2		; read initial
	addq	#1, d1
.ptr	movez.b	(a0)+, d2		; read Y coords
	addq	#1, d1
	cmp.b	#255, d2
	bne.b	.ptr
.ploop	move.w	d1, (a1)+
	dbf	d0, .ptrs

InitDone: ; -------------------------------------------------------------------

	lea	CUSTOM, a6

	move.l	#CopperList, COP1LCH(a6)	; init Copperlist

	ifd	VBLANK
	lea	VBlank(pc), a0			; init VBL interrupt
	move.l	a0, VBIptr			; bsr SystemAddVBlankRoutine
	bsr	SystemAddVBlankRoutine
	endc

********************************************************************************
* Main loop
********************************************************************************

MainLoop:
	ifnd	VBLANK

.wait1		WAITY	#SCANLINE
		bmi.b	.wait1

		bsr	DoubleBuffer
		bsr	ChessZoomer
		bsr	TextScroller

.wait2		WAITY	#SCANLINE
		bpl.b	.wait2
.wait3		WAITY	#(SCANLINE+20)
		bmi.b	.wait3

	endc

	btst	#6, CIA_A		; quit if left mouse clicked
	beq.b	Quit
	cmp.b	#KEYCODE_ESC, SDR_A	; or if Esc pressed
	bne.b	MainLoop

Quit:	lea	CUSTOM, a6
	bra	ProgramShutdown

********************************************************************************
* VBlank interrupt
********************************************************************************

	ifd	VBLANK

VBlank:
	pushall
	lea	CUSTOM, a6

	bsr	DoubleBuffer
	bsr	ChessZoomer
	bsr	TextScroller

.done	popall
	zero	d0
.out	rts

	endc

WaitInputRelease:
.wait	btst	#6, CIA_A			; left mouse down?
	beq.w	.wait				; still down
	rts

********************************************************************************
DoubleBuffer:

	DEBUGCOLOR $0FF

	eor.b	#1, WhichBuffer
	bne	.b2
.b1	lea	DblBuffer1, a0
	lea	DblBuffer2, a3
	bra	.bd
.b2	lea	DblBuffer2, a0
	lea	DblBuffer1, a3
.bd	push	a0
	lea	bplT1, a1
	bsr	SetBPLPtrs
	pop	a0
	add.w	#40, a0
	lea	bplT3, a1
	bra	SetBPLPtrs

********************************************************************************
ChessZoomer:
	DEBUGCOLOR $FF0

	; scroll
	;
;	rem>
	lea	ChessCtrX, a0
	movez.w	(a0), d0
	addq	#2, d0
	cmp.w	#714, d0
	bne	.ok1
	zero	d0
.ok1	move.w	d0, (a0)
;	<erem

	lea	ChessTabX, a0
	add.w	d0, a0
	movez.b	(a0)+, d4	; d4=coarse scroll
	movez.b	(a0), d0
	lea	XScroll, a0
	move.b	d0, 3(a0)	; bplcon1 = fine scroll

	; zoom
	;
	lea	ChessCtrZ, a0
	move.w	(a0), d0
	addq	#1, d0
	cmp.w	#466, d0
	bne	.ok2
	zero	d0
.ok2	move.w	d0, (a0)

	lea	ChessTabZ, a0
	movez.b	(a0,d0.w), d1	; Y scanline
;	moveq	#60, d1
	push	d1

	; write scrolltext palette
	lea	TextFades, a0
	movez.w	d1, d2
	lsr.b	#4, d2
	add.b	d2, d2
	move.w	(a0,d2.w), d2
	lea	TextFadePal, a0
	move.w	d2, 2+0(a0)
	move.w	d2, 2+4(a0)
	move.w	d2, 2+8(a0)

	mulu.w	#80*2, d1

	lea	ChessBitmap, a0
	add.w	d4, d1		; add coarse scroll
	add.l	d1, a0
;	add.w	#20, a0		; center
	move.l	a0, a4		; normal scanline ptr
	move.l	a0, a5
	add.w	#80, a5		; inverted scanline ptr

	; copperlist
	;
	pop	d1		; initial scanline
	add.w	d1, d1
	lea	ChessTabY, a0
	lea	ChessYPtrs, a1
	add.w	(a1,d1.w), a0

	lea	CopLines, a1	; copperlist
	addq	#2, a1

	movez.b	(a0)+, d4	; initial
	bne	.go		; =1
	exg.l	a4, a5

.go	zero	d0
	bra	.nexty

.y	cmp.b	d0, d4
	beq	.nexty

.ydo	move.w	d2, (a1)	; dc.w BPL1PTH, 0
	move.w	d3, 4(a1)	; dc.w BPL1PTL, 0

	add.w	#12, a1
	addq	#1, d0
	cmp.b	#0, d0
	bne	.y

	DEBUGCOLOR $999
	rts

.nexty	move.b	(a0)+, d4	; next line to invert on
	exg.l	a4, a5
	move.l	a5, d2
	move.w	d2, d3
	swap	d2
	bra	.y

********************************************************************************
TextScroller:

	DEBUGCOLOR $F00

	rem>
	lea	TabIndex, a0
	movez.b	(a0), d0
	addq	#1, d0
;	and.b	#127, d0
	move.b	d0, (a0)

	lsr.b	#1, d0
	and.w	#127, d0
	
	lea	TabSin, a0
	move.b	(a0,d0.w), d0
	<erem

	lea	PointsPtrs, a1	; -> .w  screen offsets
	movez.b	TabIndex, d0
	asl.w	#2, d0		; *4 for longword access
	move.l	(a1,d0), a0
	lea	OrPtrs, a2	; -> .b  or values
	move.l	(a2,d0), a1

	lea	ScrollBuf, a2
	zero	d0
	zero	d1
	moveq	#7-1, d6	; Y
.x	move.w	#MODULO-1, d5	; X

.plot	move.w	(a0)+, d3	; screen ptr
	move.b	(a1)+, d0	; or value

	move.b	(a2)+, d2	; pixel on/off?
	beq.b	.unset		; off
	
.setp	or.b	d0, (a3,d3.w)	; set pixel
	dbf	d5, .plot

.next	addq	#8, a2
	dbf	d6, .x
	bra	.scroll

.unset	eor.b	#255, d0
	and.b	d0, (a3,d3.w)	; unset pixel

	dbf	d5, .plot
	bra	.next
	
.scroll
	DEBUGCOLOR $0F0

	lea	ScrollBuf, a0
	move.l	a0, a1
	addq	#1, a1
.cy	moveq	#7-1, d6		; Y
.cx	movez.w	#(MODULO+8)/4-1, d5	; 68
.cpx	move.b	(a1)+, (a0)+
	move.b	(a1)+, (a0)+
	move.b	(a1)+, (a0)+
	move.b	(a1)+, (a0)+
	dbf	d5, .cpx
	move.b	(a1)+, (a0)+
	move.b	(a1)+, (a0)+
	move.b	(a1)+, (a0)+
	dbf	d6, .cx

	lea	ScrollX, a0
	movez.b	(a0), d0
	addq	#1, d0
	move.b	d0, (a0)
	cmp.b	#8, d0
	bne	.out

.resetx	move.b	#0, (a0)

	DEBUGCOLOR $FFF
	bsr	DrawGlyph

.out	DEBUGCOLOR $111
	rts

	; ---------------------------------------------------------------------

DrawGlyph:
.char	lea	ScrollText, a5	; a5 = scrolltext
	lea	ScrollChar, a3
	movez.w	(a3), d0
	add.w	d0, a5
	add.w	#1, (a3)	; next char in scrolltext
	movez.b	(a5)+, d0	; d0 = ascii code

	cmp.b	#254, d0	; change to next movement table?
	beq.b	.newtab

	cmp.b	#255, d0	; end of scrolltext?
	bne.b	.blit

	move.w	#0, (a3)	; reset scroll
	move.b	#0, TabIndex	; reset movement table index
	bra.b	.char

.blit	sub.b	#32, d0
	mulu.w	#8, d0
	lea	Font, a2
	add.w	d0, a2		; a2 = glyph bitmap
	lea	ScrollBuf, a1	; a1 = scrolltext pixels
	add.w	#MODULO, a1
	moveq	#7-1, d0	; y size
.dy	moveq	#8-1, d1	; x size
	move.b	(a2)+, d2	; d2 = pixel bits
.dx	btst	#7, d2		; pixel set?
	beq.b	.no		; nope
	move.b	#1, (a1)+	; yes
	bra	.cy
.no	move.b	#0, (a1)+	; no
.cy	lsl.w	#1, d2		; next pixel bit
	dbf	d1, .dx
	add.w	#MODULO, a1
	dbf	d0, .dy
	rts

.newtab	push	d0/a0
	lea	TabIndex, a0
	movez.b	(a0), d0
	addq	#1, d0
	cmp.b	#TABCOUNT, d0
	bne	.nno
	moveq	#0, d0
.nno	move.b	d0, (a0)
	pop	d0/a0
	bra	.char

********************************************************************************
* Includes
********************************************************************************

	include	"decrunch.asm"
	include	"init.asm"			; misc initialization

********************************************************************************
* Data
********************************************************************************

bpl_Fg = bplT1

TEXTCOLOR = $FFC

	SECTION COPPER, DATA_C

		ALIGN32

DARKBG  = $111
LIGHTBG = $224

CopperList:
		dc.w	FMODE,   $0		; AGA slow fetch mode

;		dc.w	DIWSTRT, $2C81		; Display window start (top left)
;		dc.w	DIWSTOP, $2CC1		; Display window stop (bottom right)
;		dc.w	DDFSTRT, $0038		; Display data fetch start (horiz.)
;		dc.w	DDFSTOP, $00D0		; Display data fetch stop  (horiz.)

		dc.w	DIWSTRT, $2C81		; Display window start (top left)
		dc.w	DIWSTOP, $2CB1		; Display window stop (bottom right)
		dc.w	DDFSTRT, $0030		; Display data fetch start (horiz.)
		dc.w	DDFSTOP, $00D0		; Display data fetch stop  (horiz.)

		dc.w	BPLCON0, $0200		; Screen off
		dc.w	BPLCON2, $64		; Playfield 2 on top
		dc.w	BPLCON3, $0C00		; AGA bitplane control

		dc.w	BPL1MOD, -2
		dc.w	BPL2MOD, -42

		ifnd DEBUG
		COPPER_SETCOLOR	0, DARKBG	; setup palette
		endc
		COPPER_SETCOLOR	2, LIGHTBG

		COPPER_SETCOLOR	1, $FDA
		COPPER_SETCOLOR	4, $FC9
		COPPER_SETCOLOR	5, $FFF

;		COPPER_SETCOLOR	3, $C96	; *
;		COPPER_SETCOLOR	6, $F84 ; *
;		COPPER_SETCOLOR	7, $F84 ; *

TextFadePal:	COPPER_SETCOLOR	3, $FDF
		COPPER_SETCOLOR	6, $FDF
		COPPER_SETCOLOR	7, $FDF

XScroll:	dc.w	BPLCON1, $0		; Horizontal scroll control

bplT1:		dc.w 	BPL1PTH, 0, BPL1PTL, 0	; Bitplane pointers
;bplChess:	dc.w 	BPL2PTH, 0, BPL2PTL, 0
bplT3:		dc.w 	BPL3PTH, 0, BPL3PTL, 0

		COPPER_WAIT	53,7 ; 62,7
		dc.w	BPLCON0, PLANES_3|COLOR

CopLines:	blk.b	12*256, 0

;		COPPER_BOTTOM			; -----------------------------
;		COPPER_WAIT	26,7
;		COPPER_SETCOLOR	0, $0F0
		dc.w BPLCON0, 	$0200		; Screen off

		COPPER_END

	ALIGN32

PIC_TOP=8
		; Copperlist for intro screen: title pic

IntroCopperPic:	dc.w BPLCON3,	$0C00 		; AGA bitplane control
		dc.w FMODE,	0		; Slow fetch mode, AGA compatibility

		dc.w DIWSTRT,	$2C81		; Display window start (top left)
		dc.w DIWSTOP,	$2CC1		; Display window stop (bottom right) 16C1
		dc.w DDFSTRT,	$0038		; Display data fetch start (horiz.)
		dc.w DDFSTOP,	$00D0		; Display data fetch stop  (horiz.)

		dc.w BPLCON1,	0		; Horizontal scroll control
		dc.w BPLCON2,	PF2PRI		; Playfield 2 on top
		dc.w BPL1MOD,	0		; Bitplane modulo (odd  planes)
		dc.w BPL2MOD,	0		; Bitplane modulo (even planes)
		dc.w BPLCON0, 	COLOR

		COPPER_SETCOLOR 00, $000
		COPPER_SETCOLOR 01, $110 
		COPPER_SETCOLOR 02, $111
		COPPER_SETCOLOR 03, $211
		COPPER_SETCOLOR 04, $223
		COPPER_SETCOLOR 05, $333
		COPPER_SETCOLOR 06, $331
		COPPER_SETCOLOR 07, $444
		COPPER_SETCOLOR 08, $544
		COPPER_SETCOLOR 09, $432
		COPPER_SETCOLOR 10, $655
		COPPER_SETCOLOR 11, $556
		COPPER_SETCOLOR 12, $8A5
		COPPER_SETCOLOR 13, $573
		COPPER_SETCOLOR 14, $683
		COPPER_SETCOLOR 15, $877

		COPPER_SETCOLOR 16, $CBB
		COPPER_SETCOLOR 17, $766
		COPPER_SETCOLOR 18, $988
		COPPER_SETCOLOR 19, $A99
		COPPER_SETCOLOR 20, $CBA
		COPPER_SETCOLOR 21, $BC6
		COPPER_SETCOLOR 22, $DE8
		COPPER_SETCOLOR 23, $CD7
		COPPER_SETCOLOR 24, $EEB
		COPPER_SETCOLOR 25, $DCB
		COPPER_SETCOLOR 26, $EDC
		COPPER_SETCOLOR 27, $BA9
		COPPER_SETCOLOR 28, $DDC
		COPPER_SETCOLOR 29, $EED
		COPPER_SETCOLOR 30, $FFE
		COPPER_SETCOLOR 31, $000

		COPPER_WAIT	PIC_TOP, 7

bplTitle1:	dc.w BPL1PTH, 0, BPL1PTL, 0	; Bitplane pointers (filled in code)
bplTitle2:	dc.w BPL2PTH, 0, BPL2PTL, 0	; 
bplTitle3:	dc.w BPL3PTH, 0, BPL3PTL, 0	; 
bplTitle4:	dc.w BPL4PTH, 0, BPL4PTL, 0	; 
bplTitle5:	dc.w BPL5PTH, 0, BPL5PTL, 0	; 

		COPPER_WAIT_NEXT
		dc.w BPLCON0, 	PLANES_5|COLOR	; Screen on

		COPPER_BOTTOM
		COPPER_WAIT	44,7
		dc.w BPLCON0, 	$0200		; Screen off

		COPPER_END			; End copperlist

********************************************************************************

	SECTION CHIP, BSS_C

		ALIGN64

SampleBuffer:	blk.b	6730
BufferFore1:	blk.b	BPLSIZE
BufferFore2:	blk.b	BPLSIZE

; ------------------------------------------------------------------------------

	SECTION INTRODATA, DATA

ScrollText:	dc.b	"      ******************                        oh no       "
		dc.b	"SORRY GUYS I DUNNO HOW TO CODE A PROPER CHESSBOARD ZOOMER "
		dc.b	"                     ",254
		dc.b	"ANYWAY WELCOME TO ANOTHER RUSHED COMPOFILLER FROM VOID!  "
		dc.b	"                     ",254
		dc.b	"CODE AND MUSIC BY hukka * PIC BY wuf                  "
		dc.b	"ALSO WELCOME TO OUR NEWEST MEMBER: * Kisu *!"
		dc.b	"                     ",254
		dc.b	"  GREETINGS TO * EXEC * SVENONACID * IVORY LABS * "
		dc.b	"8BITBUBSY * ARCHYX * DJSEBA * HAIKZ * HOFNARR * "
		dc.b	"KAARLO * MUZZY * ROSS * STINGRAY * TSR * VEDOS * WUFFE *"
		dc.b	"                     ",254
		dc.b	"   LOOK OUT FOR ISSUE 9 OF THE VERSUS DISKMAG FROM "
		dc.b	"NUKLEUS & VOID LATER THIS YEAR! "
		dc.b	"                     ",255


WhichBuffer:	dc.b	1

		ALIGN32

TextFades:	dc.w	$FDA, $FDA, $ED9, $DD9, $CC9, $BB9, $AA9, $998
		dc.w	$888, $778, $668, $557, $446, $446, $446, $446
;		dc.w	$224, $224, $224, $224, $224, $224, $224, $224
;		dc.w	$223, $222, $221, $220, $210, $110, $100, $000

Points:		incbin	"data/coordtab00.bin"	; 9
		incbin	"data/coordtab01.bin"	; half circle

;TabSin:	incbin	"data/coordtabs.sin"

ChessTabZ:	incbin	"data/chess_z.bin"	; chessboard zoom values
ChessTabX:	incbin	"data/chess_x.bin"	; chessboard scroll values
ChessTabY:	incbin	"data/chess_y.bin"	; chessboard row coords
ChessPacked:	incbin	"data/chessboard.apk"	; chessboard bitplane 640x256

Font:		blk.b	8, 0			; space
		incbin	"data/font.raw"

; ------------------------------------------------------------------------------

	SECTION SONG, DATA_C

TitlePic:	incbin	"data/bigcat.raw"		; 320x256x5p, 50 KB
Tune:		incbin	"data/music/p61.owo"

; ------------------------------------------------------------------------------

	SECTION TABLES, BSS

		ALIGN32

ScrollChar:	ds.w	1
ScrollX:	ds.b	1
TabIndex:	ds.b	1
ChessCtrZ:	ds.w	1
ChessCtrX:	ds.w	1
TextBuffer:	blk.b	80*(TEXTLINES+1)

		ALIGN32

ChessPtrs:	ds.l	256
ChessYPtrs:	ds.w	256

PointsPtrs:	ds.l	TABCOUNT
OrPtrs:		ds.l	TABCOUNT

ScrollBuf:	ds.b	(MODULO+8)*7
OrBuf:		ds.b	(MODULO*7)*TABCOUNT

; ------------------------------------------------------------------------------
