********************************************************************************
* "bears" by void
* by hukka of void^exec^svenonacid^furry trash group
* vasm, OCS unexpanded
********************************************************************************

	machine	68000

	opt     a+,o+,l+				; optimize
	opt	ob+,oc+,od+,og+,oj+,ol+,om+,on+,ox+	; vasm specific
	;opt	ow+					; show optimizations

	xdef	Main

	incdir	"data/"
	incdir	"data/lineart/"

********************************************************************************
* Defines
********************************************************************************

VBLANK
;TESTING

********************************************************************************
* Constants
********************************************************************************

;toh=$021
toh=$111

SCANLINE	= 253+16	; where to start updating if not vblank mode

SCREENWIDTH_T	= 320
SCREENHEIGHT_T	= 256
LINEWIDTH	= SCREENWIDTH_T/8
BPLSIZE		= SCREENHEIGHT_T*LINEWIDTH

PART_NORMAL = 0		; normal
PART_INTER  = 1		; intermission (1 pattern long)
PART_CREDITS = 2	; end part, credits+greets

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
	include	"blitter.asm"
	include	"player.asm"
	
	ALIGN32

Main:
	bsr	ProgramStartup

	lea	Tune, a0
	moveq	#1, d0			; PAL
	lea	0, a1			; no external samples
	lea	SampleBuffer, a2
	jsr	P61_Init		; d0=0 on return if ok

InitIntro:
	bsr	InitBitplanes

	lea	CUSTOM, a6

	move.l	#CopperList, COP1LCH(a6)	; init Copperlist
	;move.w	#32768|DMAF_BLITHOG, DMACON(a6)	; blitter hog mode on

	move.b	#-1, transframe
	move.b	#PART_NORMAL, CurrentPart

;	lea	SinTab, a0
;	bsr	GenSineTab

	lea	Text, a0		; init lyrics
	bsr	InitTextWriter

	; init main font
	lea	Font, a1		; a1 = font bitmap
	lea	BufferFore1, a2		; a2 = destination bitplane
	lea	GlyphPtrs, a4		; a4 = pointer to glyphptrs table
	lea	FontStruct, a5		; a5 = pointer to FontStruct
	moveq	#glyphheight, d0	; d0.b = glyph height in pixels
	moveq	#3, d1			; d1.b = font kerning value
	bsr	Font_Init

	; init font outline
	lea	FontStruct, a0		; a0 = source FontStruct
	lea	FontOutline, a1		; a1 = font bitmap
	lea	BufferFore2, a2		; a2 = destination bitplane
	lea	ShadowPtrs, a4		; a4 = pointer to glyphptrs table
	lea	ShadowStruct, a5	; a5 = dest FontStruct
	bsr	Font_InitOutline


	; fill with random noise
	rem
	lea	Noise, a0
	move.l	a0, NoisePtr
	move.l	#$3E50B28C, d1
	move.l	#$D461A7F9, d2
	move.w	#BPLSIZE*2/4-1, d0
.rnd	move.l	d2, d3
	swap	d3
	add.l	d1, d2
	add.l	d3, d1
	move.l	d3, (a0)+
	dbf	d0, .rnd

	lea	Noise, a0		; fill with random noise
;	move.w	#%11101011100011, d2
	move.w	#%1010101010101010, d2
;	move.w	#%1100110011001100, d2
	move.w	#SCREENHEIGHT_T/2-1, d3
.y	move.w	#LINEWIDTH*2-1, d0
.x	move.w	(a0), d1
	and.w	d2, d1
	move.w	d1, (a0)+
	dbf	d0, .x
;	ror.w	#1, d2
	dbf	d3, .y
	erem

	; create dithered gradient bitplanes
	;
	movez.w	#BPLSIZE, d0

	lea	Noise, a1
	move.l	a1, NoisePtr
	move.l	a1, a2
	add.l	d0, a2
	;sub.l	#(LINEWIDTH*2), a2

	lea	NoiseRaw, a0
	move.w	#255, d6		; height ctr

.y	move.w	(a0)+, d0
	eor.w	#-1, d0
	move.w	d0, d1
	ror.w	#3, d1		; #6

	move.w	#LINEWIDTH/2-1, d5	; x word ctr
.x	move.w	d1, (a2)+
	move.w	d0, (a1)+
	dbf	d5, .x
	dbf	d6, .y


	ifd	VBLANK
		lea	VBlank(pc), a0	; init VBL interrupt
		;move.l	a0, VBIptr	; bsr SystemAddVBlankRoutine
		bsr	SystemAddVBlankRoutine
	endc

********************************************************************************
* Main loop
********************************************************************************

MainLoop:
	ifd	VBLANK
	ifz.b	FrameStarted, MainLoop
	endc

	bsr	MainLogic
	bsr	WaitFrame

	tst.b	QuitFlag
	bz	MainLoop

Quit:	bra	ProgramShutdown

********************************************************************************
* VBlank interrupt
********************************************************************************
	ifd	VBLANK

VBlank:
	pushall

	move.b	#1, FrameStarted

	; update bg dither
	;
.noise	move.l	NoisePtr, a0
	add.l	#BPLSIZE-(LINEWIDTH*2), a0
	lea	NoiseCtr, a1
	move.b	(a1), d0
	addq.b	#1, d0
	cmp.b	#2, d0
	bne	.ok
	lea	Noise, a0
	zero	d0
.ok	move.l	a0, NoisePtr
	move.b	d0, (a1)
	and.b	#1, d0			; odd/even frame?
	bz	.a
.a	lea	bpls_Noise, a1
	bsr	SetBPLPtrs

	; update text distortion
	;
.text	ifz.b	SnareCtr, .done
	bsr	Sync_Snare\.DoSnare

.done	bsr	SetLaserPal

	popall
	zero	d0
.out	rts

	endc
********************************************************************************

WaitFrame:

	ifnd	VBLANK
.wait2		WAITY	#SCANLINE
		bpl.b	.wait2
.wait3		WAITY	#(SCANLINE+20)
		bmi.b	.wait3
	endc

	btst	#6, CIA_A		; left mouse clicked?
	bz	SetQuit
	btst	#10-8, POTGOR+CUSTOM	; right mouse pressed?
	bz	SetQuit

	ifnd	VBLANK
.wait1		WAITY	#SCANLINE
		bmi.b	.wait1
	endc

	move.b	SDR_A, d5
	cmp.b	#$75, d5		; quit if Esc pressed
	beq	SetQuit

	move.b	#0, FrameStarted
	rts

SetQuit:
	ifd	TESTING
		bsr	Sync_Credits
	else
		move.b	#1, QuitFlag
	endc
	rts

********************************************************************************

MainLogic:
	lea	CUSTOM, a6

	cmp.b	#-1, transframe
	beq	.sync

.wipe	lea	BufferBack1, a0		; a0 = source bitplane
	lea	BufferBack3, a1		; a1 = desination bitplane
	bsr	RenderTransition

	lea	BufferBack2, a0		; a0 = source bitplane
	lea	BufferBack4, a1		; a1 = desination bitplane
	bsr	RenderTransition

	bsr	AdvanceTransition	; screen wipe effect

.sync	include	"sync.asm"

********************************************************************************
; a0=line data
;
DrawImage:
	move.b	(a0), d0
	asl.w	#8, d0			; possibly unaligned word
	move.b	1(a0), d0
	ifz.w	d0, .out		; no image

	pushall
	lea	BufferBack1, a0
	bsr	FastClear
	lea	BufferBack2, a0
	bsr	FastClear
	popall

	cmp.w	#$FFFF, d0		; no image data
	beq	.out

	lea	LineStruct, a5
	move.l	a0, linedataptr(a5)

	moveq	#0, d6			; blitter fill
	bsr	DrawLineImage		; filled details
	moveq	#3, d6			; full thickness with blitter fill
	bsr	DrawLineImage		; silhouette + outline
	moveq	#3, d6			; full thickness
	bsr	DrawLineImage		; details

.out	movez.b	CurrTrans, d0
	add.b	d0, d0
	add.w	d0, d0
	lea	TransPtrs, a0
	add.l	d0, a0
	move.l	(a0), a0

	bra	InitTransition

********************************************************************************
; no input
;
DrawText:
	move.l	TextPtr, a0
	ifz.l	a0, .out

	lea	FontStruct, a5		; FontStruct
	move.b	(a0), d0		; get ascii

	cmp.b	#'#', d0		; draw new image?
	beq	.image
	cmp.b	#'$', d0		; clear bottom text? (191..235)
	beq	.clear
	cmp.b	#'%', d0		; clear whole fg?
	beq	.clearall

	movez.b	TextX, d0
	movez.b	TextY, d1

	pushr	d0-d1/a0
	addq	#1, d0			; X; offset by 1 pixel		
	addq	#1, d1			; Y -"-
	bsr	Font_DrawText		; draw fg text
	subq	#1, d1			; fix kerning between syllables
	move.b	d1, TextX		; d1 = fnt_x(a5)
	popr				; restore X,Y

	lea	ShadowStruct, a5	; FontStruct
	bsr	Font_DrawText		; draw text outline

	ifnz.b	d6, .out		; continue this line next time

.new	cmp.b	#255, (a0)		; end of text?
	beq	.eot

	move.b	(a0)+, TextX		; init next line
	move.b	(a0)+, TextY
	move.b	(a0)+, ArtNum

.out	move.l	a0, TextPtr
	rts

.image	pushall				; time to draw new image

	lea	laserpal+2, a1		; COPPER_SETCOLOR 1, $999/$F00
	move.w	#$999, (a1)

	movez.b	ArtNum, d1		; which one?

	lea	TransTable, a1
	move.b	(a1,d1.w), d0
	move.b	d0, CurrTrans

	add.w	d1, d1
	add.w	d1, d1			; 4 bytes per ptr
	lea	ArtPtrs, a1
	add.l	d1, a1			; get ptr to artwork
	move.l	(a1), a0
	bsr	DrawImage
	popall

.next	addq.l	#1, a0			; advance text ptr
	move.l	a0, TextPtr
	bra	DrawText

CL_Y1=191
CL_Y2=235

.clear					; clears only fg lines 191..235
	pushall
	lea	BufferFore1+(CL_Y1*LINEWIDTH), a0
	lea	BufferFore2+(CL_Y1*LINEWIDTH), a1
	zero	d0
	move.w	#CL_Y2-CL_Y1-1, d1	; height
.c	rept LINEWIDTH/4
	move.l	d0, (a0)+		; 32 pixels
	move.l	d0, (a1)+
	endr
	dbf	d1, .c
	popall
	bra	.next
	
.clearall				; clears the whole fg
	pushall
	bsr	ClearForeground
	popall
	bra	.next

.eot	move.l	#0, a0
	bra	.out

ClearForeground:
	lea	BufferFore2, a0		; clear mask first
	bsr	FastClear		; to minimize glitches
	lea	BufferFore1, a0
	bra	FastClear

InitTextWriter:
	move.b	(a0)+, TextX
	move.b	(a0)+, TextY
	move.b	(a0)+, ArtNum
	move.l	a0, TextPtr
	rts

********************************************************************************
* Includes
********************************************************************************

	include	"init.asm"		; misc initialization
	include	"font.asm"		; font drawing
	include	"linedrawer.asm"	; line drawing
	include	"transition.asm"	; image transition effects

********************************************************************************
* Data
********************************************************************************

	SECTION COPPER, DATA_C

CopperList:	dc.w	FMODE,   $0	; AGA slow fetch mode

		dc.w	DIWSTRT, $2C81	; Display window start (top left)
		dc.w	DIWSTOP, $2CC1	; Display window stop (bottom right)
		dc.w	DDFSTRT, $0038	; Display data fetch start (horiz.)
		dc.w	DDFSTOP, $00D0	; Display data fetch stop  (horiz.)

XScroll:	dc.w	BPLCON1, $0	; x scroll
		dc.w	BPLCON2, %1000000
		;dc.w	BPLCON3, $0C00	; AGA bitplane control

BgModulo:	dc.w	BPL1MOD, 0	; bg modulo
FgModulo:	dc.w	BPL2MOD, 0	; foreground modulo

coppal:		COPPER_SETCOLOR	0, $DCA
		COPPER_SETCOLOR 1, $DCA+$100

		COPPER_SETCOLOR 3, $764
		COPPER_SETCOLOR 2, $764-toh

;$A97 ; brown
;$9A7 ; green

		COPPER_SETCOLOR	5, $A97
		COPPER_SETCOLOR	4, $A97-toh

		COPPER_SETCOLOR	7, $554
		COPPER_SETCOLOR	6, $554-toh

textpal:	COPPER_SETCOLOR	10, $000
		COPPER_SETCOLOR	11, $FFF
laserpal:	COPPER_SETCOLOR	9,  $999 ; $F00

;		COPPER_SETCOLOR	4, $764
;		COPPER_SETCOLOR	5, $A97
;		COPPER_SETCOLOR	6, $554 ;$3CC

bpls_2:		dc.w 	BPL3PTH, 0, BPL3PTL, 0
bpls_4:		dc.w 	BPL5PTH, 0, BPL5PTL, 0
bpls_1:		dc.w 	BPL2PTH, 0, BPL2PTL, 0
bpls_3:		dc.w 	BPL4PTH, 0, BPL4PTL, 0
bpls_Noise:	dc.w 	BPL1PTH, 0, BPL1PTL, 0

		dc.w	BPLCON0, PLANES_5|DBLPF|COLOR

; gradient 2nd part
		COPPER_WAIT	174, 7

;		COPPER_SETCOLOR 1, $DCA+$100
		COPPER_SETCOLOR 2, $764+toh
		COPPER_SETCOLOR	4, $A97+toh
		COPPER_SETCOLOR	6, $554+toh


		COPPER_WAIT	235,7
scroll1:	rept	20
		dc.w	BPLCON1, $0		; x scroll
		COPPER_WAIT_NEXT
		endr

		COPPER_BOTTOM			; -----------------------------

		COPPER_WAIT	3,7
scroll2:	rept	20
		dc.w	BPLCON1, $0		; x scroll
		COPPER_WAIT_NEXTD
		endr

;		COPPER_WAIT	40,7
;		dc.w BPLCON0, 	$0200		; Screen off

		COPPER_END

CopperData_End:	dc.l	0

; ------------------------------------------------------------------------------

	SECTION STUFF, BSS

TextPtr:	ds.l	1
NoisePtr:	ds.l	1
SnarePtr:	ds.l	1

FrameStarted:	ds.b	1
QuitFlag:	ds.b	1

SnareCtr:	ds.b	1
SnareMask:	ds.b	1

CurrTrans:	ds.b	1
CurrentPart:	ds.b	1

TextX:		ds.b	1
TextY:		ds.b	1
ArtNum:		ds.b	1

NoiseCtr:	ds.b	1

; ------------------------------------------------------------------------------

	SECTION CHIP, BSS_C

		ALIGN64

BufferFore1:	blk.b	BPLSIZE	; text main
BufferFore2:	blk.b	BPLSIZE	; text outline

BufferBack1:	blk.b	BPLSIZE	; hidden; image rendered here
BufferBack2:	blk.b	BPLSIZE

BufferBack3:	blk.b	BPLSIZE	; visible; image copied here
BufferBack4:	blk.b	BPLSIZE

Noise:		blk.b	BPLSIZE*2

SampleBuffer:	blk.b	56174

; ------------------------------------------------------------------------------

	SECTION SONG, DATA_C

Tune:		incbin	"P61.bears"

; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------

	macro	incpic
		incbin	"\1-f.dat"
		incbin	"\1-o.dat"
		incbin	"\1-d.dat"
	endm

	macro	lyric
		dc.b	\1*10, \2*24-1, \3
		dc.b	\4, 0
	endm

	macro	credit
		dc.b	\1, \2+5, img_end, \3, 0
	endm
	
	macro	greet
		dc.b	21, 9*24-1, img_end, "$", \1, 0
	endm

; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------

	SECTION PICS, DATA

Font:		incbin	"font.raw"
Dither:		incbin	"dither.raw"
NoiseRaw:	incbin	"gradient2.raw"
EndPic:		incbin	"endpic.raw"	; 256*160

Text:	; %=clear fg $=clear lower #=draw image

x1=4
x2=x1
y1=8
y2=y1+1

	lyric	10,5,img_void,		"#     "
	lyric	6,8,img_logo,		"#    "
	lyric	1,1,img_clear,		"#"

	lyric	x1,y1,img_clear,	"THERE'S THIS ONE"
	lyric	x2,y2,img_secret,	"HUGE #SEC_RET"

	lyric	x1,y1,img_none,		"$FOR WHICH THERE'S"
	lyric	x2,y2,img_none,		"NO TREAT_MENT"

	lyric	7,y1,img_cloak,		"$#I'LL CON_FIRM"
	lyric	7,y2,img_none,		"YOUR WORST FEARS:"
	lyric	10,y1,img_none,		"$ALL SCE_NERS"
	lyric	8, y2,img_bear,		"ARE JUST #BEARS"

	lyric	x1,y1,img_claws,	"#$POUN_DING AT KEYS"
	lyric	x2,y2,img_none,		"ON VIC TWEN_TYS"
	lyric	x1,y1,img_none,		"$WITH THEIR BEAR CLAWS"
	lyric	x2,y2,img_none,		"LIKE THEIR GRAND_PAS"

	lyric	x1,y1,img_beer,		"#$DRIN_KING BEAR BEER"
	lyric	x2,y2,img_none,		"DEAD_LINE DRAWS NEAR"
	lyric	x1,y1,img_1084,		"#$SIX FIVE OH TWO"
	lyric	x2,y2,img_gentoo,	"#IN_STALL GEN_TOO"

	lyric	x1,y1,img_bear2,	"#$TO DIS_GUISE"
	lyric	x2,y2,img_none,		"OUR BEAR ARMS"
	lyric	x1,y1,img_none,		"$WE ALL USE"
	lyric	x2,y2,img_none,		"HO_LO_GRAMS"

	lyric	x1,y1,img_laser,	"#$TO DIS_POSE"
	lyric	x2,y2,img_none,		"OF LA_MERS"
	lyric	x1,y1,img_none,		"$OUR CROT_CHES"
	lyric	x2,y2,img_none,		"SHOOT LA_SERS"

		lyric 3,2,img_none,	"%PEW!"
		lyric 5,3,img_none,	"PEW!"
		lyric 7,4,img_none,	"PEW!"
		lyric 9,5,img_none,	"PEW!"

		lyric 16,1,img_none,	"PEW!"
		lyric 14,2,img_none,	"PEW!"
		lyric 12,3,img_none,	"PEW!"
		lyric 10,4,img_none,	"UWU"

xx1=7
xx2=16
yy1=1
yy2=y2

	lyric	xx1,yy1,img_clive,	"%#SIR CLIVE?"
	lyric	xx2,yy2,img_bear,	"YES #BEAR"
	lyric	xx1,yy1,img_tramiel,	"%#TRA_MIEL?"
	lyric	xx2,yy2,img_bear,	"WAS #BEAR"
	lyric	xx1,yy1,img_jobs,	"%#STEVE JOBS?"
	lyric	xx2,yy2,img_bear,	"MAY_#BEAR"
	lyric	xx1,yy1,img_gates,	"%#BILL GATES?"
	lyric	xx2,yy2,img_lizard,	"#LI_ZARD"

	dc.b	255
	even

LineDrawings:
	lines_none:	dc.w	$0000	; no change
	lines_clear:	dc.w	$FFFF	; clear image
	lines_logo:	incpic	logo
	lines_void:	incpic	void
	lines_claws:	incpic	claws
	lines_beer:	incpic	beer
	lines_bear2:	incpic	bear2
	lines_laser:	incpic	laser
	lines_bear:	incpic	p_bear
	lines_lizard:	incpic	p_lizard
	lines_clive:	incpic	p_clive
	lines_tramiel:	incpic	p_tramiel
	lines_jobs:	incpic	p_jobs
	lines_gates:	incpic	p_gates
	lines_1084:	incpic	c1084
	lines_gentoo:	incpic	gentoo
	lines_cloak:	incpic	cloak
	lines_secret:	incpic	secret
	lines_end:	incpic	e1084


; 0=circle, 1=radial wipe, 2=diagonal, 3=double diagonal
;
TransTable:
		dc.b	0	;img_none
		dc.b	3	;img_clear
		dc.b	3	;img_logo
		dc.b	0	;img_void
		dc.b	1	;img_claws
		dc.b	3	;img_beer
		dc.b	0	;img_bear2
		dc.b	1	;img_laser
		dc.b	0	;img_bear
		dc.b	1	;img_lizard
		dc.b	1	;img_clive
		dc.b	1	;img_tramiel
		dc.b	1	;img_jobs
		dc.b	1	;img_gates
		dc.b	2	;img_1084
		dc.b	4	;img_gentoo
		dc.b	2	;img_cloak
		dc.b	1	;img_secret
		dc.b	0

img_none	= 0
img_clear	= 1
img_logo	= 2
img_void	= 3
img_claws	= 4
img_beer	= 5
img_bear2	= 6
img_laser	= 7
img_bear	= 8
img_lizard	= 9
img_clive	= 10
img_tramiel	= 11
img_jobs	= 12
img_gates	= 13
img_1084	= 14
img_gentoo	= 15
img_cloak	= 16
img_secret	= 17
img_end		= 18

	even
; 0=circle, 1=radial wipe, 2=diagonal, 3=double diagonal
TransPtrs:
	dc.l	tr1
	dc.l	tr2
	dc.l	tr3
	dc.l	tr4
	dc.l	tr5

ArtPtrs:
	dc.l	lines_none
	dc.l	lines_clear
	dc.l	lines_logo
	dc.l	lines_void
	dc.l	lines_claws
	dc.l	lines_beer
	dc.l	lines_bear2
	dc.l	lines_laser
	dc.l	lines_bear
	dc.l	lines_lizard
	dc.l	lines_clive
	dc.l	lines_tramiel
	dc.l	lines_jobs
	dc.l	lines_gates
	dc.l	lines_1084
	dc.l	lines_gentoo
	dc.l	lines_cloak
	dc.l	lines_secret
	dc.l	lines_end


cx = 38
cy = 34
cy2 = cy+56

end_greets:	greet	"^"

		greet	"BAT^CASKET"
		greet	"DESIRE"
		greet	"EPHIDRENA"
		greet	"EXEC"

		greet	"FURRY^TRASH^GROUP"
		greet	"GHOSTOWN"
		greet	"IVORY^LABS"
		greet	"MOODS^PLATEAU"

		greet	"NAH-KOLOR"
		greet	"NUKLEUS"
		greet	"PLANET^JAZZ"
		greet	"REBELS"

		greet	"SVENONACID"
		greet	"THE^ELECTRONIC^KNIGHTS"
		greet	"THE^GANG"
		greet	"WANTED^TEAM"

		greet	"^"
		greet	"LOL"
		dc.b	255

		even
laser_ptr:	dc.l	0
laserpaltab:	dc.w	$DCA,$DB9,$DA8,$E87,$E76,$E65,$E54,$E43
		dc.w	$F22,$F11,$F00,$F00,$F00,$F00,$F44,$FFF
laserpalctr:	dc.b	0
laser_ctr:	dc.b	0
laser_dest:	dc.b	214,180	; dest x,y
laser_tab:	dc.b	143,186,159,128,051,023,097,074 ; y

; ------------------------------------------------------------------------------
