********************************************************************************
* things we know about dogs
* by hukka of void^exec^svenonacid^furry trash group
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

RESOLUTION=1	; 0=lores, 1=medres, 2=hires interlaced (unimplemented)
;NOAUDIO

********************************************************************************
* Constants
********************************************************************************

SCANLINE	= 253+16	; where to start updating if not vblank mode

BufferFore	= BufferFore1
DblBuffer1	= BufferFore1
DblBuffer2	= BufferFore2
bpl_Fg		= bpls_1

	if	RESOLUTION=0
SCREENWIDTH_T	= 320
	else
SCREENWIDTH_T	= 320*2
	endc

SCREENHEIGHT_T	= 256
LINEWIDTH	= SCREENWIDTH_T/8
BPLSIZE		= SCREENHEIGHT_T*LINEWIDTH

FONTWIDTH	= 1888/8

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
	
	ALIGN32

x1	equr	d0
y1	equr	d1
x2	equr	d2
y2	equr	d3

Main:
	bsr	ProgramStartup

	ifnd	NOAUDIO
	lea	Tune, a0
	moveq	#1, d0			; PAL
	move.l	#0, a1			; no external samples
;	lea	SampleBuffer, a2
	move.l	#0, a2
	jsr	P61_Init		; d0=0 on return if ok
	endif

InitIntro:
;	lea	DblBuffer1, a0
;	move.l	#BPLSIZE/2-1, d0		; 2 bpls, size in .l
;	bsr	ClearPlayfield

;	lea	BufferFore, a0
;	move.l	#BPLSIZE/4-1, d0		; 1 bpl, size in .l
;	bsr	ClearPlayfield

	bsr	InitBitplanes

InitDone: ; -------------------------------------------------------------------

	lea	CUSTOM, a6

	move.l	#CopperList, COP1LCH(a6)	; init Copperlist
	;move.w	#32768|DMAF_BLITHOG, DMACON(a6)	; blitter hog mode on

	ifd	VBLANK
	lea	VBlank(pc), a0		; init VBL interrupt
	move.l	a0, VBIptr		; bsr SystemAddVBlankRoutine
	bsr	SystemAddVBlankRoutine
	endc

.out	move.w	$0000, DblBufCtr	; Ctr, Idx

	rem>
	lea	LineData1, a5
	lea	linedata08, a0
	lea	BufferFore1, a1
	move.b	#0, randomness(a5)
	bsr	SetupLineStruct

	lea	LineData2, a5
	lea	linedata08, a0
	lea	BufferFore2, a1
	move.b	#1, randomness(a5)
	bsr	SetupLineStruct
	<erem

	lea	Randoms, a4
	move.l	a4, RandomsPtr

	bsr	NextScreen

********************************************************************************
* Main loop
********************************************************************************

MainLoop:
	bsr	WaitFrame
	bsr	MainLogic

	tst.b	QuitFlag
	bz	MainLoop

Quit:	lea	CUSTOM, a6
	bra	ProgramShutdown

********************************************************************************

WaitFrame:

.wait2	WAITY	#SCANLINE
	bpl.b	.wait2
.wait3	WAITY	#(SCANLINE+20)
	bmi.b	.wait3

;	btst	#6, CIA_A		; left mouse clicked?
;	bz	.Quit
	btst	#10-8, POTGOR+CUSTOM	; right mouse pressed?
	bz	.Quit

.wait1	WAITY	#SCANLINE
	bmi.b	.wait1

	move.b	SDR_A, d5
	cmp.b	#$75, d5		; quit if Esc pressed
	beq	.Quit
	rts

.Quit	move.b	#1, QuitFlag
	rts

********************************************************************************

MainLogic:
	tst.b	AtEnd
	bnz	.db

	move.l	EasingPtr, a1
	cmp.l	#0, EasingPtr
	beq	.db			; not animating transition

	movez.w	(a1)+, d0		; Y offset into bitplane
	cmp.w	#$1234, d0		; end of easing table?
	bne	.ease			; not yet
	move.l	#0, a1			; yes, stop animating transition
	moveq	#0, d0
.ease	move.l	a1, EasingPtr

	lea	BufferText2, a0		; bitplane
	muls.w	#LINEWIDTH, d0		; scanlines->bytes to offset
	ext.l	d0
	sub.l	d0, a0			; calc offset
	push	a0
	lea	bpls_3, a1		; which plane in copperlist
	bsr	SetBPLPtrs		; set offset in copperlist
	pop	a0
	add.w	#LINEWIDTH, a0
	lea	bpls_4, a1		; which plane in copperlist
	bsr	SetBPLPtrs		; set offset in copperlist

.db	bsr	DoubleBuffer

	movez.w	WaitCtr, d0
	tst.w	d0
	bz	LineDrawer

	sub.w	#1, d0
	move.w	d0, WaitCtr
	tst.w	d0
	bz	NextScreen
	rts

********************************************************************************

DoubleBuffer:
	lea	DblBufCtr, a1
	add.b	#1, (a1)
	cmp.b	#4, (a1)		; line wiggle animation speed
	bne	.out

.go	move.b	#0, (a1)+		; point to DblBufIdx
	eor.b	#1, (a1)		; switch double buffers
	bne	.b2
.b1	lea	DblBuffer1, a0
	lea	DblBuffer2, a3
	bra	.bd
.b2	lea	DblBuffer2, a0
	lea	DblBuffer1, a3
.bd	push	a0
	lea	bpls_1, a1
	bsr	SetBPLPtrs
	pop	a0
	lea	bpls_2, a1
	bra	SetBPLPtrs
.out	rts

; -----------------------------------------------------------------------------

ScreenDone:
	move.w	#50*6, WaitCtr
	move.b	#1, TextOdd
	btst	#2, screeninfoval		; text at finish?
	bnz	DrawText
	rts

; -----------------------------------------------------------------------------

NextScreen:
	tst.b	AtEnd
	bnz	.nofade

	; fade to white
	movez.b	#16*2-1, d0
.fade	pushall
	bsr	WaitFrame
	bsr	DoubleBuffer
	popall
	push	d0
	bsr	FadePalette
	pop	d0
.d	dbf	d0, .fade

.nofade	lea	screeninfos, a0
	movez.b	screeninfoidx, d0		; current screen index
	movez.b	(a0,d0.w), d1			; get text trigger flags
	move.b	d1, screeninfoval		; memorize flags
	add.b	#1, screeninfoidx		; advance screen index
	move.b	#$FF, nexttriggery		; disable Y trigger

	btst	#1, d1				; trigger text at specified Y?
	bz	.a				; no
	move.b	1(a0,d0.w), nexttriggery	; yes; get target Y
	add.b	#1, screeninfoidx		;      skip Y value

.a	lea	BufferFore1, a0
	move.l	#BPLSIZE/2-1, d0		; 2 bpls, size in .l
	bsr	ClearPlayfield

	tst.b	AtEnd
	bnz	.noend

	lea	BufferText2, a0			; src
	lea	BufferText1, a1			; dest
	bsr	CopyPlayfield			; a0, a1 = bitplane from, to

	; !!!
	lea	BufferText1, a0
	lea	bpls_3, a1
	bsr	SetBPLPtrs
	lea	BufferText1, a0
	add.w	#LINEWIDTH, a0
	lea	bpls_4, a1
	bsr	SetBPLPtrs

	lea	BufferText1, a0
	move.l	#BPLSIZE/2-1, d0		; 2 bpls, size in .l
	bsr	ClearPlayfield

	bsr	SetPalette

.noend	move.l	DataListPtr, a4
	tst.l	(a4)
	bz	.rest				; move to endpart

	move.l	(a4)+, a2			; linedata ptr
	move.l	(a4)+, TextDataPtr		; textdata ptr
	move.l	a4, DataListPtr

	lea	LineData1, a5
	move.l	a2, a0
	lea	BufferFore1, a1
	move.b	#0, randomness(a5)
	bsr	SetupLineStruct

	lea	LineData2, a5
	move.l	a2, a0
	lea	BufferFore2, a1
	move.b	#1, randomness(a5)
	bsr	SetupLineStruct

.out	move.b	#0, TextOdd

	btst	#0, screeninfoval		; text at start?
	bnz	DrawText
	rts

.rest	lea	DataList, a4			; reset to beginning
	move.l	a4, DataListPtr
	lea	Text, a4
	move.l	a4, TextDataPtr
	move.b	#0, screeninfoidx
	bra	EndScreen

; -----------------------------------------------------------------------------
; a5 = ptr to linedata structure
; a0 = ptr to line data
; a1 = ptr to bitplane
;
SetupLineStruct:
	;move.w	(a0)+, segmentcount(a5)
	addq	#2, a0
	move.l	a1, bitplaneptr(a5)
	move.l	a0, linedataptr(a5)
	move.b	#0, line_done(a5)
	bra	newline

; -----------------------------------------------------------------------------
; no input

LineDrawer:
	moveq	#0, d4		; drawing a line?
	moveq	#0, d3		; finished with whole picture?
	lea	LineData1, a5
	add.w	line_length(a5), d4
	add.b	line_done(a5), d3
	lea	LineData2, a5
	add.w	line_length(a5), d4
	add.b	line_done(a5), d3
	tst.w	d4
	bnz	.draw
	bsr	.DoInit

.draw	lea	LineData1, a5
	tst.b	line_done(a5)
	bnz	.skip
	bsr	.DoDraw

.skip	lea	LineData2, a5
	tst.b	line_done(a5)
	bnz	.out
	bsr	.DoDraw
.out	rts

.DoDraw:
	bsr	DrawLine
	bsr	DrawLine
	bsr	DrawLine
	bsr	DrawLine
	bsr	DrawLine
	bra	DrawLine

.DoInit:
	cmp.b	#2, d3		; all finished drawing picture?
	beq	ScreenDone

.notdon	lea	LineData1, a5
	bsr	newline
	lea	LineData2, a5
	bra	newline

; -----------------------------------------------------------------------------
; a5 = ptr to linedata structure
;
DrawLine:
	tst.w	line_length(a5)	; check the line length counter
	bz	.out

.draw	bsr	line_advance
	bra	plot

.out	rts

newline:
	move.l	linedataptr(a5), a1

	movez.b	(a1)+, x2	; X coord (top bit=start new line?)
	asl.w	#8, x2		; read possibly unaligned word
	move.b	(a1)+, x2

	cmp.w	#$FFFF, x2	; finished drawing the picture?
	beq	picdone		; yes

	bclr	#15, x2		; check and clear top bit; if set => new line
	bz	.next		; continue prev line

	move.w	x2, x1		; start new line

	movez.b	(a1)+, y1
	movez.b	(a1)+, x2	; read possibly unaligned word
	asl.w	#8, x2		;
	move.b	(a1)+, x2	;
	movez.b	(a1)+, y2

	if	RESOLUTION=0
	lsr.w	#1, x1
	lsr.w	#1, x2
	endc

	bra	.go

.next	movez.b	(a1)+, y2	; Y coord

	move.w	line_destx(a5), x1
	move.w	line_desty(a5), y1

.go	move.l	a1, linedataptr(a5)

; -----------------------------------------------------------------------------

line_init: 				; (x1,y1,x2,y2)
	tst.b	randomness(a5)
	bz	.go

.random	move.l	RandomsPtr, a4

.getx	movez.b	(a4)+, d5		; x randomization
	cmp.b	#255, d5
	bne	.gety
.rst	lea	Randoms, a4
	bra	.getx
.gety	subq	#1, d5
	ext.w	d5
	add.w	d5, x2

.gety2	movez.b	(a4)+, d5		; y randomization
	cmp.b	#255, d5
	bne	.foo
	lea	Randoms, a4
	bra	.gety2
.foo	subq	#1, d5
	add.b	d5, y2

	move.l	a4, RandomsPtr

.go	move.w	x1, line_x(a5)		; line_x := x1
	move.w	y1, line_y(a5)		; line_y := y1
	move.w	x2, line_destx(a5)
	move.w	y2, line_desty(a5)

	cmp.b	#255, nexttriggery
	beq	.absx
	cmp.b	nexttriggery, y2
	blo	.absx
	move.b	#255, nexttriggery
	push	d0-d6/a0-a6
	bsr	DrawText
	pop	d0-d6/a0-a6

.absx	sub.w	x1, x2			; line_dx := abs(x2-x1)
	bpl	.absy
	neg.w	x2

.absy	move.w	x2, line_dx(a5)
	movez.w	line_destx(a5), x2

	sub.w	y1, y2			; line_dy := abs(y2-y1)
	bpl	.sx
	neg.w	y2

.sx	move.w	y2, line_dy(a5)
	movez.w	line_desty(a5), y2

	move.w	#1, line_sx(a5)		; line_sx := 1
	cmp.w	x1, x2
	bge	.sy			; if x2 < x1 then
	neg.w	line_sx(a5)		;  line_sx := -1

.sy	move.w	#1, line_sy(a5)		; line_sy := 1
	cmp.w	y1, y2
	bge	.le			; if y2 < y1 then
	neg.w	line_sy(a5)		;  line_sy := -1

.le	move.w	line_dx(a5), d4
	move.w	line_dy(a5), d5
	sub.w	d5, d4
	move.w	d4, line_error(a5)	; line_error := line_dx - line_dy

	; calc line length
	move.w	line_dx(a5), d4		; d4=line_dx
	cmp.w	d4, d5			; d5=line_dy
	bhi	.put			; if line_dx >= line_dy then
	move.w	d4, d5			;   line_length := line_dx
.put	move.w	d5, line_length(a5)

	rts

; -----------------------------------------------------------------------------

line_advance:
	lea	line_error(a5), a1
	move.w	(a1), d0	; d0=e2
	add.w	d0, d0		; e2 := 2 * line_error

	move.w	line_dy(a5), d1
	neg.w	d1		; -line_dy

	cmp.w	d1, d0		; if e2 >= -line_dy
	bge	.lx

.foo	move.w	line_dx(a5), d1	; if e2 <= line_dx
	cmp.w	d1, d0
	ble	.ly
	rts

.lx	; if e2 >= -line_dy then
	move.w	line_dy(a5), d1
	sub.w	d1, (a1)	; line_error -= line_dy
	move.w	line_sx(a5), d1
	add.w	d1, line_x(a5)	; line_x += line_sx
	bra	.foo

.ly	; if e2 <= line_dx then
	add.w	d1, (a1)	; line_error += line_dx
	move.w	line_sy(a5), d1	; line_y += line_sy
	add.w	d1, line_y(a5)
	rts

; -----------------------------------------------------------------------------

plot:	sub.w	#1, line_length(a5)

.ok	move.l	bitplaneptr(a5), a0
	movez.w	line_x(a5), x1	; d0
	movez.w	line_y(a5), y1	; d1
	bsr	.plot		; x, y
	subq	#1, x1
	bsr	.plot		; x-1, y
	addq	#2, x1		; x+1, y
	bsr	.plot
	subq	#1, y1		; x+1, y-1

.plot	push	d0-d1/a0	; a0 = bitplane
				; d0,d1 = x, y
	; manage the Y position
	move.w	d1, d2		; copy y
	lsl.w	#4, d1		; multiply by 40
	lsl.w	#3, d2
	add.w	d1, d1
	add.w	d2, d1
	if	RESOLUTION>0	; for medres/hires bitplanes
	add.w	d1, d1		; scanline length 40->80
	endc
	add.w	d1, a0		; enter y pos first

	; manage the X position
	move.w	d0, d2		; copy x
	lsr.w	#3, d0		; divide by 8 to get the hardposition
	and.w	#$000f, d2	; mask the 4 lower bits for 0-15 softposition
	btst	#0, d0		; is the hardposition an odd value ?
	beq.s	.even		; nope ...skip the -1 action
	subq.w	#1, d0		; it's odd ...sub 1 to keep even steps (68000!)
.even	sub.w	#15, d2
	neg.w	d2
	add.w	d0, a0		; move to X hardposition
	move.w	(a0), d3	; take current screendata word aligned
	bset	d2, d3		; set the softposition pixel
	move.w	d3, (a0)	; put back the modified word

	pop	d0-d1/a0
	rts

; -----------------------------------------------------------------------------

picdone:
	move.b	#1, line_done(a5)
	;lea	coppal, a1
	;move.w	#$FD9, 2(a1)
	rts

; -----------------------------------------------------------------------------
; Palette manipulation
; -----------------------------------------------------------------------------

; Fades a palette table towards white
;
FadePalette:
	moveq	#5-1, d6	; color entries in table
.fra	lea	TempPal, a0
.col	movez.w	(a0), d0	; source color
	move.w	#$FFF, d1	; dest. color
	moveq	#1, d2		; fade towards white
	bsr	FadeColor
	move.w	d1, (a0)+	; write modified color
	dbf	d6, .col

PutPalette:
	lea	TempPal, a3
	lea	coppal, a2	; palette entries in copperlist
	add.w	#6, a2		; point to 1st line fringe color
	move.w	(a3)+, 00(a2)	; 1 line fringe
	move.w	(a3)+, 04(a2)	; 2 line fringe
	move.w	(a3)+, 08(a2)	; 3 line color
	move.w	(a3)+, 12(a2)	; 4 text shadow
	move.w	(a3)+, d4	; text color

	add.w	#16, a2		; 5..15 text color
	moveq	#11-1, d5
.copy	move.w	d4, (a2)
	add.w	#4, a2
	dbf	d5, .copy
	rts

SetPalette:
	move.l	PalPtr, a1	; source palette table
	lea	TempPal, a2
	move.w	(a1)+, (a2)+	; line fringe
	move.w	(a1)+, (a2)+	; line fringe
	move.w	(a1)+, (a2)+	; line color
	move.w	#$CBB, (a2)+	; text shadow
	move.w	(a1)+, (a2)+	; text color

	bsr	PutPalette

	cmp.w	#$FFFF, (a1)
	bne	.out
	lea	Palette1, a1	; reset
.out	move.l	a1, PalPtr
	rts

; Make color dimmer/brighter towards a destination color
; d0 = source color value
; d1 = destination color value, result goes here
; d2 = direction -1/+1
;
FadeColor:
	movez.w	d1, d3		; dest color
	lea	cdR, a1
	bsr	GetRGB

	movez.w	d0, d3		; src color
	lea	csR, a1
	bsr	GetRGB

	; move source RGB bytes towards dest RGB bytes
	;
.fr	move.b	cdR, d3
	cmp.b	csR, d3
	beq	.fg
	add.b	d2, csR

.fg	move.b	cdG, d3
	cmp.b	csG, d3
	beq	.fb
	add.b	d2, csG

.fb	move.b	cdB, d3
	cmp.b	csB, d3
	beq	.tocol
	add.b	d2, csB

	; RGB bytes to color entry => d1
	;
.tocol	movez.b	csR, d1
	asl.w	#8, d1
	move.b	csG, d1
	asl.b	#4, d1
	add.b	csB, d1
	rts

; d3 = color, a1 = rgb structure (csR/cdR)
;
GetRGB:
	move.w	d3, d4
	asr.w	#8, d4
	and.b	#$0F, d4
	move.b	d4, (a1)+	; write R byte

	move.b	d3, d4
	asr.b	#4, d4
	and.b	#$0F, d4
	move.b	d4, (a1)+	; write G byte

	move.b	d3, d4
	and.b	#$0F, d4
	move.b	d4, (a1)+	; write B byte
	rts

; -----------------------------------------------------------------------------
; Text output
; -----------------------------------------------------------------------------

DrawText:
	tst.b	AtEnd
	bnz	.noe

	move.l	#0, EasingPtr

	move.l	TextDataPtr, a2
	lea	TextPos, a3	; x,y bytes
.text	movez.b	(a2)+, x1	; column
	cmp.b	#255, x1
	beq	.empty

	move.b	x1, 0(a3)
	move.b	(a2)+, 1(a3)	; line
	move.b	(a2)+, d4	; type 0/1/2?

	cmp.b	#1, d4
	beq	.t1		; type 1=clear&force easing
	cmp.b	#2, d4
	bne	.t0		; type 0=clear
.t2	move.l	#0, EasingPtr

.line	movez.b	0(a3), x1	; column
	movez.b	1(a3), y1	; line
	add.b	#1, 1(a3)
	add.w	x1, x1
	mulu.w	#20, y1
	mulu.w	#LINEWIDTH, y1

	lea	BufferText2, a0
	add.w	#LINEWIDTH*4, a0
	add.w	x1, a0		; bitplane dest address
	add.w	y1, a0

.char	move.l	a0, a4

	movez.b	(a2)+, d4	; ascii#
	tst.b	d4		; stop drawing text?
	bz	.out
	cmp.b	#'|', d4	; text line finished?
	beq	.line
	sub.b	#32, d4		; ascii to 0-based

	lea	GlyphWidths, a1
	movez.b	(a1, d4.w), d5	; glyph width in words

	add.w	d4, d4		; *2
	add.w	d4, d4		; *2 (4 bytes per char)
	add.w	d5, d5

.blit	lea	Font, a1
	add.w	d4, a1		; glyph address
	moveq	#16-1, y2	; Y loop
.y	move.l	(a1), (a0)
	add.w	#LINEWIDTH, a0
	add.w	#FONTWIDTH, a1
	dbf	y2, .y

	move.l	a4, a0
	add.w	d5, a0		; advance by glyph width

	bra	.char

.empty	;move.b	#0, TextOdd
.out	move.l	a2, TextDataPtr
;	tst.b	TextOdd
;	bnz	.noe
.noe	rts

.t1	move.b	#0, TextOdd	; type 1=ease in

	lea	BufferText2, a0			; src
	lea	BufferText1, a1			; dest
	bsr	CopyPlayfield			; a0, a1 = bitplane from, to

	lea	BufferText1, a0
	lea	bpls_3, a1
	bsr	SetBPLPtrs
	lea	BufferText1, a0
	add.w	#LINEWIDTH, a0
	lea	bpls_4, a1
	bsr	SetBPLPtrs

	lea	Easing, a0	; allow text transition
	move.l	a0, EasingPtr

.t0	lea	BufferText2, a0	; type 0=clear text
	move.l	#BPLSIZE/4/4-1, d0
	moveq	#0, d1
.clear	move.l	d1, (a0)+
	move.l	d1, (a0)+
	move.l	d1, (a0)+
	move.l	d1, (a0)+
	dbf	d0, .clear
	bra	.line

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

DrawTextSmall:

	lea	EndText, a2
	lea	BufferText1, a0
	add.w	#LINEWIDTH*3, a0

	moveq	#0, d2			; x
	move.l	a0, a5			; remember scanline pos

.line	movez.b	(a2)+, d0		; ascii
	tst.b	d0			; end of line?
	bz	.nl

.char	cmp.b	#255, d0		; all done?
	beq	.out
	sub.b	#32, d0			; ascii->zero based

	push	a0
	lea	FontSmall, a1
	add.w	d0, d0
	add.w	d0, a1			; glyph bitmap
	moveq	#8-1, d1		; y
.y	move.w	(a1), (a0)
	add.w	#LINEWIDTH, a0
	add.w	#FONTWIDTH/2, a1
	dbf	d1, .y
	pop	a0
	add.w	#2, a0
	addq	#1, d2			; x
	bra	.line

.out	rts

.nl	tst.b	d2			; x > 0 ?
	bz	.xy
	move.l	#LINEWIDTH*12,  d2
	bra	.xo
.xy	move.l	#LINEWIDTH*9, d2
.xo	add.l	d2, a5

	move.l	a5, a0			; advance scanlines
	moveq	#0, d2			; x

	bra	.line

; -----------------------------------------------------------------------------

EndScreen:
	tst.b	AtEnd
	bnz	.out

	move.b	#1, AtEnd
	move.l	#0, EasingPtr

	lea	BufferText1, a0
	move.l	#BPLSIZE/4-1, d0	; 2 bitplanes, size in longwords
	bsr	ClearPlayfield

	lea	BufferText1, a0
	lea	bpls_3, a1
	bsr	SetBPLPtrs
	lea	BufferText1, a0
;	add.w	#LINEWIDTH, a0
	lea	bpls_4, a1
	bsr	SetBPLPtrs

	rem>
	ifnd	NOAUDIO
	bsr	StopMusic
	lea	Tune2, a0
	moveq	#1, d0			; PAL
	move.l	#0, a1			; no external samples
	lea	SampleBuffer, a2
	jsr	P61_Init		; d0=0 on return if ok
	endif
	<erem

	lea	PaletteEnd, a0
	move.l	a0, PalPtr
	bsr	SetPalette
	lea	coppal, a0
	move.w	#$FED, 2(a0)		; bg

	lea	EndText, a4
	bsr	DrawTextSmall

.out	rts

********************************************************************************
* Includes
********************************************************************************

	include	"init.asm"			; misc initialization
	;include	"ptplayer.asm"

********************************************************************************
* Data
********************************************************************************

TEXTCOLOR = $C10

	SECTION COPPER, DATA_C

CopperList:	dc.w	FMODE,   $0	; AGA slow fetch mode

		dc.w	DIWSTRT, $2C81	; Display window start (top left)

		if	RESOLUTION=0	; lores 320x256
		dc.w	DIWSTOP, $2CB1	; Display window stop (bottom right)
		dc.w	DDFSTRT, $003B	; Display data fetch start (horiz.)
		endc
		if	RESOLUTION=1	; medres 640x256
		dc.w	DIWSTOP, $2CC1	; Display window stop (bottom right)
		dc.w	DDFSTRT, $003C	; Display data fetch start (horiz.)
		endc

		dc.w	DDFSTOP, $00D0	; Display data fetch stop  (horiz.)

XScroll:	dc.w	BPLCON1, $1	; x scroll
		dc.w	BPLCON2, $0
		;dc.w	BPLCON3, $0C00	; AGA bitplane control

BgModulo:	dc.w	BPL1MOD, 0	; bg modulo
FgModulo:	dc.w	BPL2MOD, 0	; foreground modulo




coppal:		COPPER_SETCOLOR	0, $FFF		; bg color
		COPPER_SETCOLOR	1, $CCC ;$0FF		; line fringe
		COPPER_SETCOLOR	2, $999 ;$F0F		; line fringe
		COPPER_SETCOLOR	3, $000			; line color
		COPPER_SETCOLOR	4, $CBB			; text shadow
		COPPER_SETCOLOR	5, TEXTCOLOR
		COPPER_SETCOLOR	6, TEXTCOLOR
		COPPER_SETCOLOR	7, TEXTCOLOR
		COPPER_SETCOLOR	8, TEXTCOLOR
		COPPER_SETCOLOR	9, TEXTCOLOR
		COPPER_SETCOLOR	10,TEXTCOLOR
		COPPER_SETCOLOR	11,TEXTCOLOR
		COPPER_SETCOLOR	12,TEXTCOLOR		; text
		COPPER_SETCOLOR	13,TEXTCOLOR
		COPPER_SETCOLOR	14,TEXTCOLOR
		COPPER_SETCOLOR	15,TEXTCOLOR

bpls_1:		dc.w 	BPL1PTH, 0, BPL1PTL, 0
bpls_2:		dc.w 	BPL2PTH, 0, BPL2PTL, 0
bpls_3:		dc.w 	BPL3PTH, 0, BPL3PTL, 0
bpls_4:		dc.w 	BPL4PTH, 0, BPL4PTL, 0

;		dc.w	BPLCON0, PLANES_1|DBLPF|COLOR

		if	RESOLUTION=0
		dc.w	BPLCON0, PLANES_4|COLOR
		endc
		if	RESOLUTION=1
		dc.w	BPLCON0, PLANES_4|HIRES|COLOR
		endc

		COPPER_BOTTOM			; -----------------------------

		COPPER_WAIT	32,7
		dc.w BPLCON0, 	$0200		; Screen off

		COPPER_END

CopperData_End:	dc.l	0

; ------------------------------------------------------------------------------

	SECTION CHIP, BSS_C

		ALIGN64

BufferFore1:	blk.b	BPLSIZE
BufferFore2:	blk.b	BPLSIZE

	SECTION CHIP2, BSS_C

		ALIGN64
BufferText1:	blk.b	BPLSIZE
BufferText2:	blk.b	BPLSIZE		; help
BufferBuffer:	blk.b	80*96		; 103 really

;	SECTION SAMPLES, BSS_C

;SampleBuffer:	blk.b	184132;-(1024*10)

; ------------------------------------------------------------------------------

	SECTION LINES, DATA

PalPtr:		dc.l	Palette1

DataListPtr:	dc.l	DataList

DataList:
	dc.l	linedata01, textdata01
	dc.l	linedata05, textdata05
	dc.l	linedata08, textdata08
	dc.l	linedata13, textdata13
	dc.l	linedata16, textdata16
	dc.l	linedata18, textdata18
	dc.l	linedata21, textdata21
	dc.l	linedata24, textdata24
	dc.l	linedata28, textdata28
	dc.l	0

EasingPtr:	dc.l	Easing

WaitCtr:	dc.w	0

Palette1:	; fringe, fringe, linecolor, textcolor
	dc.w	$CCC, $999, $333, $C10	; grayscale
	dc.w	$CD6, $CC9, $309, $1C3	; yellow+green
	dc.w	$0FF, $F0F, $000, $F66	; cga
	dc.w	$9F9, $F9F, $C66, $6CF	; pastel
	dc.w	$FFFF

PaletteEnd:
	dc.w	$EDC, $FDC, $DCB, $69C	; pastel
	dc.w	$FFFF

Easing:		incbin	"data/bounce.dat"
;Easing:	incbin	"data/pics/bounce.dat"

LineData:
linedata01:	incbin	"data/pics/01.dat"
linedata05:	incbin	"data/pics/05.dat"
linedata08:	incbin	"data/pics/08.dat"
linedata13:	incbin	"data/pics/13.dat"
linedata16:	incbin	"data/pics/16.dat"
linedata18:	incbin	"data/pics/18.dat"
linedata21:	incbin	"data/pics/21.dat"
linedata24:	incbin	"data/pics/24.dat"
linedata28:	incbin	"data/pics/28.dat"

screeninfoidx:	dc.b	0
screeninfoval:	dc.b	0
nexttriggery:	dc.b	$FF
screeninfos:
		dc.b	%100		;linedata01:title
		dc.b	%111,170	;linedata05:big bang
		dc.b	%011,233	;linedata08:bull
		dc.b	%111,140	;linedata13:stare
		dc.b	%101		;linedata16:duckhunt
		dc.b	%110,128	;linedata18:president
		dc.b	%101		;linedata21:computer
		dc.b	%101		;linedata24:soul
		dc.b	%101		;linedata28:void
			;abc	new text when... a=finished b=midway c=starting
			; if bit b set, follow with Y value for trigger

		ALIGN32
Font:		incbin	"data/font.raw"
FontSmall:	incbin	"data/fontsmall.raw"

		ALIGN32
TextDataPtr:	dc.l	Text
RandomsPtr:	dc.l	0
Randoms:	dc.b	0,1,2,1, 0,2,1,2, 0,0,1,2, 0,1,2,0
		dc.b	0,0,1,2, 0,1,2,0, 1,2,0,1, 2,0, 255

; !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
;22222221222211222122222222112222222222222122222222222222222
GlyphWidths:	dc.b	1,2,2,2,2,2,2,1,2,2,2,2,1,1,2,2	;  !"#$%&'()*+,-./
		dc.b	2,1,2,2,2,2,2,2,2,2,1,1,2,2,2,2	; 0123456789:;<=>?
		dc.b	2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2	; @ABCDEFGHIJKLMNO
		dc.b	2,2,2,2,2,2,2,2,2,2,2		; PQRSTUVWXYZ
		even

Text:		; 255 for no text this time, or: x, y, type, ascii text, 0
		; type 0=clear screen of text, 1=ease in, 2=just blit on top

; Dogs cannot play the drums ------- But they can use chopsticks

;%100		;linedata01:title
textdata01:	dc.b	23,3,1," THINGS|WE  KNOW|  ABOUT|   DOGS",0

;%111,128	;linedata05:big bang
textdata05:	dc.b	1,8,1,"SINCE THE DAWN|OF BIG BANG|SCIENTISTS HAVE SAID|THAT DOGS EXIST",0
		dc.b	0,0,0,0
		dc.b	6,0,0,"ALSO THEIR HANDS|  ARE KINDA SPIKY",0

;%011		;linedata08:bull
textdata08:	dc.b	1,0,1,"        YOU CAN ACTUALLY|         BREED A BULLDOG|         USING JUST DOGS",0
		dc.b	0,5,2,"YOU  DO  NOT|REQUIRE BULLS",0

;%011		;linedata13:stare
textdata13:	dc.b	1,8,1,"IF YOU STARE|AT A DOG FOR|10 MINUTES|SOMEONE WILL DIE",0
		dc.b	18,0,0,"(PROBABLY)",0
		dc.b	0,0,0,0 ; clear text

;%101		;linedata16:duckhunt
textdata16:	dc.b	0,0,1,"        DOGS  ARE  LIKE|    MADE  FROM  WOLVES|SO THATS WHY THEY HAVE|    A SIMILAR AMOUNT OF|                LIMBS",0
		dc.b	6,0,1,"IF YOU BUY A FAKE|  DOG FROM CHINA|MAKE SURE IT SAYS|   FREE  SHIPPING",0

;%110,128	;linedata18:president
textdata18:	dc.b	1,0,1,"IT'S 2020 AND YOU STILL| CAN'T DOWNLOAD A DOG.",0
		dc.b	1,9,2,"THANKS FOR|NOTHING|MR. PRESIDENT!",0

;%101		;linedata21:computer
textdata21:	dc.b	2,0,1,"DOGS CANNOT USE THE|           COMPUTER.",0
		dc.b	0,9,2,"THEY DON'T|KNOW HOW TO USE|THE MOUSE.",0

;%101		;linedata24:soul
textdata24:	dc.b	4,0,1,"DOG SOULS OPERATE|    IN THE SHADOW| DIMENSION BETWEEN|REALITY AND DREAMS",0
		dc.b	3,1,0,"AND THAT'S TERRIBLE",0

;%101		;linedata28:void
textdata28:	dc.b	8,0,1,"DOG SAY WOOBF",0
		dc.b	4,10,2,"THANKS FOR COMING|   TO MY TED TALK",0

;XXXXXXXXXXZZZZZZZZZZXXXXXXXXXXZZZZZZZZZZ

EndText:
	dc.b	"    THINGS WE KNOW ABOUT DOGS.",0
	dc.b	0
	dc.b	"    BY VOID AT REVISION 2020",0
	dc.b	0
	dc.b	"    CODE, TEXT AND FONTS BY #UKKA",0
	dc.b	"    DRAWINGS BY KISU",0
	dc.b	"    MUSIC BY ASSASSIN",0
	dc.b	"    IDEA AND HELP BY MUZZY/WORST CODERS",0
	dc.b	0
	;	 XXXXXXXXXXZZZZZZZZZZXXXXXXXXXXZZZZZZZZZZ
	dc.b	"A WISE DOGE ONCE SAID, GREETINGS TO...",0
	dc.b	0
	dc.b	"  8BITBUBSY             ARCHYX",0
	dc.b	"  MAZA                  ROSS",0
	dc.b	"  STINGRAY              WUFFE",0
	dc.b	"  CANDLE                DESIRE",0
	dc.b	"  EPHIDRENA             EXEC",0
	dc.b	"  FURRY TRASH GROUP     GHOSTOWN",0
	dc.b	"  IVORY LABS            MOODS PLATEAU",0
	dc.b	"  NAH-KOLOR             NUKLEUS",0
	dc.b	"  REBELS                TEK",0
	dc.b	"  TUSSEJUV RANGERS      WANTED TEAM",0
	dc.b	255

; ------------------------------------------------------------------------------

	SECTION SONG, DATA_C

Tune:		incbin	"data/P61.pwh-hukka5loop"

; ------------------------------------------------------------------------------

	SECTION LINESTRUCTS, BSS

		ALIGN32

		rsreset	; linedata struct
bitplaneptr:	rs.l	1
linedataptr:	rs.l	1
line_x:		rs.w	1
line_y:		rs.w	1
line_dx:	rs.w	1
line_dy:	rs.w	1
line_sx:	rs.w	1
line_sy:	rs.w	1
line_error:	rs.w	1
line_length:	rs.w	1
line_destx:	rs.w	1
line_desty:	rs.w	1
randomness:	rs.b	1	; 1=want to randomly vary line coords?
line_done:	rs.b	1	; 1=current drawing is fully drawn
		even
linestructsize:	rs.w	1

		ALIGN32
TempPal:	ds.w	5

LineData1:	ds.b	linestructsize
LineData2:	ds.b	linestructsize
		even

AtEnd:		ds.b	1
QuitFlag:	ds.b	1

DblBufCtr:	ds.b	1
DblBufIdx:	ds.b	1

TextPos:	ds.b	2
TextOdd:	ds.b	1

csR:		ds.b	1
csG:		ds.b	1
csB:		ds.b	1
cdR:		ds.b	1
cdG:		ds.b	1
cdB:		ds.b	1

; ------------------------------------------------------------------------------
