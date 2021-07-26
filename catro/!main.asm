********************************************************************************
* compofiller for assembly 2019
* by hukka of void/exec/svenonacid 2019
* vasm
********************************************************************************

;NOAUDIO

	machine	mc68000
	opt	a+,o+,od+,og+,oj+,ol+,os+,ox+,d-,ow-,w+

	ifnd	DEBUG		; hide optimization messages from assembler
	opt	ow-, d-
	endif

	SECTION	CODE

	xdef	_main

	bra	_InternalMain

	include	"exec_lib.i"
	include	"dos/dos_lib.i"
	include	"custom.i"
	include	"util.i"

;	include	"startup.asm"
	include	"custom.i"
	include	"util.i"
	include	"player.asm"

********************************************************************************
* Constants
********************************************************************************

HEIGHT_ICONS	= 26				; page icons panel height
HEIGHT_PANEL	= 9				; scroller panel height
HEIGHT_PANELS	= HEIGHT_ICONS+HEIGHT_PANEL+1	; total height of navi panels
HEIGHT_CONTENT	= 256-HEIGHT_PANELS		; text content area height 220
PLANES_PANEL	= 2				; # of bitplanes
PLANES_CONTENT	= 2				; # of bitplanes
SCREENSIZE_X	= 320
SCREENSIZE_Y	= HEIGHT_CONTENT		; =220
LINEHEIGHT	= 10
CHARSPERROW	= SCREENSIZE_X/8
TEXTLINES	= SCREENSIZE_Y/LINEHEIGHT	; =22
LINEWIDTH	= CHARSPERROW
BPLSIZE		= SCREENSIZE_X*SCREENSIZE_Y/8	; 
GLYPHSINFONT	= 760/8
MB_Left		= 1
MB_Right	= 2
SCREEN_TOP 	= 45

********************************************************************************
* Initialization
********************************************************************************

_main:
	bsr	ProgramStartup

	ifnd	NOINTRO
	bsr	IntroScreen
	endif

	; init magazine engine
	;
;	bsr	InitFonts

	moveq	#0, d0
InitMag:
	bsr	ClearPlayFields

	move.b	#0, Blitting
	move.b	#0, FadeDir
	move.l	#0, YSlidePtr
	move.w	#-1, CurrentPage
	bsr	SwitchPage

	move.l	#CopperList, COP1LCH+CUSTOM	; initialize Copperlist

;	bsr	InitMouseCursor

	lea	MouseSpriteData, a0		; mouse sprite
	lea	sprptr0, a1
	bsr	SetBPLPtrs
	lea	NullSprite, a0			; empty dummy sprite
	lea	sprptr1, a1
	bsr	SetBPLPtrs
	lea	sprptr2, a1
	bsr	SetBPLPtrs
	lea	sprptr3, a1
	bsr	SetBPLPtrs
	lea	sprptr4, a1
	bsr	SetBPLPtrs
	lea	sprptr5, a1
	bsr	SetBPLPtrs
	lea	sprptr6, a1
	bsr	SetBPLPtrs
	lea	sprptr7, a1
	bsr	SetBPLPtrs

	lea	Tooltips\.ti00, a0
	bsr	UpdateTooltip

	bsr	WaitInputRelease

	lea	VBlank(pc), a0			; init VBL interrupt
	bsr	SystemAddVBlankRoutine

	move.w	#$83A0,	DMACON+CUSTOM		; enable sprite DMA

	bsr	ChangeMusic1

	move.b	#1, Started

MainLoop:
	cmp.b	#1, InVBL
	beq	MainLoop

	cmp.b	#KEYCODE_ESC, KBPrevKey		; quit if Esc pressed
	bne	MainLoop

Quit:	lea	CUSTOM, a6
	bra	ProgramShutdown
	rts

********************************************************************************
* VBlank interrupt
********************************************************************************

VBlank:
	pushall
	move.b	#1, InVBL

	bsr	HandleInput

	ifnz.b	Blitting, .done

.go	bsr	Animation

.done	popall
	move.b	#0, InVBL
	zero	d0
Rts:	rts


InVBL:	dc.b	0
Started:dc.b	0
	align32

********************************************************************************
* Includes
********************************************************************************

;	include	"player.asm"			; P61 module replay
	include	"init.asm"			; misc initialization
	include	"startup.asm"			; startup/shutdown code

	include	"intro.asm"			; intro screen

********************************************************************************
* Data
********************************************************************************

	SECTION TITLE, DATA_C

TitlePic:	incbin	"data/title.raw"		; 320x256x5p, 50 KB
Song00:		incbin	"data/music/p61.owo"

		ALIGN32

Font:		blk.b	8, 0					; space
		incbin	"data/font.raw"

; ------------------------------------------------------------------------------

	SECTION PLANES, BSS_C

		ALIGN64
		blk.b	LINEWIDTH
BufferFore1:	blk.b	BPLSIZE
		ALIGN64
BufferFore2:	blk.b	BPLSIZE
		ALIGN64

SampleBuffer:	blk.b	6730

; ------------------------------------------------------------------------------
