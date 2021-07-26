***************************************************************************************************
* aaltoefekti!
* hukka & wuffe / exec 2014-2015
* vasm


FXCOLOR				; use raster colors in wave effect
;FXFADES

;DEBUG				; debug mode
;QUICKSTART			; for testing


	machine mc68000
	opt	a+,o+,od+,og+,oj+,ol+,os+,ox+,d-,p+,ow-,w+

	ifnd	DEBUG		; hide optimization messages from assembler
	opt	ow-, d-
	endif

	SECTION	CODE, CODE

	xdef	_main

	include	"exec_lib.i"
	include	"dos/dos_lib.i"
	include	"custom.i"
	include	"util.i"

***************************************************************************************************
* Macros

	macro	BGColor
	ifd	DEBUG
	move.w	#\1, COLOR00+CUSTOM
	endif
	endm

***************************************************************************************************
* Constants

HEIGHT		= 255
PLANES		= 2			; bitplanes per playfield
SCREENSIZE_X	= 320
SCREENSIZE_Y	= 256
SCREENSIZE_XB	= SCREENSIZE_X/8
LINEWIDTH	= SCREENSIZE_XB
BPLSIZE		= SCREENSIZE_X*SCREENSIZE_Y/8

***************************************************************************************************
* Initialization

_main:	bsr	ProgramStartup

	move.b	#1, Started

	ifd	FXFADES
	lea	ColorFades, a0
	move.l	a0, ColorPtr
	move.b	#0, ColorCtr
	endif

MainLoop:
	btst	#6, CIA_A			; quit if left mouse clicked
	bz	Quit

	move.b	SDR_A, d6
	cmp.b	#$75, d6
	bne	MainLoop

Quit:	lea	CUSTOM, a6
	bsr	StopMusic
	bra	ProgramShutdown

***************************************************************************************************
* VBlank interrupt
***************************************************************************************************

VBlank:
	pushall
	lea	CUSTOM, a6

;	move.w	#$060, COLOR00+CUSTOM
	bsr	Waves
;	move.w	#$600, COLOR00+CUSTOM
	bsr	TextWriter
;	move.w	#$0, COLOR00+CUSTOM

.done	popall
	moveq	#0, d0
	rts

***************************************************************************************************

	include	"player.asm"			; P61 module player
	include	"init.asm"			; data/screen/game initialization
	include	"waves.asm"			; wave effect
	include	"textwriter.asm"		; textwriter

***************************************************************************************************

	SECTION	DATA_C

;Song:	incbin	"data/P61.patteris"
Song:	incbin	"data/P61.paroni"

	include	"copperlist.asm"

; -------------------------------------------------------------------------------------------------

	SECTION PLANES, BSS_C

BufferBack:	;blk.b	BPLSIZE*1		; playfield for wave effect
BufferFore:	blk.b	BPLSIZE*1		; playfield for text writer

;BufSamples:	blk.b	4826+2 			; buffer for decrunched samples
BufSamples:	blk.b	10512+2 			; buffer for decrunched samples

Started:	dc.b	0
		even

***************************************************************************************************
