; -------------------------------------------------------------------------------------------------

;	include	"player.asm"

	xdef	_VersionString
	xref	_sigtask			; current task
	xref	_DosBase
	xref	_GfxBase

	xref	_LVOOwnBlitter
	xref	_LVODisownBlitter

	xref	P61_Init
	xref	P61_Music
	xref	P61_End

	xref	SystemAddVBlankRoutine
	xref	SystemRemoveVBlankRoutine

; -------------------------------------------------------------------------------------------------

_VersionString:	dc.b	0,"$VER: exec 2015",0	; shown when quitting
		even

; -------------------------------------------------------------------------------------------------
; Program startup; setup hardware
;
ProgramStartup:
	move.b	#0, Started

	bsr	KillSpriteDMA			; disable sprite DMA

	;move.l	_GfxBase, a6
	;jsr	_LVOOwnBlitter(a6)		; own Blitter

	bsr	InitBitplanes

	move.l	#CopperListSrc, COP1LCH+CUSTOM	; initialize Copperlist

	bsr	Waves_Init

	bsr	TextWriter_Init

	lea	VBlank(pc), a0			; init VBL interrupt
	bsr	SystemAddVBlankRoutine

	bsr	InitMusic

	rts

; -------------------------------------------------------------------------------------------------
; Restore system and shutdown program
;
ProgramShutdown:
	bsr	SystemRemoveVBlankRoutine

	;move.l   _GfxBase, a6
	;jsr	_LVODisownBlitter(a6)		; give blitter back

	move.w	#$8020, DMACON+CUSTOM		; re-enable sprite DMA

	moveq	#0, d0				; errorcode=0
	rts

; -------------------------------------------------------------------------------------------------
; Init bitplane pointers
;
InitBitplanes:
	lea	BufferBack, a0
	lea	bplWave, a1
	bsr	SetBPLPtrs

	lea	BufferFore, a0
	lea	bplText, a1
	bsr	SetBPLPtrs

	lea	BufferFore, a0
	add.l	#80, a0
	lea	bplShadow, a1
	bsr	SetBPLPtrs

	lea	BufferBack, a0
	lea	BackY, a1
	zero	d0
	move.b	#255, d0
.y	move.l	a0, (a1)+
	add.l	#LINEWIDTH, a0
	dbf	d0, .y

	lea	lines, a0
	lea	linesY, a1
	zero	d0
	move.b	#160-1, d0
.yl	move.l	a0, (a1)+
	add.l	#(320/8), a0
	dbf	d0, .yl

;	lea	BufferBack, a0
;	bsr	ClearPlayfield
	lea	BufferFore, a0

; -------------------------------------------------------------------------------------------------
; Clear a bitplane
;
ClearPlayfield:				; a0=address
	move.l	#BPLSIZE/4-1, d0	; 1 bitplane, size in longwords
	moveq	#0, d1
.clear	move.l	d1, (a0)+
	dbf	d0, .clear
	rts

; -------------------------------------------------------------------------------------------------
