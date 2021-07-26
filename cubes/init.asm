********************************************************************************
* Diskmag engine (de-)initialization routines
********************************************************************************

	xref	_sigtask			; current task
	xref	_DosBase
	xref	_GfxBase
	;xref	_LVOOwnBlitter
	;xref	_LVODisownBlitter
	xref	SystemAddVBlankRoutine
	xref	SystemRemoveVBlankRoutine
	;xref	P61_Init
	;xref	P61_Music
	;xref	P61_End

	SECTION CODE

; ------------------------------------------------------------------------------
; Program startup; setup hardware
;
ProgramStartup:

	bsr	KillSpriteDMA
	bsr	InitBitplanes
	rts

; ------------------------------------------------------------------------------
; Restore system and shutdown program
;
ProgramShutdown:
	ifd	VBLANK
	bsr	SystemRemoveVBlankRoutine
	endif

	ifnd	NOAUDIO
	bsr	StopMusic
	endif

	;move.l   _GfxBase, a6
	;jsr	_LVODisownBlitter(a6)		; give blitter back

	move.w	#$8020, DMACON+CUSTOM		; re-enable sprite DMA

	moveq	#0, d0				; errorcode=0
	rts

; ------------------------------------------------------------------------------
; Init bitplane pointers

InitBitplanes:

	lea	BufferFore, a0
	add.w	#GRADSIZE, a0
	;lea	Logo, a0

	lea	bpl_Fg1, a1
	bsr	SetBPLPtrs
	add.w	#40, a0
	lea	bpl_Fg2, a1
	bsr	SetBPLPtrs

	lea	trib1, a1
	bsr	SetCubePtrs

	lea	trib2, a1
	bsr	SetCubePtrs

	lea	trib3, a1
	bsr	SetCubePtrs

	lea	trib4, a1
	bsr	SetCubePtrs

	lea	trib5, a1
	bsr	SetCubePtrs

	lea	trib6, a1
	bsr	SetCubePtrs

	lea	trib7, a1
	bsr	SetCubePtrs

	lea	BufferFore, a0
	move.l	#BPLSIZE/2-1, d0	; 2 bitplanes, size in longwords
	bra	ClearPlayfield


SetCubePtrs:
	rem>
	lea	CubeBuf, a0
	bsr	SetBPLPtrs
	add.w	#LINEWIDTH, a0
	add.w	#8, a1
	bra	SetBPLPtrs
	<erem

	lea	CubeData, a0
	bsr	SetBPLPtrs
	add.w	#8, a1
	add.l	#TRISIZE+LINEWIDTH, a0
	bra	SetBPLPtrs

; ------------------------------------------------------------------------------
; Clear a bitplane
;
ClearPlayfield:				; a0=address, d0=size in longwords
	moveq	#0, d1
.clear	move.l	d1, (a0)+
	dbf	d0, .clear
	rts

; ------------------------------------------------------------------------------
; Duplicate a bitplane
;
	rem>
CopyPlayfield:				; a0, a1 = bitplane from, to
	push	d0-d1
	move.l	#BPLSIZE/4-1, d0	; 1 bitplane, size in longwords
	moveq	#0, d1
.clear	move.l	(a0)+, (a1)+
	dbf	d0, .clear
	pop	d0-d1
	rts
	<erem

; ------------------------------------------------------------------------------
; Copy a bitplane, clearing the original
;
	rem>
MovePlayfield:				; a0, a1 = bitplane from, to
	push	d0-d1
	move.l	#BPLSIZE/4-1, d0	; 1 bitplane, size in longwords
	moveq	#0, d1
.clear	move.l	(a0), (a1)+
	move.l	#0, (a0)+
	dbf	d0, .clear
	pop	d0-d1
	rts
	<erem

; ------------------------------------------------------------------------------
