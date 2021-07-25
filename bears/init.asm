********************************************************************************
*initialization routines
********************************************************************************

	SECTION CODE

; ------------------------------------------------------------------------------
; Program startup; setup hardware
;
ProgramStartup:

	bsr	KillSpriteDMA
	bra	InitBitplanes

; ------------------------------------------------------------------------------
; Restore system and shutdown program
;
ProgramShutdown:
	lea	CUSTOM, a6

	ifd	VBLANK
	bsr	SystemRemoveVBlankRoutine
	endif

	ifnd	NOAUDIO
	bsr	StopMusic
	endif

	;move.l   _GfxBase, a6
	;jsr	_LVODisownBlitter(a6)		; give blitter back

	move.w	#$8020, DMACON+CUSTOM		; re-enable sprite DMA

	zero	d0				; errorcode=0
	rts

; ------------------------------------------------------------------------------
; Init bitplane pointers

InitBitplanes:

	lea	BufferFore1, a0
	lea	bpls_1, a1
	bsr	SetBPLPtrs

	lea	BufferFore2, a0
	lea	bpls_3, a1
	bsr	SetBPLPtrs

	lea	BufferBack3, a0
	lea	bpls_2, a1
	bsr	SetBPLPtrs

	lea	BufferBack4, a0
	lea	bpls_4, a1
	bsr	SetBPLPtrs

	lea	Noise, a0
	lea	bpls_Noise, a1
	bsr	SetBPLPtrs

	lea	BufferFore1, a0
	move.l	#BPLSIZE/4-1, d0	; 2 bitplanes, size in longwords
;	bsr	ClearPlayfield

; ------------------------------------------------------------------------------
; Clear a bitplane
;
ClearPlayfield:				; a0=address, d0=size in longwords
	moveq	#0, d1
.clear	move.l	d1, (a0)+
	dbf	d0, .clear
	rts

; cpu clear. screen 320x256			; a0=bitplane
;
FastClear:
	move.l	a0, a3
	add.l	#40*256, a3			; end of screen
	move.w	#256-1, d7			; height
	movem.l	.clrbuf(pc), d0-d6/a0-a2	; 10.L=40 bytes
.clear	movem.l	d0-d6/a0-a2, -(a3)		; fill one line with 0
	dbf	d7, .clear
	rts

.clrbuf	blk.b 40 ; 1 line
; or 14 with movem.l d0-d6/a0-a6 and complete with one movem.l d0-d6/a0-a4 

; ------------------------------------------------------------------------------
; Duplicate a bitplane
;
	rem
CopyPlayfield:				; a0, a1 = bitplane from, to
	push	d0-d1
	move.l	#BPLSIZE/4-1, d0	; 1 bitplane, size in longwords
	moveq	#0, d1
.clear	move.l	(a0)+, (a1)+
	dbf	d0, .clear
	pop	d0-d1
	rts
	erem

; ------------------------------------------------------------------------------
; Copy a bitplane, clearing the original
;
	rem
MovePlayfield:				; a0, a1 = bitplane from, to
	push	d0-d1
	move.l	#BPLSIZE/4-1, d0	; 1 bitplane, size in longwords
	moveq	#0, d1
.clear	move.l	(a0), (a1)+
	move.l	#0, (a0)+
	dbf	d0, .clear
	pop	d0-d1
	rts
	erem

; ------------------------------------------------------------------------------
