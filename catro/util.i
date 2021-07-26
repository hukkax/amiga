********************************************************************************
* Utility functions

	ifnd inc_util
inc_util = 1

********************************************************************************
* Branching

bz	macro						; branch if zero
	beq	\1
	endm

bnz	macro						; branch if nonzero
	bne	\1
	endm

ifz	macro						; branch if param1 = 0
	ifc	\0, l
	cmp.l	#0, \1
	else
	tst.\0	\1
	endif
	bz	\2
	endm

ifnz	macro						; branch if param1 <> 0
	ifc	\0, l
	cmp.l	#0, \1
	else
	tst.\0	\1
	endif
	bnz	\2
	endm

********************************************************************************
* Data

	macro	STRING
		dc.b	\1
		dc.b	0, 0
	endm

********************************************************************************

movez	macro
	moveq	#0, \2
	move.\0	\1, \2
	endm

zero	macro
	moveq	#0, \1
	endm

********************************************************************************
* Data alignment

	macro	ALIGN64
	cnop	0,8
	endm
	macro	ALIGN32
	cnop	0,4
	endm
	macro	ALIGN16
	even
	endm

	macro	align64
	cnop	0,8
	endm
	macro	align32
	cnop	0,4
	endm
	macro	align16
	even
	endm

********************************************************************************
* Push/pop registers

push	macro
	movem.l	\1, -(sp)
	endm

pop	macro
	movem.l	(sp)+, \1
	endm

pushr	macro
.Regs	reg	\1
	push	.Regs
	endm

popr	macro
	pop	.Regs
	endm

pushall	macro
	pushr	d0-d7/a0-a6
	endm

popall	macro
	popr	d0-d7/a0-a6
	endm

********************************************************************************

WaitForVBL	macro
	push	d0
	WaitLine 286
	pop	d0
endm

********************************************************************************
* Nybbles in bytes

	macro	UnpackNybbles		; 1=AB, 2:=A, 3:=B
	moveq	#0, \2
	moveq	#0, \3
	move.b	\1, \3			; right nybble
	and.b	#$0F, \3
	move.b	\1, \2			; left nybble
	asr.b	#4, \2
	and.b	#$0F, \2
	endm

	macro	PackNybbles		; 1=A, 2=B, 3:=AB
	move.b	\1, \3
	asl.b	#4, \3
	or.b	\2, \3
	endm

********************************************************************************
* Memory allocation

ALLOC	macro
	movea.l	EXEC, a6
	move.l	#\1, d0
	move.l	#\2, d1
	jsr	AllocMem(a6)
	tst.l	d0
	beq	ErrorMem
	move.l	d0, \3(a5)
	endm

FREE	macro
	movea.l	EXEC, a6
	move.l	#\1, d0
	movea.l	\2(a5), a1
	jsr	FreeMem(a6)
	endm

********************************************************************************

SetBPLPtrs:	; a0 = address of raw image data
		; a1 = where to write bitplane pointers (in copperlist)
	push	d0
	move.l	a0, d0			; image data address to d0
	move.w	d0, 6(a1)		; insert bitplane ptr into copperlist
	swap	d0
	move.w	d0, 2(a1)
	pop	d0
	rts

********************************************************************************

KillSpriteDMA:				; disable sprite DMA after vblank
	move.l	VPOSR+CUSTOM, d0
	and.l	#$1FF00, d0
	cmp.l	#303<<8, d0
	bne.b	KillSpriteDMA
	move.w	#$20, DMACON+CUSTOM
	rts

********************************************************************************

KillFloppy:
	ori.b	#$f8, $bfd100		; floppy motor off
	andi.b	#$87, $bfd100
	ori.b	#$78, $bfd100
	rts

********************************************************************************

WaitFrames:				; d3 = # of frames to wait
	push	d0
.wait	WaitForVBL
	dbf	d3, .wait
	pop	d0
	rts

********************************************************************************

	endif
