********************************************************************************
* transition.asm by hukka 2021
********************************************************************************

	SECTION CODE

transw = 20
transh = 16
transsize = transw*transh

; ------------------------------------------------------------------------------
; a0 = transition tilemap

InitTransition:
	pushr	d0-d2/a0-a1

	move.l	a0, transptr

	move.w	#transsize-1, d0
	zero	d2			; highest value (=frame count)
.copy	move.b	(a0)+, d1
	cmp.b	d2, d1			; higher than current max?
	bls	.next			; no
	move.b	d1, d2
.next	dbf	d0, .copy

	move.b	d2, transframecnt
	move.b	d2, transframe

	popr
	rts

; ------------------------------------------------------------------------------
; a0 = source bitplane
; a1 = desination bitplane

RenderTransition:
	move.b	transframe, d5		; d5 = tile ID we want this iteration
	cmp.b	#-1, d5
	beq	.out

	move.l	transptr, a2

	move.l	#LINEWIDTH*(transh-1), d4	; tile height scanline skip
	move.l	#LINEWIDTH, d6		; bytes per scanline
	move.w	#transh-1, d2
.ty	move.w	#transw-1, d3

.tx	move.b	(a2)+, d0		; d0 = tile
	cmp.b	d0, d5			; want to draw this tile?
	bne	.ncol			; no

	push	a0-a1
	rept	16
	move.w	(a0), (a1)
	add.l	d6, a0			; next scanline
	add.l	d6, a1
	endr
	pop	a0-a1

.ncol	addq.l	#2, a0
	addq.l	#2, a1
	dbf	d3, .tx

.nrow	add.l	d4, a0			; advance to
	add.l	d4, a1			; scanline for next tile row
	dbf	d2, .ty
.out	rts

AdvanceTransition:
	sub.b	#1, transframe
	rts

********************************************************************************

	SECTION STUFF, BSS

		even
transptr:	ds.l	1
transframe:	ds.b	1
transframecnt:	ds.b	1

; ------------------------------------------------------------------------------

	SECTION PICS, DATA

	even
tr1:	incbin	"transitions/circle.raw"
tr2:	incbin	"transitions/diag3.raw"
tr3:	incbin	"transitions/diag1.raw"
tr4:	incbin	"transitions/diag2.raw"
tr5:	incbin	"transitions/rand.raw"

; ------------------------------------------------------------------------------
