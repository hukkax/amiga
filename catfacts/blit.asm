; -------------------------------------------------------------------------------------------------
; a0=destination, d0=coords

ClearTile:
	push	d0/a0
	and.l	#$FF, d0
	add.w	d0, d0
	add.w	d0, d0				; *4
	add.l	d0, a0
	move.l	(a0), a0

	moveq	#16*PLANES-1, d0
.clear	move.w	#0, (a0)
	add.l	#LINEWIDTH, a0
	dbf	d0, .clear
	pop	d0/a0
	rts

; -------------------------------------------------------------------------------------------------
; a0=destination, d1=coords, d2=tileid, a6=CUSTOM | destroys d1, d2, a0
;
BlitTile:
	add.w	d1, d1
	add.w	d1, d1				; *4

	add.w	d2, d2
	add.w	d2, d2				; *4

	WaitBlitter

	add.l	d1, a0
	move.l	(a0), BLTDPTH(a6)		; destination on screen

	lea	TileXY, a0
	add.l	d2, a0
	move.l	(a0), BLTAPTH(a6)		; source buffer

	move.l	#$09F00000, BLTCON0(a6)		; control words=straight copy A to D
	move.l	#$FFFFFFFF, BLTAFWM(a6)		; no masking
	move.w	#LINEWIDTH-1, BLTAMOD(a6)	; source modulo
	move.w	#LINEWIDTH-1, BLTDMOD(a6)	; destination modulo
	move.w	#(16*PLANES)*64+1, BLTSIZE(a6)	; blit size h=16*PLANES, w=16 px/1 word
	rts

; -------------------------------------------------------------------------------------------------
; d0=x, d1=y, d2=tileid
;
BlitTileXY:
	asl.b	#4, d1
	or.b	d0, d1
	bra	BlitTile

; -------------------------------------------------------------------------------------------------
; Clear the 3 bitplanes of a screen playfield (BufferFore/BufferBack)
;
ClearPlayfield:				; a0=address
	pushr	d0-d1 ;/a0
	move.l	#BPLSIZE/4*3-1, d0	; 3 bitplanes, size in longwords
	moveq	#0, d1
.clear	move.l	d1, (a0)+
	dbf	d0, .clear
	popr
	rts

; -------------------------------------------------------------------------------------------------
