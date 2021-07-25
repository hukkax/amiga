	; sync to module

	; 1 = syllable
	; F = snare
	; E = kick
	; B = kick + syllable
	; C = credits part
	; D = intermission
	; A = end

;	ifz.b	SnareCtr, .sync2
;	bsr	Sync_Snare\.DoSnare

.sync2	move.b	P61_E8+1, d0		; sync event happened?
	bnz	.got			; yes
.out	rts

.got	move.w	#0, P61_E8		; acknowledge

	cmp.b	#$E, d0			; kickdrum hit?
	beq	Sync_Kick		; 

	cmp.b	#$F, d0			; snare hit?
	beq	Sync_Snare		;

	cmp.b	#$1, d0			; syllable?
	beq	Sync_Syllable		;

	cmp.b	#$B, d0			; kick + syllable?
	beq	Sync_80b		;

	cmp.b	#$D, d0			; crotch lasers?
	beq	Sync_Laser		;

	cmp.b	#$C, d0			; credits part?
	beq	Sync_Credits		;

	cmp.b	#$A, d0			; quit?
	beq	SetQuit			;

	rts

Sync_Syllable:
	cmp.b	#PART_CREDITS, CurrentPart
	beq	.out

	bra	DrawText
.out	rts

; --------------------------------------- kick + syllable
Sync_80b:
	cmp.b	#PART_CREDITS, CurrentPart
	beq	.out

	bsr	Sync_Kick		; process kick
	bra	DrawText		; draw next syllable
.out	rts

; --------------------------------------- kickdrum hit
Sync_Kick:
	cmp.b	#PART_CREDITS, CurrentPart
	beq	.out

	move.b	#%10000, SnareMask
	movez.b	#8, d0			; # of frames (words) in dither bitmap
	bra	Sync_Snare\.sn2
.out	rts

; --------------------------------------- snare hit
Sync_Snare:
	cmp.b	#PART_CREDITS, CurrentPart
	beq	.greet

.sn1	move.b	#%1110000, SnareMask
	movez.b	#16, d0			; # of frames (words) in dither bitmap

.sn2	move.b	d0, SnareCtr
	lea	Dither, a0
	move.l	a0, SnarePtr
	bra	.go

.DoSnare
	movez.b	SnareCtr, d0		; frame counter
	move.l	SnarePtr, a0		; dither bits
.go	subq.b	#1, d0
	move.b	d0, SnareCtr

	ifz.b	d0, .done

	; rept	20		; 20*4 words
	; dc.w	BPLCON1, $0	; 2 words
	; COPPER_WAIT_NEXT	; 2 words

	move.w	(a0)+, d1		; get bits for frame
	move.l	a0, SnarePtr

.go2	lea	scroll1, a1
	lea	scroll2, a2
	movez.b	SnareMask, d3
	movez.b	#20-1, d0		; loop counter

.y	move.w	d1, d2
	and.w	d3, d2
	ror.w	#1, d1			; next bit

	move.w	d2, 2(a1)
	move.w	d2, 2(a2)
	addq.l	#8, a1
	addq.l	#8, a2

	dbf	d0, .y
	rts

.done	zero	d1			; reset scroll regs when done
	bra	.go2


.greet	bsr	DrawText
	bra	.sn1

; --------------------------------------- shoot crotch lasers
Sync_Laser:
	pushall

	lea	laserpal+2, a1		; COPPER_SETCOLOR 1, $999/$F00
	move.w	#$F00, (a1)

	bsr	ClearForeground
	
	move.b	#15, laserpalctr
	bsr	SetLaserPal

	lea	laser_ctr, a0		; PEW!
	move.b	(a0), d0
	addq	#1, d0
	move.b	d0, (a0)
	bsr	DrawText

	lea	laser_dest, a0
	movez.b	(a0)+, d2		; X2
	movez.b	(a0)+, d3		; Y2

	move.l	laser_ptr, a0
	ifnz.l	a0, .ok
	lea	laser_tab, a0

.ok	zero	d0			; X1
	movez.b	(a0)+, d1		; Y1
	move.l	a0, laser_ptr

	lea	CUSTOM+2, a6		; $DFF002
	lea	BufferFore1, a0		; PlanePtr
	zero	d6			; Line Mode0=normal
	move.l	#LINEWIDTH, d4		; PlaneWidth

	pushall
	bsr	DrawLine
	popall
	addq	#1, d1
	addq	#1, d3
	bsr	DrawLine

	popall
	rts

SetLaserPal:
	lea	laserpalctr, a0
	movez.b	(a0), d0
	bz	.out

	lea	laserpal, a1
	add.b	d0, d0

	sub.b	#1, (a0)
	bz	.clear

	lea	laserpaltab, a0
	add.l	d0, a0
	move.w	(a0), 2(a1)
.out	rts

.clear	move.w	#$999, 2(a1)
	bra	ClearForeground

; --------------------------------------- credits part
Sync_Credits:
	cmp.b	#PART_CREDITS, CurrentPart
	beq	.out

.init	move.b	#PART_CREDITS, CurrentPart

	rem
		lea	end_credits, a0
		bsr	InitTextWriter
.draw		bsr	DrawText
		move.l	TextPtr, a0		; more text?
		bnz	.draw			; keep drawing
	erem

.draw	bsr	ClearForeground

	lea	EndPic, a0		; 256*160
	lea	BufferFore1, a1
	lea	BufferFore2, a2
	movez.w	#(LINEWIDTH*30)+4, d1
	add.l	d1, a1
	add.l	d1, a2

	move.w	#160-1, d1
.y	move.w	#8-1, d0
.x	move.l	(a0)+, d2
	move.l	d2, (a1)+
	move.l	d2, (a2)+
	dbf	d0, .x
	addq.l	#8, a1
	addq.l	#8, a2
	dbf	d1, .y

	lea	end_greets, a0
	bsr	InitTextWriter

	move.b	#img_end, ArtNum
	bsr	DrawText\.image

.out	rts
