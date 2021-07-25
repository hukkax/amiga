*******************************************************************************
* Blitter drawing functions
*******************************************************************************

DL_Width	= 40

	SECTION	CODE

* WaitBlitter: Wait for Blitter to finish

WaitBlitter:
;	lea	CUSTOM, a6
	tst	DMACONR(a6)		; for compatibility
.wait	btst	#6, DMACONR(a6)
	bne.s	.wait
	rts

*******************************************************************************
* Clear: Use Blitter to clear a 320*256 bitplane
	REM
Clear:	btst	#14, CUSTOM+DMACONR
	bne.b	Clear
	move.l	a0, CUSTOM+BLTDPTH
	move.w	#$0100, CUSTOM+BLTCON0
	clr.w	CUSTOM+BLTCON1
	clr.l	CUSTOM+BLTAFWM
	move.w	#0, CUSTOM+BLTDMOD
	move.w	#(240*64)+20, CUSTOM+BLTSIZE
	rts
	EREM

;	REM
safety	= 0	; high safety takes more raster

Clear:	moveq	#0,d1
	move.l	d1,d2
	move.l	d1,d3
	move.l	a0, d0
	move.w	xmin(pc),d1	
	move.w	xmax(pc),d2
	sub.w	d1,d2				; d2 = clear width in pixels
	lsr.w	#3,d1				; d1 = left pixels in bytes
	ext.l	d1
	subq.w	#1,d1
	add.l	d1,d0				; d0 = start address
	lsr.w	#4,d2				; d2 = clear width in words
	add.w	#3,d2				; for safety	
	move.w	d2,d1
	mulu	#2,d1				; d1 = clear width in bytes
	move.w	#DL_Width,d3
	sub.w	d1,d3				; d3 = clear modulo
	move.w	ymin(pc),d4
;	sub.w	#safety,d4
	move.w	ymax(pc),d5
	sub.w	d4,d5				; height
;	add.w	#safety,d5
	mulu	#DL_Width,d4
	ext.l	d4
	add.l	d4,d0
	lsl.w	#6,d5
	or.w	d5,d2				; height and width

.wait1	btst	#14, DMACONR(a6)
	bne.s	.wait1
	move.w	#$0100, BLTCON0(a6)
	move.w	#$0000, BLTCON1(a6)
	move.l	#$ffffffff, BLTAFWM(a6)
	move.l	d0, BLTDPTH(a6)
	move.w	d3, BLTDMOD(a6)
	move.w	d2, BLTSIZE(a6)
	add.l	#10240,d0			; point on next plane
.wait2	btst	#14, DMACONR(a6)
	bne.s	.wait2
	rts
;	EREM

*******************************************************************************
* Fill: Fill a polygon with Blitter

Fill:	;lea	CUSTOM, a6
	move.l	a0, d0				; a0 = bitplane ptr
	moveq	#0, d1
	move.l	d1, d2
	move.l	d1, d3
	move.w	ymax(pc), d1
	mulu	#40, d1	
	add.l	d1, d0				; d0 = bottom vertical line
	move.w	#320, d2
	sub.w	xmax(pc), d2
	lsr.w	#3, d2				; 16 pixel precision
	ext.l	d2
	sub.l	d2, d0				; d0 = start address 
	move.w	xmax(pc), d1
	sub.w	xmin(pc), d1			; d1 = fill width in pixels
	lsr.w	#4, d1				; d1 = fill width in words
	addq.w	#3, d1
	move.w	d1, d2
	move.w	#DL_Width, d3			; full screen modulo
	add.w	d2, d2				; d2 = fill width in bytes
	sub.w	d2, d3				; d3 = fill modulo
	move.w	ymax(pc), d2
	sub.w	ymin(pc), d2			; d2 = height in pixels
	lsl.w	#6, d2				; place height on right bits
	or.w	d2, d1				; d1 = fill height and width
.wait1	btst	#14, DMACONR(a6)
	bne.b	.wait1				; wait
	move.l	d0, BLTAPTH(a6)
	move.l	d0, BLTDPTH(a6)
	move.w	#$09f0, BLTCON0(a6)		; minterm d=a
	move.w	#%00010010, BLTCON1(a6)		; exclusive fill (bit 4)
	move.l	#$ffffffff, BLTAFWM(a6)		; masks
	move.w	d3, BLTAMOD(a6)			; A modulo
	move.w	d3, BLTDMOD(a6)			; D modulo
	move.w	d1, BLTSIZE(a6)			; fill height and start address
.wait2	btst	#6, DMACONR(a6)
	bne.s	.wait2				; wait
	rts

*******************************************************************************
* DrawLine: Draw lines using Blitter (to prepare polygon for filling)

;	A0 = PlanePtr, A6 = $DFF002, D0/D1 = X0/Y0, D2/D3 = X1/Y1
;	D4 = PlaneWidth > Kills: D0-D4/A0-A1 (+D5 in Fill Mode)

DL_MInterns	= $4A ; $CA

Oktants	dc.b	2+01, 2+01+$40
	dc.b	2+17, 2+17+$40
	dc.b	2+09, 2+09+$40
	dc.b	2+21, 2+21+$40

DrawLine:
	lea	CUSTOM+2, a6
	move.l	#DL_Width, d4		; PlaneWidth
	cmp.w	d1,d3			; Drawing only from Top to Bottom is
	bge.s	.y1ly2			; necessary for:
	exg	d0,d2			; 1) Up-down Differences (same coords)
	exg	d1,d3			; 2) Blitter Invert Bit (only at top of line)
.y1ly2:	sub.w	d1,d3			; D3 = yd
	mulu	d4,d1			; Use muls for neg Y-Vals
	add.l	d1,a0			; Please don't use add.w here !!!
	moveq	#0,d1			; D1 = Quant-Counter
	sub.w	d0,d2			; D2 = xd
	bge.s	.xdpos
	addq.w	#2,d1			; Set Bit 1 of Quant-Counter (here it
					; could be a moveq)
	neg.w	d2
.xdpos:	moveq	#$f,d4			; D4 full cleaned (for later oktants
					; move.b)
	and.w	d0,d4
	move.b	d4,d5			; D5 = Special Fill Bit
	not.b	d5
	lsr.w	#3,d0			; Yeah, on byte (necessary for bchg)...
	add.w	d0,a0			; ...Blitter ands automagically
	ror.w	#4,d4			; D4 = Shift
	or.w	#$B00+DL_MInterns,d4	; BLTCON0-codes
	swap	d4
	cmp.w	d2,d3			; Which Delta is the Biggest ?
	bge.s	.dygdx
	addq.w	#1,d1			; Set Bit 0 of Quant-Counter
	exg	d2,d3			; Exchange xd with yd
.dygdx:	add.w	d2,d2			; D2 = xd*2
	move.w	d2,d0			; D0 = Save for $52(a6)
	sub.w	d3,d0			; D0 = xd*2-yd
	addx.w	d1,d1			; Bit0 = Sign-Bit
	move.b	Oktants(PC,d1.w),d4	; In Low Byte of d4
					; (upper byte cleaned above)
	swap	d2
	move.w	d0,d2
	sub.w	d3,d2			; D2 = 2*(xd-yd)
	moveq	#6,d1			; D1 = ShiftVal (not necessary) 
					; + TestVal for the Blitter
	lsl.w	d1,d3			; D3 = BLTSIZE
	add.w	#$42,d3
	lea	$52-2(a6),a1		; A1 = CUSTOM+$52

	tst.b	(a6)			; Agnus bug
.wb:	btst	d1,(a6)			; Waiting for the Blitter...
	bne.s	.wb
	bchg	d5,(a0)			; Inverting the First Bit of Line

	move.l	d4,$40-2(a6)		; Writing to the Blitter Regs as fast
	move.l	d2,$62-2(a6)		; as possible
	move.l	a0,$48-2(a6)
	move.w	d0,(a1)+
	move.l	a0,(a1)+		; Shit-Word Buffer Ptr...
	move.w	d3,(a1)
	rts

; Init DrawLine			  A6 = $DFF000 > Kills : D0-D2
DL_Init:
	lea	CUSTOM, a6
	addq.w	#2,a6		; A6 = $DFF002 for DrawLine !
	moveq	#-1,d1
	IFGT	DL_Width-127
	move.w	#DL_Width,d0	; screen width in bytes
	ELSE
	moveq	#DL_Width,d0	; screen width in bytes
	ENDC
	moveq	#6,d2
.wb:	btst	d2,(a6)
	bne.s	.wb
	move.w	d1,$44-2(a6)
	move.w	d1,$72-2(a6)
	move.w	#$8000,$74-2(a6)
	move.w	d0,$60-2(a6)
	move.w	d0,$66-2(a6)
	rts

DL_Exit:
	subq.w	#2,a6		; A6 = $DFF000
	rts

*******************************************************************************
