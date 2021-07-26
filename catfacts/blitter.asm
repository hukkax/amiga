*******************************************************************************
* Blitter drawing functions
*******************************************************************************

DL_Width	= 40

* WaitBlitter: Wait for Blitter to finish

WaitBlitter:
	btst	#6, DMACONR+CUSTOM
.wait:
	btst	#6, DMACONR+CUSTOM		; test twice to fix old bug
	beq.b	.wait				; wait until blitter DMA is over
	rts

*******************************************************************************
* Use Blitter to clear a bitplane

safety	= 3					; high safety takes more raster

BlitterClear:					; a0=address
	WaitBlitter

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
	sub.w	#safety,d4
	move.w	ymax(pc),d5
	sub.w	d4,d5				; height
	add.w	#safety,d5
	mulu	#DL_Width,d4
	ext.l	d4
	add.l	d4,d0
	lsl.w	#6,d5
	or.w	d5,d2				; height and width

.wait1	btst	#14,CUSTOM+DMACONR
	bne.s	.wait1
	move.w	#$0100,CUSTOM+BLTCON0
	move.w	#$0000,CUSTOM+BLTCON1
	move.l	#$ffffffff,CUSTOM+BLTAFWM
	move.l	d0,CUSTOM+BLTDPTH
	move.w	d3,CUSTOM+BLTDMOD
	move.w	d2,CUSTOM+BLTSIZE
	add.l	#10240,d0			; point on next plane
.wait2	btst	#14,CUSTOM+DMACONR
	bne.s	.wait2
	rts

	rts

*******************************************************************************
* Fill: Fill a polygon with Blitter

BlitterFill:
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
.wait1	btst	#14, CUSTOM+DMACONR
	bne.b	.wait1				; wait
	move.l	d0, CUSTOM+BLTAPTH
	move.l	d0, CUSTOM+BLTDPTH
	move.w	#$09f0, CUSTOM+BLTCON0		; minterm d=a
	move.w	#%00010010, CUSTOM+BLTCON1	; exclusive fill (bit 4)
	move.l	#$ffffffff, CUSTOM+BLTAFWM	; masks
	move.w	d3, CUSTOM+BLTAMOD		; a modulo
	move.w	d3, CUSTOM+BLTDMOD		; d modulo
	move.w	d1, CUSTOM+BLTSIZE		; fill height and start address
.wait2	btst	#6, CUSTOM+DMACONR
	bne.s	.wait2				; wait
	rts

*******************************************************************************

xmin	dc.w	0		; Bounding box
ymin	dc.w	0
xmax	dc.w	0
ymax	dc.w	0

*******************************************************************************
