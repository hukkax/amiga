********************************************************************************
* linedrawer.asm by hukka 2020-2021
********************************************************************************

	SECTION CODE

RESOLUTION=0 ; lores

x1	equr	d0
y1	equr	d1
x2	equr	d2
y2	equr	d3

osx	equr	d7
osy	equr	d4

; -----------------------------------------------------------------------------
; reads a possibly unaligned word from a1 into d1

ReadWord:
	movez.b	(a1)+, d1		; x := Seg.X
	asl.w	#8, d1			; possibly unaligned word
	move.b	(a1)+, d1
	rts

; -----------------------------------------------------------------------------
; d6 = line thickness

DrawLineImage:
	zero	osx			; x offset for drawn lines
	lea	LineStruct, a5

	move.l	linedataptr(a5), a1	; image header
	move.l	a1, origdataptr(a5)
	move.b	(a1), d0
	push	d0/a1			; store header fill mode

	; 0=1 pixel thin
	; 1=2 pixels, vert.
	; 2=3 pixels, vert + horiz
	; 3=3 pixels, vert + horiz + fill
	cmp.b	#3, d6
	beq	.silh
	lea	BufferBack1, a1

.redraw

.skip	pushall
	bsr	DrawSetup
	popall

.next	dbf	d6, .redo
	pop	d0/a1			; restore header
	move.b	d0, (a1)
	rts

.redo	; a1=bpl
	cmp.b	#2, d6		; just drew silhouette?
	beq	.si		; yes

	; didn't draw silhouette
.ns	eor.b	#1, osx		; toggle x=0/1

	btst	#0, osx		; set back to zero?
	bne	.ok
	add.l	#40, a1
	bra	.ok

.si	zero	osx
	lea	BufferBack1, a1

.ok	move.l	origdataptr(a5), a2
	move.l	a2, linedataptr(a5)
	bra	.redraw

.silh	lea	BufferBack2, a1
	bra	.skip

; -----------------------------------------------------------------------------
; a0 = ptr to line data
; a1 = ptr to bitplane
; a5 = ptr to LineStruct structure
;
DrawSetup:
	move.l	a1, bitplaneptr(a5)
;	move.l	a0, linedataptr(a5)

	bsr	DL_Init

;	lea	LineStruct, a5
;	lea	CUSTOM+2, a6

	; image header
	move.l	linedataptr(a5), a1
	;move.l	a1, origdataptr(a5)	; store if we want to redraw after fill

	move.b	(a1), d6		; just interested in the top bit
	move.b	#0, (a1)+
	and.b	#%10000000, d6		; for line drawing mode
	move.b	(a1)+, d0		; ignore rest

	;move.w	osx, osy	; y offset
	;swap	osx		; x offset

	; read bounding box (for fill)
	bsr	ReadWord
;		add.b	osx, d1
	move.w	d1, xmin

	bsr	ReadWord
	;	add.w	osy, d1
	move.w	d1, ymin

	bsr	ReadWord
;		add.b	osx, d1
	move.w	d1, xmax

	bsr	ReadWord
	;	add.w	osy, d1
	addq	#1, d1
	move.w	d1, ymax

.new	move.l	a1, linedataptr(a5)
	move.b	#0, pic_done		; new image

.draw	bsr	DoDrawLine

	tst.b	pic_done		; finished drawing?
	bz	.draw

	ifnz.b	d6, .fill
.out	rts

.fill	; if we were in area outline line mode, fill the area drawn
	lea	LineStruct, a5

	move.l	bitplaneptr(a5), a0	; A0 = PlanePtr

	bsr	Fill			; blitter fill
	bra	DL_Init			; must reinit line drawer!
	;rts

; -----------------------------------------------------------------------------
; a5 = ptr to LineStruct structure
;
DoDrawLine:
	move.l	linedataptr(a5), a1

.read
	; x := Seg.X and $7FFF
	;
	movez.b	(a1)+, x1		; x := Seg.X
	asl.w	#8, x1			; possibly unaligned word
	move.b	(a1)+, x1

	cmp.w	#$FFFF, x1		; finished drawing the picture?
	beq	picdone			; yes

	add.b	osx, x1

	movez.b	(a1)+, y1		; y := Seg.Y

	; if (Seg.X and $8000) = 0 then
	;
	bclr	#15, x1			; x := x and $7FFF
	bz	.drawline		; DrawLine(ox,oy, x,y)

;	add.w	osy, y1

	move.w	x1, line_x(a5)		; ox := x
	move.w	y1, line_y(a5)		; oy := y

	bra	.read

.drawline
	move.l	a1, linedataptr(a5)

	;move.w	x1, line_destx(a5)
	;move.w	y1, line_desty(a5)
	movez.w	x1, x2
	movez.w	y1, y2
	movez.w	line_x(a5), x1		; line_x := x1
	movez.w	line_y(a5), y1		; line_y := y1

	; blitter
	move.l	bitplaneptr(a5), a0	; A0 = PlanePtr
	move.l	#LINEWIDTH, d4		; D4 = PlaneWidth

	move.w	x2, line_x(a5)
	move.w	y2, line_y(a5)

	bra DrawLine

; -----------------------------------------------------------------------------

picdone:
	move.l	a1, linedataptr(a5)
	move.b	#1, pic_done
	rts

********************************************************************************

	SECTION LINESTRUCTS, BSS

		rsreset	; LineStruct struct
bitplaneptr:	rs.l	1
linedataptr:	rs.l	1
origdataptr:	rs.l	1
line_x:		rs.w	1
line_y:		rs.w	1
line_destx:	rs.w	1
line_desty:	rs.w	1
origheader:	rs.b	1
linestructsize:	rs.w	0

		even
LineStruct:	ds.b	linestructsize+2

pic_done:	ds.b	1
		even

; ------------------------------------------------------------------------------
