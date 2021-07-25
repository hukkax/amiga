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


	; CPU bresenham implementation
	;
	rem

.absx	sub.w	x1, x2			; line_dx := abs(x2-x1)
	bpl	.absy
	neg.w	x2

.absy	move.w	x2, line_dx(a5)
	movez.w	line_destx(a5), x2

	sub.w	y1, y2			; line_dy := abs(y2-y1)
	bpl	.sx
	neg.w	y2

.sx	move.w	y2, line_dy(a5)
	movez.w	line_desty(a5), y2

	move.w	#1, line_sx(a5)		; line_sx := 1
	cmp.w	x1, x2
	bge	.sy			; if x2 < x1 then
	neg.w	line_sx(a5)		;  line_sx := -1

.sy	move.w	#1, line_sy(a5)		; line_sy := 1
	cmp.w	y1, y2
	bge	.le			; if y2 < y1 then
	neg.w	line_sy(a5)		;  line_sy := -1

.le	move.w	line_dx(a5), d4
	move.w	line_dy(a5), d5
	sub.w	d5, d4
	move.w	d4, line_error(a5)	; line_error := line_dx - line_dy

	; calc line length
	move.w	line_dx(a5), d4		; d4=line_dx
	cmp.w	d4, d5			; d5=line_dy
	bhi	.put			; if line_dx >= line_dy then
	move.w	d4, d5			;   line_length := line_dx

.put	move.w	d5, line_length(a5)

	move.l	bitplaneptr(a5), a0

; -----------------------------------------------------------------------------
; d5=length counter

line_advance:
	lea	line_error(a5), a1
	move.w	(a1), d0	; d0=e2
	add.w	d0, d0		; e2 := 2 * line_error

	move.w	line_dy(a5), d1
	neg.w	d1		; -line_dy

	cmp.w	d1, d0		; if e2 >= -line_dy
	bge	.lx

.foo	move.w	line_dx(a5), d1	; if e2 <= line_dx
	cmp.w	d1, d0
	ble	.ly

	bra	.ok

.lx	; if e2 >= -line_dy then
	move.w	line_dy(a5), d1
	sub.w	d1, (a1)	; line_error -= line_dy
	move.w	line_sx(a5), d1
	add.w	d1, line_x(a5)	; line_x += line_sx
	bra	.foo

.ly	; if e2 <= line_dx then
	add.w	d1, (a1)	; line_error += line_dx
	move.w	line_sy(a5), d1	; line_y += line_sy
	add.w	d1, line_y(a5)

; -----------------------------------------------------------------------------

.ok	movez.w	line_x(a5), x1	; d0
	movez.w	line_y(a5), y1	; d1
;	sub.w	#1, line_length(a5)

;	bsr	.plot		; x, y
;	addq	#1, x1
;	bsr	.plot		; x-1, y
;	addq	#1, y1		; x+1, y-1
;	bsr	.plot

.plot	push	d0-d1/a0	; a0 = bitplane
				; d0,d1 = x, y
	; manage the Y position
	move.w	d1, d2		; copy y
	lsl.w	#4, d1		; multiply by 40
	lsl.w	#3, d2
	add.w	d1, d1
	add.w	d2, d1
	if	RESOLUTION>0	; for medres/hires bitplanes
	add.w	d1, d1		; scanline length 40->80
	endc
	add.w	d1, a0		; enter y pos first
	move.w	d0, d4
	move.l	a0, a1

	; manage the X position
	move.w	d0, d2		; copy x
	lsr.w	#3, d0		; divide by 8 to get the hardposition
	and.w	#$000F, d2	; mask the 4 lower bits for 0-15 softposition
	btst	#0, d0		; is the hardposition an odd value ?
	beq.s	.evn		; nope ...skip the -1 action
	subq.w	#1, d0		; it's odd ...sub 1 to keep even steps (68000!)
.evn	sub.w	#15, d2
	neg.w	d2
	add.w	d0, a0		; move to X hardposition
	move.w	(a0), d3	; take current screendata word aligned
	bset	d2, d3		; set the softposition pixel
	move.w	d3, (a0)	; put back the modified word

	add.w	#40, a0
	move.w	(a0), d3	; take current screendata word aligned
	bset	d2, d3
	move.w	d3, (a0)	; put back the modified word

	; manage the X position
	move.l	a1, a0
	move.w	d4, d0
	addq	#1, d0
	move.w	d0, d2		; copy x
	lsr.w	#3, d0		; divide by 8 to get the hardposition
	and.w	#$000F, d2	; mask the 4 lower bits for 0-15 softposition
	btst	#0, d0		; is the hardposition an odd value ?
	beq.s	.evn2		; nope ...skip the -1 action
	subq.w	#1, d0		; it's odd ...sub 1 to keep even steps (68000!)
.evn2	sub.w	#15, d2
	neg.w	d2
	add.w	d0, a0		; move to X hardposition
	move.w	(a0), d3	; take current screendata word aligned
	bset	d2, d3		; set the softposition pixel
	move.w	d3, (a0)	; put back the modified word

	pop	d0-d1/a0

.ldone	ifz.b	d5, .out
	subq	#1, d5
	bnz	line_advance

	; line done
.out	move.w	line_destx(a5), x1
	move.w	line_desty(a5), y1
	move.w	x1, line_x(a5)
	move.w	y1, line_y(a5)
	rts

	erem

; -----------------------------------------------------------------------------

picdone:
	move.l	a1, linedataptr(a5)
;	move.b	#1, line_done(a5)
	move.b	#1, pic_done
	rts

********************************************************************************

	SECTION LINESTRUCTS, BSS

		ALIGN32

		rsreset	; LineStruct struct
bitplaneptr:	rs.l	1
linedataptr:	rs.l	1
origdataptr:	rs.l	1
line_x:		rs.w	1
line_y:		rs.w	1
;line_dx:	rs.w	1
;line_dy:	rs.w	1
;line_sx:	rs.w	1
;line_sy:	rs.w	1
;line_error:	rs.w	1
;line_length:	rs.w	1
line_destx:	rs.w	1
line_desty:	rs.w	1
;randomness:	rs.b	1	; 1=want to randomly vary line coords?
;line_done:	rs.b	1	; 1=current drawing is fully drawn
;linethickness:	rs.b	1
origheader:	rs.b	1
		even
linestructsize:	rs.w	1

		ALIGN32
LineStruct:	ds.b	linestructsize

pic_done:	ds.b	1
ScreenDone:	ds.b	0

; ------------------------------------------------------------------------------
