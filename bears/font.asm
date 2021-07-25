********************************************************************************
* font.asm by hukka 2021
*
* draws a proportional font
* glyph bitmap: first char #33, 16 bits wide
********************************************************************************

	SECTION CODE

; ------------------------------------------------------------------------------
; a1 = font bitmap
; a2 = destination bitplane
; a4 = pointer to glyphptrs table
; a5 = pointer to FontStruct
; d0.b = glyph height in pixels
; d1.b = font kerning value
;
Font_Init:
	pushall

	move.b	d0, fnt_glyphheight(a5)
	move.b	d1, fnt_kerning(a5)
	move.w	#0, fnt_x(a5)
	move.l	a1, fnt_dataptr(a5)
	move.l	a2, fnt_bitplane(a5)
	move.l	a4, fnt_glyphptrs(a5)

	; fill the glyph bitmap pointer table
	movez.b	d0, d1			; glyph height*2 bytes
	add.w	d1, d1
	movez.b	#glyphcount-1, d0	; counter
	zero	d2
.f	move.l	d2, (a4)+
	add.l	d1, d2
	dbf	d0, .f

	popall
	rts

; ------------------------------------------------------------------------------
; a0 = pointer to SOURCE FontStruct
; a1 = font bitmap
; a2 = destination bitplane
; a4 = pointer to glyphptrs table
; a5 = pointer to DEST FontStruct
;
Font_InitOutline:
	; copy some things from the source struct
	move.b	fnt_glyphheight(a0), d0	; d0.b = glyph height in pixels
	addq	#2, d0
	move.b	fnt_kerning(a0), d1	; d1.b = font kerning value

	bsr	Font_Init

	move.l	fnt_dataptr(a0), a3	; get source font bitmap
	movez.b	#glyphcount-1, d3	; glyph counter

	pushall	; simply copy original glyphs first
.g	movez.b	fnt_glyphheight(a0), d2	; get source glyph height
	subq	#1, d2			; scanline counter
.y	move.w	(a3)+, d0		; get source scanline
	move.w	d0, (a1)+		; clone
	dbf	d2, .y
	move.l	#0, (a1)+		; skip 2 rows in dest
	dbf	d3, .g
	popall

.glyph	movez.b	fnt_glyphheight(a0), d2	; get source glyph height
	subq	#1, d2			; scanline counter

.row	move.w	(a3)+, d1		; get source scanline
	move.w	d1, d0			; copy

	rept	2
	asr.w	#1, d1			; shift right by 1 pixel
	or.w	d1, d0			; merge
	endr

	or.w	d0, 4(a1)
	or.w	d0, 2(a1)
	or.w	d0, (a1)+
	
	dbf	d2, .row

	addq	#4, a1
	dbf	d3, .glyph
	rts

; ------------------------------------------------------------------------------
; a0 = zero-terminated string
; a5 = ptr to FontStruct
; d0.w = x pixel coordinate
; d1.w = y pixel coorinate
;
Font_DrawText:
	move.l	fnt_dataptr(a5), a1
	move.l	fnt_bitplane(a5), a2

	move.w	d0, fnt_x(a5)
	push	d0

;	movez.w	fnt_bplwidth, d0
	movez.w	#LINEWIDTH, d0		; !!! optimization
	mulu.w	d1, d0
	add.w	d0, a2
	pop	d1

	zero	d0
.char	move.b	(a0)+, d0		; get ascii
	move.b	d0, d6
	bz	.out			; end of string

	cmp.b	#' ', d0
	beq	.space

	cmp.b	#'_', d0
	beq	.out

	sub.b	#' ', d0		; ascii->glyph index
	bsr	Font_DrawGlyph
	bsr	.drawn
	bra	.char

.out	move.w	d1, fnt_x(a5)
	rts

.drawn	lea	fnt_glyphwidths, a3
	add.w	d0, a3
	movez.b	(a3), d0		; get glyph width
	add.w	d0, d1			; advance x pos
	move.b	fnt_kerning(a5), d0	; add kerning
	add.w	d0, d1			; advance x pos
	rts
	
.space	zero	d0
	bra	.drawn

; ------------------------------------------------------------------------------
; d0.b = glyph index (0-based)
; d1.w = x pixel coordinate
; a1 = font bitmap
; a2 = destination bitplane
; a5 = ptr to font struct
;
Font_DrawGlyph:
	pushr	d0/a1-a2

	move.l	fnt_glyphptrs(a5), a3
	movez.b	d0, d3
	subq	#1, d3
	add.w	d3, d3
	add.w	d3, d3			; get location of glyph bitmap ptr
	add.l	d3, a3
	add.l	(a3), a1		; font glyph bitmap

	; get dest byte addr
	move.w	d1, d4			;
	asr.w	#3, d4			; x / 8
	add.w	d4, a2			; offset destination bpl

	move.b	d1, d3			;
	and.w	#7, d3			; get pixel shift
	movez.w	#8, d4
	sub.b	d3, d4			; actual shift value

	movez.b	fnt_glyphheight(a5), d3	; d3 = scanline
	subq	#1, d3

	ifz.b	d4, .draw1		; choose draw routine
	
.draw2	; need to write 3 bytes per glyph row
	movez.w	(a1)+, d0		; get row from glyph bitmap

	asl.l	d4, d0			; shift glyph in place
	or.b	d0, 2(a2)
	asr.l	#8, d0
	or.b	d0, 1(a2)
	asr.w	#8, d0
	or.b	d0, (a2)

;	move.w	fnt_bplwidth, d0	; next row
	move.w	#LINEWIDTH, d0		; !!! optimization
	add.w	d0, a2
	dbf	d3, .draw2

.out	popr
	rts

.draw1	; only need to write 2 bytes per glyph row
	movez.w	(a1)+, d0		; get row from glyph bitmap

	or.b	d0, 1(a2)
	asr.w	#8, d0
	or.b	d0, (a2)

;	move.w	fnt_bplwidth, d0	; next row
	move.w	#LINEWIDTH, d0		; !!! optimization
	add.w	d0, a2
	dbf	d3, .draw1
	bra	.out

; ------------------------------------------------------------------------------

;fnt_bplwidth:	dc.w	LINEWIDTH	; byte width of destination bitplane

fnt_glyphwidths:	; array of glyph widths
	;	 !"#$ %&'( )*+, -./    0123 4567 89:; <=>?
	;	@ABC DEFG HIJK LMNO    PQRS TUVW XYZ[ h]^_
	;
	dc.b	6,3,5,7,  0,6,8,3,    5,5,7,8,  3,9,2,6
	dc.b	7,4,7,7,  8,7,7,7,    7,7,2,3,  5,9,5,8
	dc.b	11,9,9,9, 9,9,9,9,    9,6,9,9,  9,9,9,9
	dc.b	9,9,9,9,  10,9,9,9,   9,10,8,4, 9,4,5,10

; ------------------------------------------------------------------------------

	SECTION FONTINFO, BSS

glyphheight = 18
glyphcount  = 63			; number of glyphs in font

		rsreset	; FontStruct
fnt_dataptr:	rs.l	1
fnt_bitplane:	rs.l	1
fnt_glyphptrs:	rs.l	1
fnt_x:		rs.w	1
fnt_glyphheight:rs.b	1		; pixel height of one glyph * bitplanes in font
fnt_kerning:	rs.b	1		; spacing between blitted glyphs
fontstructsize:	rs.w	0

		even
FontStruct:	ds.b	fontstructsize	; main font struct
		even
ShadowStruct:	ds.b	fontstructsize	; separate font for the outline
		even
GlyphPtrs:	blk.l	glyphcount	; pointers to glyph bitmaps
ShadowPtrs:	blk.l	glyphcount

FontOutline:	blk.b	(glyphheight+2)*glyphcount*2
		even
