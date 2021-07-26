********************************************************************************
* Diskmag engine font blitting routines
********************************************************************************
; Text data format:
;
; word		P = number of pages
; word		A = number of articles
; word * A	starting page numbers for each article
; long * P	pointers to page texts
; bytes		text for each page (ascii offset by -32; 0=space etc.)
;		each page terminated by byte EOF

********************************************************************************

	SECTION CODE

NL  	= 10	; end of text line
EOF 	= 255	; end of page content
INVERT	= 253	; invert this text line

********************************************************************************
; Fill in pointers to all glyphs inside font bitmap and
; all character rows in output bitmap
;
InitFonts:
	; multiply precalced sine table by CHARSPERROW
	;
	zero	d1
	lea	YSlideTab, a0
.ftab	move.w	(a0), d1
	cmp.w	#-1, d1			; end of table?
	beq	.lines
	mulu.w	#CHARSPERROW, d1
	move.w	d1, (a0)+
	bra	.ftab

	; fill in pointers to text lines
	;
.lines	zero	d1
	lea	LinePtr, a1
	moveq	#30, d0
.lcopy	add.w	#CHARSPERROW*2, d1
	move.l	d1, (a1)+
	add.l	#CHARSPERROW*(LINEHEIGHT-2), d1
	dbf	d0, .lcopy

	rts

********************************************************************************
Animation:
	ifnz.b	Blitting, .out
	ifz.b	FadeDir, .out

	zero	d1
	move.l	YSlidePtr, a0

	cmp.b	#1, FadeDir
	bne	.du

.ud	move.w	(a0)+, d1		; scroll down from top page
	bra	.done

.du	move.w	-(a0), d1		; scroll up from bottom page

.done	move.l	a0, YSlidePtr
	cmp.w	#-1, d1
	beq	.stop			; done 

	lea	BufferFore1, a0
	add.l	d1, a0

	lea	bplText, a1
	bsr	SetBPLPtrs
	add.l	#CHARSPERROW, a0
	lea	bplShadow, a1
	bsr	SetBPLPtrs

.out	rts

.stop	cmp.b	#1, FadeDir
	bne	.stout

	move.b	#1, Blitting

	lea	BufferFore2, a0		; copy lower to upper screen
	lea	BufferFore1, a1
	bsr	CopyPlayfield

	WaitForVBL

	lea	BufferFore1, a0		; switch display to upper screen
	lea	bplText, a1
	bsr	SetBPLPtrs
	add.l	#CHARSPERROW, a0
	lea	bplShadow, a1
	bsr	SetBPLPtrs

.stout	move.b	#0, FadeDir
	move.b	#0, Blitting
	rts

********************************************************************************
; Enable animated page switch from upper to lower or vice versa
;
; d0.w = page number
;
SwitchPage:
	ifnz.b	Blitting, .done		; page drawing still in progress?
	ifnz.b	FadeDir, .done		; page change still animating?

	cmp.w	CurrentPage, d0
	beq	.done			; no change of page

	move.b	#1, Blitting

	cmp.w	#-1, CurrentPage
	beq	.ud

	cmp.w	CurrentPage, d0
	blo	.du

	; scrolling down to next page
	; 
.ud	lea	YSlideTab, a2		; slide down from upper page
	move.l	a2, YSlidePtr
	move.b	#1, FadeDir
	moveq	#1, d4
	bra	.load

	; scrolling up to prev page
	; 
.du	lea	YSlidE, a2
	move.l	a2, YSlidePtr
	move.b	#-1, FadeDir

	lea	BufferFore1, a0		; copy upper to lower screen
	lea	BufferFore2, a1
	bsr	CopyPlayfield

	WaitForVBL

	lea	BufferFore2, a0		; switch display to lower screen
	lea	bplText, a1
	bsr	SetBPLPtrs
	add.l	#CHARSPERROW, a0
	lea	bplShadow, a1
	bsr	SetBPLPtrs

	moveq	#0, d4

.load	move.w	d0, CurrentPage

	; find pointer to new page
	;
	lea	Pages, a1
	asl.w	#2, d0			; * 4
	move.l	(a1,d0), a0
	lea	Text, a1
	add.l	a1, a0
	move.l	d4, d0

	move.w	(a0)+, CurrentPage
	move.w	(a0)+, CurrentArticle
;	addq	#4, a0			; skip header

	lea	TextBuffer, a1
	bsr	Decrunch		; A0 = Source / A1 = Destination
	lea	TextBuffer, a0

	; process header
	;

	bra	BlitPage		; blit the text content
.done	rts

********************************************************************************
; Blits a pageful of text into a bitplane
; A0 = pointer to text
; d0.b = page (0=upper 1=lower)
;
; Special codes:
; 255 = End
; 254 = line break
;
BlitPage:
	move.b	#1, Blitting
	zero	d4			; y
	ifz.b	d0, .p1

.p2	lea	BufferFore2, a3		; bottom page
	bra	.clear

.p1	lea	BufferFore1, a3		; top page

.clear	move.l	a0, a1
	move.l	a3, a0
	push	d0
	move.l	#BPLSIZE/4-1, d0	; 1 bitplane, size in longwords
	bsr	ClearPlayfield
	pop	d0
	move.l	a1, a0

.nl	moveq	#0, d5 			; want invert?
	move.l	a3, a1
	move.l	d4, d1
	lea	LinePtr, a4
	asl.w	#2, d1			; * 4
	add.l	(a4,d1.w), a1
	move.l	a1, a4			; a4 = dest address for plotting

	cmp.b	#INVERT, (a0)		; check for invert code at line start
	beq	.invert			; we want to invert this text line

.c	zero	d2			; d2 = char code
	move.b	(a0)+, d2		; a0 = ptr to current char in text

	cmp.b	#NL, d2			; end of string?
	beq	.newline
	cmp.b	#EOF, d2
	beq	.done

.go	move.l	a4, a1
	addq	#1, a4			; advance x position for next char

;	sub.w	#32, d2
;	ifz.b	d2, .c			; don't blit space chars
	cmp.b	#32, d2
	beq	.c

	lea	Font-(32*8), a2
	asl.w	#3, d2			; * 8
	add.l	d2, a2

	moveq	#8-1, d3		; font height
.y	move.b	(a2)+, (a1)		; copy 8px from font data
	add.l	#CHARSPERROW, a1
	dbf	d3, .y			; next scanline
	bra	.c			; next char

.done	move.w	#-1, d0
	bsr	UpdateTooltip

	move.b	#0, Blitting
	rts

.invert	moveq	#1, d5
	addq	#1, a0
	move.l	a1, a5
	bra	.c

.newline
	ifz.b	d5, .ni

	; invert the text line we just painted
	move.l	a5, a1
	sub.l	#CHARSPERROW, a1
	move.l	#20*10-1, d3
.iy	eor.l	#$FFFFFFFF, (a1)+
	dbf	d3, .iy


.ni	cmp.b	#TEXTLINES-2, d4	; exit if last row of text on
	beq 	.done			; screen has been drawn
	addq	#1, d4
	bra	.nl

********************************************************************************
; D0 = index of tooltip (or -1 for current article title)
; A0 = address of tooltip string pointer table
;
UpdateTooltip:
	push	d0/a0

	cmp.w	#-1, d0
	bne	.ok

	; title of current article
	lea	Tooltips\.ChapterNamePtr, a0
	move.w	CurrentArticle, d0

.ok	asl.w	#2, d0			; *4
	move.l	(a0, d0.w), a0

	lea	BufferPanelBottom, a4
	add.l	#80+14, a4		; X=14, Y=second rasterline

.clear	move.l	a4, a1
	zero	d0
	moveq	#8-1, d1
.cy	move.l	d0, (a1)+		; zero 42 chars, 8 scanlines
	move.l	d0, (a1)+
	move.l	d0, (a1)+
	move.l	d0, (a1)+
	move.l	d0, (a1)+
	move.l	d0, (a1)+
	move.l	d0, (a1)+
	move.l	d0, (a1)+
	move.l	d0, (a1)+
	move.l	d0, (a1)+
	move.w	d0, (a1)+		;!
	add.l	#80-40-2, a1
	dbf	d1, .cy

.c	zero	d2			; d2 = char code
	move.b	(a0)+, d2		; a0 = ptr to current char in text

	ifz.b	d2, .done		; end of string?
;	sub.w	#32, d2
;	ifz.b	d2, .c			; don't blit space chars
;	cmp.b	#32, d2
;	beq	.c

	move.l	a4, a1
	addq	#1, a4			; advance x position

	asl.w	#3, d2
	lea	Font-(32*8), a2
	add.l	d2, a2

	moveq	#8-1, d3		; font height
.y	move.b	(a2)+, (a1)		; copy 8px from font data
	add.l	#CHARSPERROW, a1	; next scanline
	dbf	d3, .y

	bra	.c

.done	pop	d0/a0
	rts

********************************************************************************
; Blits a line of text into bitplane
; A0 = pointer to null-terminated string
; A1 = pointer to bitplane
; D0 = X (char coords)
; D1 = Y (char coords)
;
	rem>
BlitText:
	move.l	a1, a4
	lea	LinePtr, a2
	asl.w	#2, d1
	move.l	(a2,d1.w), a1		; read LinePtr[Y]

	add.l	a4, a1
	add.w	d0, a1			; dest. X pos in scanline
	move.l	a1, a4

.c	zero	d2			; d2 = char code
	move.b	(a0)+, d2		; a0 = ptr to current char in text

	cmp.b	#NL, d2			; end of string?
	beq	.done

	move.l	a4, a1
	addq	#1, a4			; advance x position

	asl.w	#3, d2
	lea	Font, a2
	add.l	d2, a2

	moveq	#8-1, d3		; font height
.y	move.b	(a2)+, (a1)		; copy 8px from font data
	add.l	#CHARSPERROW, a1	; next scanline
	dbf	d3, .y

	bra	.c

.done	rts
	<erem

********************************************************************************

	SECTION	DATA

		ALIGN32

LinePtr:	blk.l	31

YSlidePtr:	dc.l	0

CurrentPage:	dc.w	-1
CurrentArticle:	dc.w	0

FadeDir:	dc.b	0
Blitting:	dc.b	0
		even

		dc.w	-1
YSlideTab:	; used to smoothly animate slide on page change
		dc.w 	000,001,002,003,006,011,016,022,030,038,047,056,067
		dc.w 	077,089,100,112,124,136,148,159,171,182,191,200,208
		dc.w	214,216,218,219
YSlidE:		dc.w	220, -1


		include	"text.asm"

********************************************************************************
