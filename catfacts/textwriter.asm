***************************************************************************************************
* Text writer by hukka/exec 2014
*

	SECTION CODE

; -------------------------------------------------------------------------------------------------

ROWLEN		= 20			; length in chars of one text row
MASKS		= 8			; amount of mask chars available

W_FONT		= 182			; width of font bitmap in bytes
;W_FONT		= 118			; width of font bitmap in bytes
W_MASK		= 16			; width of mask bitmap in bytes

PICWIDTH	= 320;288			; width of image bitmap
PICHEIGHT	= 246;212

LINESPACING	= 17

EMPTYLINE 	= 1
TWMODES		= 4

;CHARSPERSCREEN	= 162

DEFAULTWAIT	= 5*100			; frames

CMD_SETCOLOR	= 249
CMD_IMAGE_OUT	= 250
CMD_IMAGE_IN	= 251
CMD_WAIT	= 252
CMD_TEXT_OUT	= 253
CMD_TEXT	= 254
CMD_END		= 255

; -------------------------------------------------------------------------------------------------
; initialize textwriter
;
TextWriter_Init:
	move.b	#CMD_TEXT, twMode
	move.w	#$036, WaveColor
	move.w	WaveColor, WaveColorOrig

	lea	Text, a0
	move.l	a0, twTextPtr
	move.w	#0, twInterval

	;move.b	#-1, twMaskMode
	bsr	TW_NextScreen
	move.w	#0, twInterval

	lea	pic1, a6			; image bitmap
	move.l	a6, twPicRow
	rts

; -------------------------------------------------------------------------------------------------
;
TextWriter:
	ifz.b	Started, .done

	move.b	twMode, d0

	cmp.b	#CMD_TEXT, d0
	beq	.i

	cmp.b	#CMD_TEXT_OUT, d0
	beq	TW_EraseText

	cmp.b	#CMD_IMAGE_IN, d0
	beq	TW_DrawImage

	cmp.b	#CMD_IMAGE_OUT, d0
	beq	TW_EraseImage

;	rts

.i	ifz.w	twInterval, .ok

	sub.w	#1, twInterval
	rts

.ok	;cmp.b	#5, twSpeed
	;bmi	.go

.go	cmp.b	#ROWLEN, twMasksDone
	beq	TW_NextLine

	; draw masked chars
	;
	lea	twTextRow, a3		; a3 = text buffer
	lea	twMaskRow, a4		; a4 = mask buffer
	move.l	twCurrentLine, a5	; a5 = address of first raster line
	move.l	a5, a2			; memorize

	moveq	#ROWLEN-1, d5		; char loop counter

.char	zero	d0
	move.b	(a3)+, d0		; d0 = char #

;	cmp.b	#32, d0			; space?
;	beq	.nchar

	ifz.b	d0, .domask

	zero	d1
	move.b	(a4)+, d1		; d1 = mask #
	btst	#7, d1
	bne	.nchar

	sub.b	#32, d0

	lea	font, a0		; a0 = font bitmap
	add.w	d0, d0
	add.l	d0, a0			; find font char

.mask	lea	mask, a1		; a1 = mask bitmap
	add.w	d1, d1
	add.l	d1, a1			; find mask

	moveq	#12-1, d4		; d4=y loop counter

.y	move.w	(a0), d0		; get char pixrow
	and.w	(a1), d0		; mask it
	move.w	d0, (a5)		; pixrow to screen
	add.l	#LINEWIDTH*1, a5	; or *2 for scanline effect
	add.l	#W_FONT, a0
	add.l	#W_MASK, a1

	dbf	d4, .y			; to next row in char

.nchar	addq	#2, a2			; advance to next char pos.
	move.l	a2, a5			; remember raster line

	dbf	d5, .char		; to next char

	; advance mask buffer
	;
.domask	lea	twMaskRow, a0
	moveq	#ROWLEN-1, d0
	zero	d2			; how many masks done?
	zero	d1
.cc	move.b	(a0), d1
	cmp.b	#MASKS-1, d1		; is this char completely showing already?
	bne	.ma			; yes; mask already ok
	addq	#1, d2
	bra	.ne
.ma	add.b	#1, d1			; no; grow to next mask
	move.b	d1, (a0)
.ne	addq	#1, a0
	dbf	d0, .cc
	move.b	d2, twMasksDone

.done	rts

; -------------------------------------------------------------------------------------------------
; advance to next line of text
;
TW_NextLine:
	; copy first line of text into buffer
	;
.text	move.l	twTextPtr, a0

	move.b	(a0), d1
	btst	#7, d1			; top bit set?
	bne	.special		; screen ends or text restarts

	lea	twTextRow, a1
	moveq	#ROWLEN-1, d0
.e	move.b	#' ', (a1)+
	dbf	d0, .e

	lea	twTextRow, a1
	moveq	#ROWLEN-1, d0
.cc	move.b	(a0)+, d1
	move.b	d1, (a1)+
	ifz	d1, .mask
	dbf	d0, .cc

	; clear mask buffer
	;
.mask	move.l	a0, twTextPtr

	cmp.b	#EMPTYLINE, twTextRow
	bne	.ok
	bra	.nextline

.ok	zero	d1
	move.b	#-ROWLEN, d1
	lea	twMaskRow, a1
	lea	twAppears, a0
	zero	d0
	add.b	#1, twMaskMode
	cmp.b	#TWMODES, twMaskMode
	blo	.md
	move.b	#0, twMaskMode
.md	move.b	twMaskMode, d0
	mulu.w	#ROWLEN, d0
	add.l	d0, a0
	moveq	#ROWLEN-1, d0
.mm	move.b	(a0)+, (a1)+
	add.b	#1, d1
	dbf	d0, .mm

	zero	d0
	move.b	twMaskMode, d0

	move.b	#0, twMasksDone
	;move.w	#0, twCounter
	move.w	#0, twInterval

.nextline
	move.l	twCurrentLine, a0		; address of first raster line
;	add.l	#LINEWIDTH*25, a0		; advance rasterline (line spacing for text)
	add.l	#LINEWIDTH*LINESPACING, a0	; advance rasterline (line spacing for text)
	move.l	a0, twCurrentLine
	rts

; handle command chars
;
.special
	add.l	#1, twTextPtr
	move.w	#0, twInterval
	move.w	#DEFAULTWAIT, twInterval

	cmp.b	#CMD_WAIT, d1			; wait X more seconds (next value = X)
	bne	.n1
	bsr	getval				; get seconds amount in d1
	mulu.w	#100, d1
	move.w	d1, twInterval
	rts

.n1	move.b	d1, twMode

	cmp.b	#CMD_IMAGE_OUT, d1		; erase image on screen
	bne	.n2
	rts

.n2	cmp.b	#CMD_IMAGE_IN, d1		; display image
	beq	TW_InitImage

.to	cmp.b	#CMD_TEXT_OUT, d1
	beq	TW_NextScreen

.tc	cmp.b	#CMD_SETCOLOR, d1
	beq	TW_ColorChange

.tm	move.w	#DEFAULTWAIT, twInterval	; text mode

	cmp.b	#CMD_TEXT, d1
	beq	TW_NextScreen			; screen ends, wait for a while

	cmp.b	#CMD_END, d1
	beq	TextWriter_Init			; text restarts from beginning
.out	rts

; read parameter for command
;
getval:	zero	d1
	move.l	twTextPtr, a0
	move.b	(a0)+, d1
	move.l	a0, twTextPtr
	rts

; -------------------------------------------------------------------------------------------------

TW_ColorChange:
	move.l	twTextPtr, a0

	move.b	(a0)+, d1
	ifnz.b	d1, .ok				; skip zero byte if word was autoaligned
	move.b	(a0)+, d1
.ok	rol.w	#8, d1
	move.b	(a0)+, d1			; d1 = waves color
	move.l	a0, twTextPtr

	ifnd	FXFADES
	move.w	d1, WaveColorOrig
	move.w	d1, WaveColor
	endif

	move.b	#CMD_TEXT, twMode
	move.w	#0, twInterval
	rts

; -------------------------------------------------------------------------------------------------
; advance to next screenful of text
;
TW_NextScreen:
	lea	BufferFore, a0
	move.l	a0, twDestRow
;	add.l	#2-(LINEWIDTH*7), a0		; initial rasterline for text output
	sub.l	#(LINEWIDTH*10), a0		; initial rasterline for text output
	move.l	a0, twCurrentLine
	move.w	#DEFAULTWAIT, twInterval
;	move.b	#SCREENSIZE_Y, twLinesDrawn
	move.b	#0, twLinesDrawn	; 256
	rts

; -------------------------------------------------------------------------------------------------

TW_InitImage:
	bsr	getval				; d1 = image #
	move.b	#0, twLinesDrawn
	lea	BufferFore, a0
	move.l	a0, twDestRow
	rts

; -------------------------------------------------------------------------------------------------

TW_DrawImage:
	move.l	twPicRow, a6
	move.l	twDestRow, a0

	move.l	#(PICWIDTH/32)-1, d0
.x	move.l	(a6)+, (a0)+
	move.l	(a6)+, (a0)+
	dbf	d0, .x

	move.l	a0, twDestRow
	move.l	a6, twPicRow
	add.b	#2, twLinesDrawn
	cmp.b	#PICHEIGHT, twLinesDrawn
	bne	.out
;	ifnz	twLinesDrawn, .out

	; image fully on screen, done with drawing
	move.b	#CMD_TEXT, twMode
.out	rts

; -------------------------------------------------------------------------------------------------

TW_EraseText:
	move.l	twDestRow, a0

	move.l	#(LINEWIDTH/4)-1, d0
.x	move.l	#$0, (a0)+
	move.l	#$0, (a0)+
	move.l	#$0, (a0)+
	move.l	#$0, (a0)+
	dbf	d0, .x

	move.l	a0, twDestRow
	sub.b	#4, twLinesDrawn
	ifnz.b	twLinesDrawn, .out

	move.b	#CMD_TEXT, twMode
	bsr	TW_NextScreen
	move.w	#0, twInterval
.out	rts

TW_EraseImage:
	move.l	twDestRow, a0
;	sub.l	#2, a0

	move.l	#(PICWIDTH/32)-1, d0
.x1	move.l	#0, -(a0)
	dbf	d0, .x1
;	sub.l	#4, a0

	move.l	#(PICWIDTH/32)-1, d0
.x2	move.l	#0, -(a0)
	dbf	d0, .x2
;	sub.l	#2, a0

	move.l	a0, twDestRow
	sub.b	#2, twLinesDrawn
	ifnz.b	twLinesDrawn, .out

	move.b	#CMD_TEXT, twMode
	bsr	TW_NextScreen
	move.w	#0, twInterval
.out	rts

***************************************************************************************************



***************************************************************************************************

	SECTION	BSS

		align32

twTextPtr:	dc.l	0
twPicRow:	dc.l	0			; current scanline in image (if drawing image)
twDestRow:	dc.l	0			; destination scanline to draw to


twCurrentLine:	dc.l	0			; scanline address of current text line
twCurrentY:	dc.w	0			; number of current text line

twInterval:	dc.w	0			; delay counter
;twCounter:	dc.w	0			; how many frames current text line has been shown

WaveColor:	dc.w	0			; waves color
WaveColorOrig:	dc.w	0

twTextRow:	blk.b	ROWLEN			; table of chars for this text line
twMaskRow:	blk.b	ROWLEN			; table of masks for this text line

twMaskMode:	dc.b	-1
twMasksDone:	dc.b	0

twMode:		dc.b	0
twLinesDrawn:	dc.b	0			; amount of scanlines of image drawn
		even

; -------------------------------------------------------------------------------------------------

	SECTION TEXT, DATA

;ZZZ = -010

twAppears:	dc.b	-09,-08,-07,-06,-05,-04,-03,-02,-01,000,000,-01,-02,-03,-04,-05,-06,-07,-08,-09
		dc.b	000,-01,-02,-03,-04,-05,-06,-07,-08,-09,-09,-08,-07,-06,-05,-04,-03,-02,-01,000
		dc.b	-19,-18,-17,-16,-15,-14,-13,-12,-11,-10,-09,-08,-07,-06,-05,-04,-03,-02,-01,000
		dc.b	000,-01,-02,-03,-04,-05,-06,-07,-08,-09,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19
;		dc.b	000,ZZZ,000,ZZZ,000,ZZZ,000,ZZZ,000,ZZZ,000,ZZZ,000,ZZZ,000,ZZZ,000,ZZZ

Text:
		dc.b	CMD_SETCOLOR
		dc.w	$036

		dc.b	CMD_WAIT, 1
		dc.b	CMD_IMAGE_IN, 1 	; show image 1
		dc.b	CMD_WAIT, 2		; wait
		dc.b	CMD_IMAGE_OUT	 	; erase image

	;rem>
		;	"####################"
		dc.b	"                    "
		dc.b	"                    "
		dc.b	"                    "
		dc.b	"    TO CELEBRATE    "
		dc.b	"      30 YEARS      "
		dc.b	"    OF THE AMIGA    "
		dc.b	"                    "
		dc.b	"                    "
		dc.b	" WE PROUDLY PRESENT "
		dc.b	"    20 KILOBYTES    "
		dc.b	"     WORTH OF...    "
		dc.b	"                    "
		dc.b	"                    "
	;<erem

		dc.b	CMD_WAIT, 3		; wait
		dc.b	CMD_IMAGE_IN, 2 	; show image 2
		dc.b	CMD_WAIT, 5		; wait
		dc.b	CMD_IMAGE_OUT	 	; erase image

		;	"####################"
		dc.b	CMD_SETCOLOR
		dc.w	$321

		dc.b	"AMAZING CAT FACT #1:"
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"                    "
		dc.b	"   A fully grown    "
		dc.b	"   male tiger can   "
		dc.b	"   eat up to        "
		dc.b	"   1.2 tons of      "
		dc.b	"   plankton         "
		dc.b	"   in a day.        "
		dc.b	"                    "
		dc.b	CMD_WAIT, 4
		dc.b	"                    "
		dc.b	"   That's a lot of  "
		dc.b	"   fucking plankton!"
		dc.b	CMD_TEXT
		dc.b	CMD_TEXT_OUT

		;	"####################"
		dc.b	CMD_SETCOLOR
		dc.w	$123
		dc.b	"AMAZING CAT FACT #2:"
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"                    "
		dc.b	"  According to this "
		dc.b	"  sentence, the     "
		dc.b	"  cat's closest     "
		dc.b	"  living relative   "
		dc.b	"  is the dolphin.   "
		dc.b	"                    "
		dc.b	"                    "
		dc.b	" Mee-               "
		dc.b	"  -(dolphin noise)- "
		dc.b	"            -yow!!! "
		dc.b	CMD_TEXT
		dc.b	CMD_TEXT_OUT

		;	"####################"
		rem>
		dc.b	"AMAZING CAT FAT #3: "
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"Schroedinger's cat  "
		dc.b	"was the first cat to"
		dc.b	"become the mayor of "
		dc.b	"Akihabara, Japan.   "
		dc.b	"                    "
		dc.b	"Akihabara has since "
		dc.b	"had only three other"
		dc.b	"cat mayors,  the    "
		dc.b	"lowest number in all"
		dc.b	"of Japan.           "
		dc.b	"                    "
		dc.b	"Konnichiwa,Neko-san!"
		dc.b	CMD_TEXT
		dc.b	CMD_WAIT, 2
		dc.b	CMD_TEXT_OUT
		<erem

		dc.b	CMD_SETCOLOR
		dc.w	$142
		dc.b	"AMAZING CAT FAT #3: "
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"The simplest way to "
		dc.b	"melt  a cat  is  to "
		dc.b	"feed it a mixture of"
		dc.b	"mustard and sulfuric"
		dc.b	"acid.               "
		dc.b	"                    "
		dc.b	"The resulting liquid"
		dc.b	"can be used as glue,"
		dc.b	"or as a healthy and "
		dc.b	"delicious alternat- "
		dc.b	"ive to mayonnaise!  "
		dc.b	CMD_WAIT, 2
		dc.b	CMD_TEXT
		dc.b	CMD_TEXT_OUT

		;	"####################"
		dc.b	CMD_SETCOLOR
		dc.w	$314
		dc.b	"AMAZING CAT FAFT #4:"
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"Did you know the    "
		dc.b	"Pink Panther was    "
		dc.b	"in reality not an   "
		dc.b	"actual panther?     "
		dc.b	"                    "
		dc.b	"It was actually a   "
		dc.b	"series of drawings  "
		dc.b	"painted on animation"
		dc.b	"cels and was not a  "
		dc.b	"living being.       "
		dc.b	"                    "
		dc.b	CMD_WAIT, 3
		dc.b	CMD_TEXT
		dc.b	CMD_TEXT_OUT

		;	"####################"
		dc.b	CMD_SETCOLOR
		dc.w	$341
		dc.b	"AMAZING CAT FACT #5:"
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"Cats and ponies are "
		dc.b	"actually  not  as   "
		dc.b	"closely related as  "
		dc.b	"one might think.    "
		dc.b	"                    "
		dc.b	"While their diets   "
		dc.b	"and appearances are "
		dc.b	"similar, ponies are "
		dc.b	"in fact horses while"
		dc.b	"cats are mammals.   "
		dc.b	"                    "
		dc.b	"        Oink!       "
		dc.b	CMD_WAIT, 5
		dc.b	CMD_TEXT
		dc.b	CMD_TEXT_OUT

		;	"####################"
		dc.b	CMD_SETCOLOR
		dc.w	$014
		dc.b	"CACT #6:            "
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"In 1954, a cat      "
		dc.b	"jumped off an       "
		dc.b	"airplane and fell   "
		dc.b	"safely onto its feet"
		dc.b	"from a height of    "
		dc.b	"3 kilometers!       "
		dc.b	"                    "
		dc.b	CMD_WAIT, 3
		dc.b	"It died immediately "
		dc.b	"upon impact.        "
		dc.b	"                    "
		dc.b	CMD_WAIT, 1
		dc.b	"                    "
		dc.b	"    Gesundheit!!    "
		dc.b	CMD_TEXT
		dc.b	CMD_TEXT_OUT

		;	"####################"
		dc.b	CMD_SETCOLOR
		dc.w	$300
		dc.b	"AMAZING CAT FART #7:"
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"There are approx.   "
		dc.b	"17  million  cats   "
		dc.b	"(3.1 tons or 63 sqm)"
		dc.b	"per every living    "
		dc.b	"person on earth.    "
		dc.b	"                    "
		dc.b	"If you stacked them "
		dc.b	"all on top of each  "
		dc.b	"other,  the stack   "
		dc.b	"would fall over at  "
		dc.b	"some point!         "
		dc.b	CMD_WAIT, 4
		dc.b	CMD_TEXT
		dc.b	CMD_TEXT_OUT

		;	"####################"
		dc.b	CMD_SETCOLOR
		dc.w	$123
		dc.b	"CAT FACT #8-10:     "
		dc.b	"The largest cat in  "
		dc.b	"the world,  Jimmy,  "
		dc.b	"has a  2  litre,  4 "
		dc.b	"cylinder common-rail"
		dc.b	"diesel  engine  that"
		dc.b	"produces  184  PS   "
		dc.b	"(181 bhp, 135 kW) of"
		dc.b	"power and 380 N-m of"
		dc.b	"torque at 1750-2750 "
		dc.b	"rpm. It is available"
		dc.b	"with  either  a  six"
		dc.b	"speed manual  or 8- "
		dc.b	"speed automatic box."
		dc.b	CMD_WAIT, 4
		dc.b	CMD_TEXT
		dc.b	CMD_TEXT_OUT

		;	"####################"
		dc.b	CMD_SETCOLOR
		dc.w	$036
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"  CREDITS FOR THIS  "
		dc.b	"     MULTIMEDIA     "
		dc.b	"    EXTRAVAGANZA    "
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	" hukka              "
		dc.b	"    MADE THE THINGS "
		dc.b	"                    "
		dc.b	" wuffe              "
		dc.b	"      DID THE MATHS "
		dc.b	"                    "
		dc.b	" muzzy              "
		dc.b	"      GAVE THE IDEA "
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	CMD_TEXT
		dc.b	CMD_WAIT, 2

		;	"####################"
		dc.b	CMD_SETCOLOR
		dc.w	$146
		dc.b	"WE TIP OUR FEDORAS @"
		dc.b	"^^^^^^^^^^^^^^^^^^^^"
		dc.b	"  ARCHYX            "
		dc.b	"   CRANK            "
		dc.b	"    HAIKZ           "
		dc.b	"     IRAH           "
		dc.b	"      JAAKKOR       "
		dc.b	"       KAARLO       "
		dc.b	"        LARKKU      "
		dc.b	"         STINGRAY   "
		dc.b	"          SVENONACID"
		dc.b	"           TSR      "
		dc.b	"            VEDOS   "
		dc.b	"AND THE AMIGA SCENE!"
		dc.b	CMD_TEXT
		dc.b	CMD_WAIT, 9
		dc.b	CMD_TEXT_OUT

		dc.b	CMD_END

; -------------------------------------------------------------------------------------------------

	SECTION FONT, DATA_C

font:		incbin	"data/font16x12.raw"	; 16x12 font; 182 bytes wide
;font:		incbin	"data/fontq.raw"	; 16x12 font; 118 bytes wide

mask:		incbin	"data/fontmask.raw"	; 8 tiles of 16x12 masks; 16 bytes wide

pic1:		incbin	"data/picture.raw"	; 288x212x1p (18x13 16*16tiles, 18x17 font tiles)

***************************************************************************************************
