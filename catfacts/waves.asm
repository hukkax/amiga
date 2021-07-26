***************************************************************************************************
* Wave effect
* code adapted by hukka from pseudocode by wuffe; exec 2014
*
; -------------------------------------------------------------------------------------------------

	SECTION CODE

	xref	P61_E8

GRADIENTS_ON	= 0
GRADIENTS_OFF	= 255
FLASH_DURATION	= 3

; -------------------------------------------------------------------------------------------------

Waves:
	move.w	P61_E8, d0	; $E8x command triggered in song?
	beq.s	.start

	; 802 = flash waves
	; 804 = enable gradients
	; 806 = disable gradients
	clr.w	P61_E8

	cmp.b	#4, d0
	beq	.e82
	cmp.b	#2, d0
	beq	.e81

.e83	ifd	QUICKSTART
	bra	.e82			; disable gradient disabling!
	endif
	move.b	#GRADIENTS_OFF, Flash
	bra	.fade

.e81	move.b	#FLASH_DURATION, Flash
	bra	.fade

.e82	move.b	#GRADIENTS_ON, Flash
	move.b	#1, Started
	bra	.fade

	; handle copperlist doublebuffering
	;
.start	ifz.b	Flash, .fade
	cmp.b	#GRADIENTS_OFF, Flash
	beq	.fade
	sub.b	#1, Flash

.fade	ifd	FXFADES
	add.b	#1, ColorCtr
	cmp.b	#50, ColorCtr
	bne	.lto
	move.b	#0, ColorCtr

	move.l	ColorPtr, a0
	move.w	(a0)+, d0
	ifnz.w	d0, .fok
	lea	ColorFades, a0

.fok	move.l	a0, ColorPtr
	move.w	d0, WaveColorOrig
	move.w	d0, WaveColor
	endif

.lto	ifz	CurrList, .lto1
.lto0	move.b	#0, CurrList
	move.l	bplLines1, a4
	lea	CopperList2, a0
	bra	.setc

.lto1	move.b	#1, CurrList
	move.l	bplLines2, a4
	lea	CopperList1, a0
.setc	move.l	a0, COP1LCH+CUSTOM


	; d0 	phase
	; d2,d3	a, b
	; d1	x
	; d4	y
	;
	zero	d0
	move.b	phase, d0
	zero	d4		; for y = 0 to 255

	move.w	WaveColorOrig, d6

.y	; a := phase + (y >> 1)
	;
;	zero	d2		; a
	move.b	d4, d2		; a=y
	asr.w	#1, d2		; y >> 1		*** "blob"
	add.b	d0, d2		; + phase

	; b := phase + (y << 1) + y
	;
;	zero	d3		; b
	move.b	d4, d3		; b=y
;	asl.w	#1, d3		; y << 1		*** "y scale"
	add.b	d0, d3		; + phase
	add.b	d4, d3		; + y

	; x := mTable[a + (b << 8)]
	;
	zero	d5
	move.b	d3, d5		; b
	lsl.w	#8, d5		; b << 8
;	lsl.w	#1, d5		; b << 8		*** "y scale" too
	add.w	d5, d5
	or.b	d2, d5		; + a
	lea	mTable, a1
	add.l	d5, a1
	moveq	#0, d1
	move.b	(a1), d1	; x := mTable[index]

.plot	; plot line 0..x
	;
;	BGColor	$600

	lea	linesY, a0
	add.l	d1, a0
	add.l	d1, a0
	add.l	d1, a0
	add.l	d1, a0
	move.l	(a0), d5	; get line address

	; write to copperlist
	;
	swap	d5
	move.w	d5, (a4)	; dc.w BPL1PTH, 0	; 2 + 2 bytes
	add.l	#4, a4
	swap	d5
	move.w	d5, (a4)	; dc.w BPL1PTL, 0	; 2 + 2 bytes

	ifd	FXCOLOR
	addq	#4, a4

	cmp.b	#GRADIENTS_OFF, Flash
	bne	.grad

.nograd	move.w	#$136, (a4)
	bra	.cd

.grad	lea	colors, a0
	zero	d5
	move.b	d1, d5
	add.l	d5, a0
	add.l	d5, a0
	move.w	(a0), (a4)

	ifnz.b	Flash, .cd

.nofl	sub.w	d6, (a4)	; no flash
	endif

.cd	add.l	#8, a4		; COPPER_WAIT_NEXT	; 4 bytes

	addq	#1, d4
	cmp.b	#HEIGHT, d4
	bne	.y		; next y

	addq	#2, d0		; phase++
	move.b	d0, phase

	rts

***************************************************************************************************
* Initialization
*
Waves_Init:
	bsr	BuildSTable
	bsr	BuildMTable
	bsr	GenerateCoppers
	rts

***************************************************************************************************
* Precalc tables
*
BuildSTable:
	; copy first 64 words reversed into following 64 words
	;
	lea	sTabS, a0	; source
	lea	sTable, a1	; dest, straight copy
	move.l	a1, a2		; dest, rev. copy
	add.l	#128*2, a2

	moveq	#64-1, d0
.c1	move.w	(a0)+, d1
	move.w	d1, (a1)+
	move.w	d1, -(a2)
	dbf	d0, .c1

	; copy the whole 128 words into the next 128 words, negating values
	;
	lea	sTable, a0	; source
	move.l	a0, a1
	add.l	#128*2, a1	; dest

	moveq	#128-1, d0
.c2	move.w	(a0)+, d1
	neg.w	d1
	move.w	d1, (a1)+
	dbf	d0, .c2

	rts

; -------------------------------------------------------------------------------------------------

BuildMTable:
	lea	sTable, a0
	zero	d0		; a
.aa	lea	sTable, a1
	zero	d1		; b
.bb	zero	d4
	zero	d5
	move.w	(a0), d4	; sTable[a]
	move.w	(a1)+, d5	; sTable[b]
	muls.w	d4, d5		; d5 := sTable[a] * sTable[b]
	asr.l	#7, d5		; d5 := d5 >> 14
	asr.l	#7, d5		;
	add.l	#80, d5		; d5 := d5 + 80

	lea	mTable, a3	; mTable[a + (b << 8)] := d5
	zero	d2
	move.b	d1, d2		; b
	asl.w	#8, d2		; b << 8
	add.w	d0, d2		; + a
	add.l	d2, a3
	move.b	d5, (a3)	; mTable[]

	addq	#1, d1		; b++
	cmp.w	#256, d1
	bne	.bb		; next b

	addq	#2, a0		; sTable[]
	addq	#1, d0		; a++
	cmp.w	#256, d0
	bne	.aa		; next a

	rts

***************************************************************************************************
* Generate copperlist shell for wave effect
*
GenerateCoppers:
	; copy the copperlist stub to CopperList1
	;
	lea	CopperListSrc, a0		; copy from here
	lea	CopperList1, a1			; to here
	move.l	#COffs_bplLines-1, d0		; this many bytes
.copy1	move.b	(a0)+, (a1)+
	dbf	d0, .copy1

	; fill in the rest of CopperList1
	;
	bsr	GenerateCopList

	; clone the completed copperlist to CopperList2
	;
	lea	CopperList1, a0			; copy from here
	lea	CopperList2, a1			; to here
	move.l	#COPPERLISTSIZE-1, d0		; this many bytes
.copy2	move.b	(a0)+, (a1)+
	dbf	d0, .copy2

	lea	CopperList1, a0
	add.l	#COffs_bplLines, a0
	add.l	#2, a0
	move.l	a0, bplLines1

	lea	CopperList2, a0
	add.l	#COffs_bplLines, a0
	add.l	#2, a0
	move.l	a0, bplLines2

;	WaitForVBL
	lea	CopperList1, a0
	move.l	a0, COP1LCH+CUSTOM		; initialize Copperlist
	move.b	#0, CurrList

	rts

; -------------------------------------------------------------------------------------------------

GenerateCopList:
	zero	d0
	lea	CopperList1, a0
	add.l	#COffs_bplLines, a0

.yy	move.w	#BPL1PTH, (a0)+		; dc.w BPL1PTH, 0
	move.w	#0, (a0)+
	move.w	#BPL1PTL, (a0)+		; dc.w BPL1PTL, 0
	move.w	#0, (a0)+

	ifd	FXCOLOR
	move.w	#COLOR01, (a0)+
	move.w	#$456, (a0)+
	endif

	cmp.b	#74, d0
	bpl	.bb
.tt	move.l	#$00DF80FE, (a0)+	; COPPER_WAIT_NEXT
	bra	.nn
.bb	move.l	#$80DF80FE, (a0)+	; COPPER_WAIT_NEXTD

.nn	addq	#1, d0
;	cmp.b	#HEIGHT, d0
;	blo	.yy
	ifnz.b	d0, .yy

	move.w	#BPLCON0, (a0)+
	move.w	#$0200, (a0)+		; Screen off

	move.l	#$FFFFFFFE, (a0)+	; COPPER_END
	move.l	#$FFFFFFFE, (a0)
	rts

***************************************************************************************************

	SECTION DATA

	ifd	FXCOLOR
colors:	dc.w	$456,$467,$567,$556,$456,$567,$457,$467,$567,$467
	dc.w	$566,$556,$567,$566,$456,$566,$567,$557,$456,$557
	dc.w	$567,$577,$678,$567,$667,$678,$567,$567,$678,$567
	dc.w	$678,$567,$678,$678,$567,$678,$678,$678,$567,$678
	dc.w	$678,$678,$789,$678,$678,$789,$678,$678,$789,$678
	dc.w	$789,$678,$789,$789,$678,$789,$789,$789,$678,$789
	dc.w	$789,$789,$89A,$789,$789,$89A,$789,$789,$89A,$789
	dc.w	$89A,$789,$89A,$89A,$789,$89A,$89A,$89A,$789,$889
	dc.w	$899,$89A,$99A,$89A,$8AA,$9AB,$8AB,$89A,$9AA,$89A
	dc.w	$9AB,$89A,$9AB,$9AB,$89A,$9AB,$9AB,$9AB,$89A,$9AB
	dc.w	$9AB,$9BB,$ABC,$9BB,$9BC,$ABC,$9AB,$9AB,$ABC,$9AB
	dc.w	$ABC,$9AB,$ABC,$ABB,$9AB,$ABC,$BBC,$ABC,$9AB,$ABC
	dc.w	$ABC,$ABC,$BCD,$ACC,$ABC,$BCC,$ABC,$ABD,$BCD,$BBC
	dc.w	$BCD,$ABC,$BCD,$BCD,$ABC,$BCD,$BCD,$BCD,$ABC,$BCD
	dc.w	$BCD,$BCD,$CDE,$BCD,$BCD,$CDE,$BCD,$BCD,$CDE,$BCD
	dc.w	$CDE,$BCD,$CDE,$CDE,$BCD,$CDE,$CDE,$CDE,$BCD,$CDE
	rem>
colors:	dc.w	$456,$456,$567,$456,$456,$567,$456,$456,$567,$456
	dc.w	$567,$456,$567,$567,$456,$567,$567,$567,$456,$567
	dc.w	$567,$567,$678,$567,$567,$678,$567,$567,$678,$567
	dc.w	$678,$567,$678,$678,$567,$678,$678,$678,$567,$678
	dc.w	$678,$678,$789,$678,$678,$789,$678,$678,$789,$678
	dc.w	$789,$678,$789,$789,$678,$789,$789,$789,$678,$789
	dc.w	$789,$789,$89A,$789,$789,$89A,$789,$789,$89A,$789
	dc.w	$89A,$789,$89A,$89A,$789,$89A,$89A,$89A,$789,$89A
	dc.w	$89A,$89A,$9AB,$89A,$89A,$9AB,$89A,$89A,$9AB,$89A
	dc.w	$9AB,$89A,$9AB,$9AB,$89A,$9AB,$9AB,$9AB,$89A,$9AB
	dc.w	$9AB,$9AB,$ABC,$9AB,$9AB,$ABC,$9AB,$9AB,$ABC,$9AB
	dc.w	$ABC,$9AB,$ABC,$ABC,$9AB,$ABC,$ABC,$ABC,$9AB,$ABC
	dc.w	$ABC,$ABC,$BCD,$ABC,$ABC,$BCD,$ABC,$ABC,$BCD,$ABC
	dc.w	$BCD,$ABC,$BCD,$BCD,$ABC,$BCD,$BCD,$BCD,$ABC,$BCD
	dc.w	$BCD,$BCD,$CDE,$BCD,$BCD,$CDE,$BCD,$BCD,$CDE,$BCD
	dc.w	$CDE,$BCD,$CDE,$CDE,$BCD,$CDE,$CDE,$CDE,$BCD,$CDE
	<erem
	endif

sTabS:	; sTable is generated from these 128 bytes
	dc.w	0000, 0025, 0050, 0075, 0100, 0125, 0150, 0175, 0199, 0224, 0248, 0273, 0297, 0321, 0344, 0368
	dc.w	0391, 0414, 0437, 0460, 0482, 0504, 0526, 0547, 0568, 0589, 0609, 0629, 0649, 0668, 0687, 0706
	dc.w	0724, 0741, 0758, 0775, 0791, 0807, 0822, 0837, 0851, 0865, 0878, 0890, 0903, 0914, 0925, 0936
	dc.w	0946, 0955, 0964, 0972, 0979, 0986, 0993, 0999, 1004, 1008, 1012, 1016, 1019, 1021, 1022, 1023

phase:		dc.w	0
CurrList:	dc.w	0			; current copper list 0/1
Flash:		dc.b	GRADIENTS_OFF
		even

		ifd	FXFADES
ColorFades:	dc.w	$321,$322,$222,$223
		dc.w	$123,$133,$132
		dc.w	$142,$132,$133,$123,$223,$224,$214
		dc.w	$314,$313,$312,$302,$301
		dc.w	$300,$200,$101,$102,$112,$113,$114
		dc.w	$014,$123,$232,$241
		dc.w	$341,$331,$231,$222,$212
		dc.w	$112,$123,$125,$136
		dc.w	$036,$136
		dc.w	$146,$135,$134,$233,$232,$222,$322
		dc.w	0

ColorPtr:	dc.l	0
ColorCtr:	dc.b	0
		even
		endif

; -------------------------------------------------------------------------------------------------

	SECTION TABLES, BSS

BackY:		blk.l	256			; addresses of rasterlines in BufferBack
linesY:		blk.l	160			; addresses of rasterlines in lines

sTable:		blk.w	256			; used in generating mTable

mTable:		blk.b	65536			; multiplication table

; -------------------------------------------------------------------------------------------------

	SECTION WAVES, DATA_C

lines:		incbin	"data/liness.raw"	; rasterlines of the wave effect

***************************************************************************************************
