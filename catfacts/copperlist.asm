***************************************************************************************************
* Copperlist stub for Aaltro intro
*
	align32

CopperListSrc:	dc.w FMODE,	$0000			; AGA slow fetch mode
		dc.w BPLCON3,	$0C00 			; AGA bitplane control
		;dc.w BPLCON4,	$0011			; AGA sprite palettes
		dc.w BPLCON0, 	$0200			; Screen off
		dc.w BPLCON1,	2			; Offset shadow by scrolling 2 pixels horizontally
		dc.w BPLCON2,	$64			; Playfield 2 and sprites on top

		dc.w DIWSTRT,	$2C81			; Display window start (top left)
		dc.w DIWSTOP,	$2CC1			; Display window stop (bottom right) 16C1
		dc.w DDFSTRT,	$0038			; Display data fetch start (horiz.)
		dc.w DDFSTOP,	$00D0			; Display data fetch stop  (horiz.)

		dc.w BPL1MOD,	0			; Bitplane modulo (odd  planes)
		dc.w BPL2MOD,	0			; Bitplane modulo (even planes)

		COPPER_WAIT 	53,7			; Wait for screen top

	;rem>
Palette:	ifnd	DEBUG
		COPPER_SETCOLOR 0, $247			; background
		endif
		COPPER_SETCOLOR 1, $358			; waves
		COPPER_SETCOLOR 2, 0 ;$024		; shadow over bg
		COPPER_SETCOLOR 3, 0; $432		; shadow over waves
		COPPER_SETCOLOR 9, $FFF			; text
	;<erem

bplWave:	dc.w BPL1PTH, 0, BPL1PTL, 0		; Bitplane pointers
bplText:	dc.w BPL3PTH, 0, BPL3PTL, 0		;
bplShadow:	dc.w BPL2PTH, 0, BPL2PTL, 0		;

		COPPER_WAIT_NEXT

		dc.w BPLCON0,	PLANES_3|DBLPF|COLOR	; Screen begins

bplLines:
;		ifd	FXCOLOR
;		ds.l	(HEIGHT*4)+2
;		else
;		ds.l	(HEIGHT*3)+2
;		endif

bplEnd:		COPPER_END

***************************************************************************************************

; offsets to bplLines from the beginning of copperlist stub
;
COffs_bplLines	= bplEnd-CopperListSrc

; size of a complete copperlist in bytes
;
	ifd FXCOLOR
COPPERLISTSIZE = COffs_bplLines+(2*4)+(256*4*4)+2 ; HEIGHT*4*4 for FXCOLOR
	else
COPPERLISTSIZE = COffs_bplLines+(2*4)+(256*3*4)+2
	endif

***************************************************************************************************

	SECTION	COPPER, BSS_C ;DATA_C

;		dc.b	'SIZE'
		dc.l	COPPERLISTSIZE

;		dc.b	'BPLS'
bplLines1:	dc.l	0
bplLines2:	dc.l	0

;		dc.b	'COP1'
CopperList1:	blk.b	COPPERLISTSIZE
;		even
;		dc.b	'COP2'
CopperList2:	blk.b	COPPERLISTSIZE
;		even

***************************************************************************************************
