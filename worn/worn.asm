; -----------------------------------------------------------------------------
; Worn: a bootblock for Amiga OCS/AGA (vasm)
; by hukka/void^exec^svenonacid^ftg Mar 2020
; -----------------------------------------------------------------------------

RELEASE        = 0	; generate final bootblock?
AUDIOFILTER    = 1	; filter on or off?
AUDIOWOBBLE    = 1	; "worn tape" unstable pitch effect?
MORE_EFX_MASKS = 0	; spend 15 bytes for double the efx variations?
SNARE          = 0	; crappy "snare" on the bass channel?
EXITABLE       = 0	; include exit code?

BG = $111

; -----------------------------------------------------------------------------

	machine 68000

	if	RELEASE = 0
		opt	a+,o+,l+			; optimize
		;opt	ow+				; show optimizations
		include	"startup.asm"
	else
		opt	a+,o+,p+			; optimize
	endc

	opt	ob+,oc+,od+,og+,oj+,ol+,om+,on+,ox+	; vasm specific

	include "custom.i"

; -----------------------------------------------------------------------------

	macro push
		movem.l \1, -(sp)
	endm

	macro pop
		movem.l (sp)+, \1
	endm

	macro echosize
.\@		echo	\1
		printv	.\@ - \2
	endm

	macro printsize
.\@		printv	.\@ - \1
	endm

; -----------------------------------------------------------------------------

	if	RELEASE = 1
_LVOFindResident= -96
gb_ActiView	= 34
gb_copinit	= 38
	endc

SCREENWIDTH	= 320
SCREENHEIGHT	= 256
SCREENDEPTH	= 2		; number of bitplanes
YPADDING	= 40

ENDBYTE		= $FE		; end marker for nybblepacked data

MAXSONGLOOPS	= 4		; times to loop song before resetting vfx
SONGTICKS	= 5		; song tempo, higher=slower
PATTERNSIZE	= 128*2

FADESPEED	= 1		; melody instrument fade speed (higher=faster)
FADESPEED_BASS	= 4		; bass instrument fade speed

VOLUME_BASS	= 64
VOLUME_MELODY	= 64

S_VOL		= 0		; volume
S_FADEY		= 1		; fade yes
S_FADEN		= 2		; fade no
CHANSIZE	= 2		; channel struct length
CHANNELS_ON	= (DMAF_SETCLR!DMAF_AUD0!DMAF_AUD1!DMAF_AUD2!DMAF_AUD3!DMAF_MASTER)

	SECTION BBLK, CODE

; -----------------------------------------------------------------------------
; Testing wrapper - for running bootblock during development
;
	if	RELEASE = 0
Main:		bsr	Init			; Call the bootblock

Quit:		lea	CUSTOM, a6
		move.w	#$8020, DMACON(a6)	; re-enable sprite DMA
		moveq	#0, d0			; errorcode=0
		rts
	endc

*******************************************************************************
; The bootblock starts here. It cannot generally be assumed to be placed in a
; specific kind of memory, or at any specific address, so all code must be
; PC relative, and all chip data must be explicitly copied to chip memory.
; -----------------------------------------------------------------------------
; A4 = ChipSpace
; A6 = Custom

BB_START:
	DC.B	'DOS',0
	DC.L	0			; checksum will go here
	DC.L	880

Init:
	if EXITABLE = 1
	push d0-a6
	endc

	lea	CUSTOM, a6
	move.l	#ChipSpace, a4

	move.l	a4, a2			; ChipSpace
	move.l	#SPACE_SIZE, d2
.clear	move.b	#0, (a2)+		; clear chipspace
	dbf	d2, .clear

	bsr	ResetBpls

	; Init audio channel structures
InitAudio:
	move.l	a6, a5			; CUSTOM
	lea	Sample2(a4), a3
	lea	Channel0(pc), a2	; channel info
	moveq	#4-1, d0		; channel #
.chinit move.l	a3, AUD0LCH(a5)		; sample location
	move.w	(a2)+, AUD0LEN(a5)	; sample length in words
	add.w	#$10, a5		; next dest regs
	dbf	d0, .chinit

	lea	Sample1(a4), a3		; bass channel
	move.l	a3, AUD3LCH(a6)

	if	AUDIOFILTER=0
	bset.b	#1, PRA_A		; turn off audio filter
	endc

Inited:
	if	SNARE = 1
	lea	Fadespeeds(pc), a1
	lea	Sample3(a4), a0
	moveq	#64-1, d0
.cpsn	move.w	(a1)+, (a0)+
	dbf	d0, .cpsn
	endc

	move.w	#$4000, INTENA(a6)	; disable interrupts and set some DMAs
	move.w	#$87CF, DMACON(a6)

;	echosize "Init:", Init

Unpack: ; unpack samples
	lea	Sample1(a4), a1
	lea	Sample2(a4), a2
;	lea	Sample1RLE(pc), a0
;	bsr	UnpackSample
	lea	Sample1RLE(pc), a0
;	bsr	UnpackSample
; -----------------------------------------------------------------------------
; a0 = rle-packed sample data
; a1,a2 = destination
;
UnpackSample:
.copy	move.b	(a0)+, d0	; value
	cmp.b	#ENDBYTE, d0
	beq.b	.done
.rle	moveq	#0, d1
	move.b	(a0)+, d1	; length
.fill	move.b	d0, (a1)+	; sample1
	move.b	d0, (a2)+	; sample2
	dbf	d1, .fill
	bra.b	.copy
.done	;rts

	; unpack pattern data
	lea	NotesMelo0_0(pc), a0	; packed pattern data
	lea	P00_0(a4), a1		; unpack here
	;bsr	UnpackPatterns

; -------------------------------------------------------------------
; Unpacks a 32-row pattern
;
; a0 = notedata (rhythm# byte, run length + periodtab offset nybbles)
; a1 = pointer to patterndata dest. row

UnpackPatterns:
	moveq	#4*8-1, d6
.yeet	moveq	#0, d5
	move.b	(a0)+, d5	; rhythm #
	mulu.w	#4, d5		; 4 bytes per rhythm
	lea	Rhythms(pc), a2	; ptr to rhythm bitpatterns
	move.l	(a2,d5.w), d4	; get rhythm pattern

	moveq	#0, d3		; run length counter
	moveq	#32-1, d2	; row counter
.row	rol.l	#1, d4		; get next bit in rhythm pattern
	btst	#0, d4		; bit set?
	beq.b	.loop		; no, don't put new byte

	tst.b	d3		; need to read next byte?
	bne.b	.put		; no

.read	move.b	(a0), d3	; get run length + note#
	move.w	d3, d5
	and.b	#%1111, d5	; d5 = note#
	addq	#1, d5
	lsr.b	#4, d3		; d3 = run length
	and.b	#%1111, d3	; req?
	tst.b	d3		; or are we done with this pattern?
	beq.b	.fff		; yes

	addq	#1, a0

	cmp.b	#$F, d3
	bne.b	 .put
	addq	#1, d3

.put	move.b	d5, (a1)
	subq	#1, d3

.loop	addq	#1, a1
	dbf	d2, .row

.fff	dbf	d6, .yeet
	;rts

*******************************************************************************
* Main loop
*******************************************************************************

MainLoop:

.WaitVBL:
	tst.b	VPOSR+1(a6)
	beq.b	.WaitVBL
.wvb:	tst.b	VPOSR+1(a6)
	bne.b	.wvb

	move.w	#$0020, DMACON(a6)	; switch off sprite DMA

	bsr	PlayMusic

	rem>
	lea	wobbleamt(pc), a1
	move.b	row(pc), d0
;	move.b	Sample2(a4), d0
;	asr.b	#7, d0
	and.b	#%111, d0
	or.b	#%11, d0
	move.b	d0, (a1)
	<erem

	lea	tilepattern(pc), a5
;	move.w	Sample1(a4), (a5)
	move.w	(a5), d5		; rotate tile pattern

	; Draw logo			Screen in a3, Bits in a2
DrawLogo:
	lea	Channel0(pc), a1
	lea	Logo(a4), a3		; logo pixel bits
	move.l	a3, a2
;	lea	Sample1(a4), a5

	;move.b	wobbleamt(pc), (a3)+
	move.b	efxpos(a4), (a3)+
	move.b	(a1), (a3)+
	move.b	2(a1), (a3)+
	move.b	6(a1), (a3)+
	move.b	row(pc), (a3)

	lea	Screen+(SCREENWIDTH/8*YPADDING)+40*91+12(a4), a3
	moveq	#5-1, d0		; y size
.y
	rol.w	#1, d5
	move.w	d5, (a5)

	moveq	#16-1, d1		; x size
	move.b	(a2)+, d2		; pixel bits

.x	moveq	#0, d3			; outpixel
	btst	#0, d2
	beq.b	.no			; no pixel

;	move.w	(a5)+,d3
;	subq	#1, d3			; = move.w #%1111111111111111, d3
	move.w	d5, d3
;	move.w	#%1111111011111110, d3

.no	moveq	#8-1, d4
	move.l	a3, a1
.cy	move.w	d3, (a1)
	add.w	#80, a1			; advance 2 scanlines
	dbf	d4, .cy
	addq	#2, a3

	asr.w	#1, d2			; next pixel bit
	dbf	d1, .x
	add.w	#488+40, a3
	dbf	d0, .y

	; ---------------------------------------------------------------------

	move.l	#$1ff00, d6

.wait	move.l	VPOSR(a6), d0
	and.l	d6, d0
	cmp.w	#130<<8, d0
	bmi.b	.wait

	move.w	#BG, d7
	lea	Copper(a4), a1
	lea	Sample1(a4), a0
	move.b	SongLoops(a4), d1

.d	moveq	#0, d0
	move.b	efxpos(a4), d0
	add.w	d0, a0

.line	move.b	(a0)+, d2
	add.b	row(pc), d2
	and.w	#119, d2		; 51,115,55,119

;	btst	#0, d1			; songloops even?
;	bne.b	.scroll
;.fx2	and.w	d0, d2			; !!!

.scroll	move.b	d2, BPLCON1+1(a6)	; horizontal scroll

	move.b	(a0), d3
	and.w	#%1, d3
	;asl.w	#8, d3	; red
	add.w	d7, d3
	move.w	d3, COLOR00(a6) ; 27*2(a1)

	move.l	VPOSR(a6), d3
	and.l	d6, d3
	cmp.w	#210<<8, d3
	bpl	MainLoop

	; ---------------------------------------------------------------------

	if EXITABLE = 0

		bra.b	.line

	else

		btst.b	#6, PRA_A		; loop until mouse clicked
		bne	.line

Exit:		move.w	#$000F, DMACON(a6)	; turn off sound
		pop	d0-a6			; restore registers

		if	RELEASE = 0		; assembled as executable
		rts
		endc

		move.l	IVBLIT(a6), a0		; restore copper
		move.l	gb_copinit(a0), CUSTOM+COP1LCH

		; Return init function of dos.library resident in A0
		lea	DosName(pc), a1
		jsr	_LVOFindResident(a6)
		move.l	d0, a0
		move.l	RT_INIT(a0), a0
		moveq	#0, d0

		if	RELEASE = 1
		rts
		endc

DosName	dc.b	'dos.library',0

	endc	; exitable

ResetBpls:
	; set bitplane pointers
	lea	Screen+(SCREENWIDTH/8*YPADDING)(a4), a0
;	move.l	a0, a3			; store screen loc for future use
	;subq	#1, a0
	lea	bpl1+2(pc), a1
	bsr	SetBPLPtrs
	addq	#8, a1
;	sub.w	#40*4, a0
	bsr	SetBPLPtrs

	; copy copperlist to chip
	lea	CopperData(pc), a0
	lea	Copper(a4), a1
	move.l	a1, COP1LCH(a6)	; set copperlist
	moveq	#(CopperData_End-CopperData)/4-1, d6
.copy	move.l	(a0)+, (a1)+
	dbf	d6, .copy

	;move.b	#0, SongLoops(a4)	; reset song
	move.w	#0, wantfade(a4)	; reset song & enable intro section

	rts

*******************************************************************************
* Music playroutine
*******************************************************************************

PlayMusic:
	moveq	#0, d0
	;lea	wantfade(a4), a2
	lea	ticks(a4), a2		; also used to address wantfade
	move.b	(a2), d0
	;tst.b	d0			; enough ticks passed?
	bne	.fade			; no
	move.b	#SONGTICKS, (a2)+	; yes; reset tick counter and advance

	lea	row(pc), a1
	move.b	(a1), d0
	addq.w	#1, d0			; advance to next row in pattern

	cmp.b	#PATTERNSIZE/2, d0
	beq	.half
	cmp.w	#PATTERNSIZE, d0	; pattern played through?
	bne.b	.rok			; not yet
.restart
	if	MORE_EFX_MASKS = 1
	lea	efxmasks(pc), a3	; change effect mask type
	move.w	(a3), d0		; (4 different 4-bit patterns)
	rol.w	#4, d0
	move.w	d0, (a3)
	else
	lea	efxmaskval(pc), a3
	eor.b	#%1000, (a3)
	endc

	lea	SongLoops(a4), a3
	addq.b	#1, (a3)
	cmp.b	#MAXSONGLOOPS, (a3)	; reset fx when looped this many times
	bne.b	.nore
	push	a1
	bsr	ResetBpls		; reset bpls and SongLoops
	pop	a1

.nore	moveq	#0, d0			; loop to pattern start
.nore2	add.w	#40, Copper+BPLPTRS+6+8(a4) ; offset bitplane
;	lea	Copper(a4), a0
;	add.w	#$111, 27*2(a0)

.rok	move.b	d0, (a1)		; store row number
	lea	P00_0(a4), a1		; patterndata
	moveq	#4-1, d4		; channel #
	lea	Channel0(pc), a0
	move.l	a6, a5			; CUSTOM
.chan	moveq	#0, d1
	move.b	(a1,d0), d1		; note := patterndata[row]
	;tst.b	d1
	beq.b	.next			; skip if note=0

	; crappy snare on the bass channel, too much code!
	if	SNARE = 1
	tst.b	d4	; bass channel?
	bne.b	.putp	; no
	cmp.b	#2, d1	; snare hit?
	bne.b	.nosn	; no
.snare	move.b	#11, d1
	move.b	#1, issnare
	lea	Sample3(a4), a3		; snare sample
	;lea	ChannelS(pc), a2	; channel info
	bra.b	.psn
.nosn	cmp.b	#1, issnare
	bne	.putp
	move.b	#0, issnare
	lea	Sample1(a4), a3		; bass sample
	;lea	Channel3(pc), a2	; channel info
.psn	move.l	a3, AUD0LCH(a5)		; sample location
	;move.w	(a2), AUD0LEN(a5)	; sample length in words
	endc

.putp	lea	Periods(pc), a3
	subq	#1, d1			; 0-based periodtable access
	add.b	d1, d1
	move.w	(a3,d1), AUD0PER(a5)	; periodtable[note]

	; do a funky "worn tape" wow effect
	if	AUDIOWOBBLE = 1
	move.w	(a3,d1), d7
	move.b	efxpos(a4), d6
	and.b	#%011, d6 ;wobbleamt(pc), d6
	add.b	d6, d7
	move.w	d7, AUD0PER(a5)	; periodtable[note]
	endc

	lea	Fadespeeds(pc), a3
	cmp.b	#1, (a2) ;wantfade
	beq	.fadey

.faden	move.b	(a3,d4.w), S_FADEN(a0)
	bra.b	.maxvol

.fadey	move.b	(a3,d4.w), S_FADEY(a0)
.maxvol	lea	Chanvols(pc), a3
	move.b	(a3,d4.w), d2
.pv	move.b	d2, S_VOL(a0)
	move.w	d2, AUD0VOL(a5)

.next	addq	#CHANSIZE, a0		; next channel's info
	add.w	#PATTERNSIZE, a1	; next channel's pattern data
.last	add.w	#$10, a5		; next channel's Paula regs
	dbf	d4, .chan		; process next channel

.out	move.w	#CHANNELS_ON, DMACON(a6)

.efx	;eor.b	#%1, kekkala(a4)
	;bne	.noefx

	rem>
	lea	efxmaskpos(a4), a5	; advance efx mask type
	addq	#1, (a5)
	and.b	#3, (a5)		; table index 0..3
	move.b	(a5), d0
	lea	efxmask(pc), a3
	move.b	(a3, d0.w), 
	<erem

	lea	efxpos(a4), a5

	lea	Sample1(a4), a3
	move.b	(a5), d0		; dest. pos in sample data
	and.w	#64-1, d0		; sample is 128 bytes long, don't go past it
	add.w	d0, a3
	not.b	(a3)			; simulate ProTracker EFx effect


	if	MORE_EFX_MASKS = 1
	move.w	efxmasks(pc), d1
	and.b	#%1111, d1
	or.b	#%10000, d1
	and.b	d1, d0
	else
	and.b	efxmaskval(pc), d0
	endc

	;and.b	#32-9, d0		; sample is 32 bytes long, don't go past it 9 1 3 5
	lea	Sample2(a4), a3
	not.b	(a3,d0)			; simulate ProTracker EFx effect
	addq.b	#1, (a5)
.noefx	rts

.half:	tst.b	(a2) ;wantfade
	bne	.nore2
	move.b	#1, (a2) ;wantfade
	lea	Chanvols(pc), a0
	move.l	#$40282828, (a0)
	bra	.nore

.domaxvol:
	lea	Channel3+1(pc), a1
	move.b	#5, (a1)
	bra.b	.maxvol

.fade:	lea	Channel0(pc), a3
	move.l	a6, a5
	moveq	#0, d4			; channel #
	moveq	#0, d2
.fac	move.b	S_VOL(a3), d2		; current channel volume

	tst.b	1(a2)			; wantfade
	beq.b	.faden2
.fadey2	sub.b	S_FADEY(a3), d2		; subtract fade speed
	bra.b	.fad
.faden2	sub.b	S_FADEN(a3), d2		; subtract fade speed
.fad	bcs.b	.nxt			; stop fading
	move.b	d2, S_VOL(a3)
	move.w	d2, AUD0VOL(a5)

.nxt	addq	#CHANSIZE, a3
	add.w	#$10, a5
	addq	#1, d4
	cmp.b	#4, d4
	bne.b	.fac
	subq	#1, d0
	move.b	d0, (a2)+		; ticks++; point a2 to wantfade
	bra.b	.efx

	echosize "Playroutine:", PlayMusic ;+32+36+2+8+71

	if	SNARE = 1
issnare:	dc.b 0
		even
	endc

; -----------------------------------------------------------------------------

SetBPLPtrs:	; a0 = bitmap, a1 = location of BPLxPTH in copperlist
	move.l	a0, d0
	move.w	d0, 4(a1)
	swap	d0
	move.w	d0, (a1)
	rts

*******************************************************************************
* Data
*******************************************************************************

DataStart:

COLMOD    = -$111
COLMODBG  = $1
BARHEIGHT = 41

CopperData:
		dc.w	FMODE,	 $0	; AGA slow fetch mode
		dc.w	DIWSTRT, $2C81	; Display window start (top left)
		dc.w	DIWSTOP, $2CB1	; Display window stop (bottom right)
		dc.w	DDFSTRT, $38	; Display data fetch start (horiz.)
		dc.w	DDFSTOP, $D0	; Display data fetch stop  (horiz.)
		dc.w	BPLCON1, $0
		dc.w	BPLCON2, $0	; not required
		dc.w	BPL1MOD, $0
		dc.w	BPL2MOD, $0

bpl1:		dc.w	BPL1PTH, 0, BPL1PTL, 0	; Bitplane pointers
bpl2:		dc.w	BPL2PTH, 0, BPL2PTL, 0	;
		dc.w	BPLCON0, PLANES_2|COLOR ; Screen on

;		COPPER_SETCOLOR	0, BG+COLMODBG		; bar

		; muted candy
		COPPER_SETCOLOR 1, $423+COLMOD	; ->
		COPPER_SETCOLOR 3, $623+COLMOD	; <-
		COPPER_SETCOLOR 2, $642+COLMOD 	; text 327

		; candy
		rem>
		COPPER_SETCOLOR 1, $434+COLMOD	; ->
		COPPER_SETCOLOR 3, $834+COLMOD	; <-
		COPPER_SETCOLOR 2, $861+COLMOD 	; text 327
		<erem


;		COPPER_SETCOLOR 3, $534+COLMOD	; ->
;		COPPER_SETCOLOR 2, $334+COLMOD	; <-
;		COPPER_SETCOLOR 1, $323+COLMOD 	; text 327

;		COPPER_WAIT	240-70-BARHEIGHT,7
;		COPPER_SETCOLOR 0, BGDARK+COLMODBG	; background
		COPPER_WAIT	240-70+BARHEIGHT,7
		COPPER_SETCOLOR 0, BG+COLMODBG	; background

		;dc.w BPLCON0, 	$0200		; Screen off
		COPPER_END

CopperData_End:
		echosize "Copper:", CopperData

; ===========================================================================
;  SONG DATA
; ===========================================================================

; Audio channels

Chanvols:	dc.b	VOLUME_BASS,VOLUME_MELODY,VOLUME_MELODY,VOLUME_MELODY

Channel0:	dc.b	00	; volume
		dc.b	16	; length/fade speed
Channel1:	dc.b	00	; volume
		dc.b	16	; length/fade speed
Channel2:	dc.b	00	; volume
		dc.b	16	; length/fade speed
Channel3:	dc.b	00	; volume
ch3fade:	dc.b	64	; length/fade speed
		; note: next byte gets overwritten by the player!

; Sample data
		; Sample 01 (64 words, 128 bytes): "bass"
Sample1RLE:	dc.b	127,83,115,2,100,6,077,4,055,4
		dc.b	039,3,020,3,12,4,250,10,ENDBYTE

		;echosize "Samples:", Sample1RLE

row:		dc.b	PATTERNSIZE-1 ; currently playing pattern row

		if	MORE_EFX_MASKS = 1
		;-9=0001.0111   -1=0001.1111   -3=0001.1101   -5=0001.1011
efxmasks:	dc.w	%0111111110111111 ; 4x4 bits
		endc

; Period table, 15 words

Periods:	dc.w	604,570,538,508,480,453,428,404 ; 0..7
		dc.w	381,360,339,320,302,285,269	; 8..E

; On which rows to insert notedata?

tilepattern:	dc.w	%1111110011111100

Rhythms:	dc.l	%10101010101010101010101010101010 ; bass P.1+2
		dc.l	%10001000100100100010010010010010 ; melody P.#1
		dc.l	%10001000100100100010000010001000 ; melody P.#2

; Pattern data
; Format: rhythm# followed by run length + periodtab offset nybbles

Fadespeeds:	dc.b	FADESPEED_BASS,FADESPEED,FADESPEED,FADESPEED
NotesMelo0_0:	dc.b	01, $2B,$2C,$3B,$2C,$1B, 01, $29,$2A,$39,$2A,$19
		dc.b	01, $2B,$2C,$3B,$2C,$1B, 01, $29,$2A,$39,$2A,$19
NotesMelo1_0:	dc.b	02, $2D,$2E,$4D, 02, $2D,$2E,$4D
		dc.b	02, $2D,$2E,$4D, 02, $2D,$2E,$4D
NotesMelo0_1:	dc.b	01, $A7, 01, $A5
		dc.b	01, $A7, 01, $A5
NotesMelo1_1:	dc.b	02, $89, 02, $49,$48
		dc.b	02, $89, 02, $49,$48
NotesMelo0_2:	dc.b	01, $A4, 01, $52,$50
		dc.b	01, $A4, 01, $52,$50
NotesMelo1_2:	dc.b	02, $86, 02, $84
		dc.b	02, $86, 02, $84
NotesMelo0_3:	if	SNARE = 1
SN=$11
		dc.b	00, $24, SN, $34, SN, $34, SN, $34, SN, $14
		dc.b	00, $22, SN, $32, SN, $12, $29, SN, $39, SN, $19
		dc.b	00, $24, SN, $34, SN, $34, SN, $34, SN, $14
		dc.b	00, $22, SN, $32, SN, $12, $29, SN, $39, SN, $19
		else
		dc.b	00, $F4, 00, $F2     ; bass
		dc.b	00, $F4, 00, $82,$89 ; bass
		endc
NotesMelo1_3:	dc.b	00, $FE, 00, $89,$8D ; bass
		dc.b	00, $FE, 00, $89,$88 ; bass

;wobbleamt:	dc.b	%011

		if	MORE_EFX_MASKS = 0
efxmaskval:	dc.b	31 ;23 ; toggle bit 3
		endc

;efxmask:	dc.b	23,31,29,27

; ============================================================================

		echosize "Song: ", Chanvols

BB_END:
BB_SIZE = BB_END-BB_START

	;echo "------------"
	echosize "Data:", Channel0
	echosize "Total:", BB_START

	echo "Remaining:"
	printv 1024-BB_SIZE

	if	RELEASE = 1
	if	BB_SIZE<1024
	ds.b	1024 - BB_SIZE, "B"
	endc
	endc

*******************************************************************************
; Put everything that needs to be in chip memory in this section!

Space:		rsreset

Copper:		rs.b	CopperData_End-CopperData

BPLPTRS = bpl1-CopperData

Sample1:	rs.w	64
Sample2:	rs.w	64 ;16
	if	SNARE = 1
Sample3:	rs.w	64
	endc

P00_0:		rs.b	PATTERNSIZE
P00_1:		rs.b	PATTERNSIZE
P00_2:		rs.b	PATTERNSIZE
P00_3:		rs.b	PATTERNSIZE

efxpos:		rs.b	1
ticks:		rs.b	1
wantfade:	rs.b	1	; pos must be word aligned!
SongLoops:	rs.b	1

Screen:		rs.b	SCREENWIDTH/8*(SCREENHEIGHT+(YPADDING*2))

Logo:		rs.b	6
;kekkala:	rs.b	1
;efxmaskpos:	rs.b	0
;efxmaskval:	rs.b	0

SPACE_SIZE:	rs.l	0

ChipSpace = $7FF00-SPACE_SIZE

	rem>
	echo
	echo "SPACE_SIZE="
	printv SPACE_SIZE
	echo
	echo "ChipSpace="
	printv ChipSpace
	<erem

; -----------------------------------------------------------------------------
