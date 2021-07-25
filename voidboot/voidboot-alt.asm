; -----------------------------------------------------------------------------
; VoidBoot: a simple bootblock for Amiga OCS/AGA (vasm)
; by hukka/void Feb/Mar 2018
; -----------------------------------------------------------------------------

RELEASE	 = 0			; generate final bootblock?

; -----------------------------------------------------------------------------

	machine 68000

	if	RELEASE = 0

	opt	a+,o+,l+				; optimize
	opt	ob+,oc+,od+,og+,oj+,ol+,om+,on+,ox+	; vasm specific

	include	"startup.asm"

	else

	opt	a+,o+,p+				; optimize
	opt	ob+,oc+,od+,og+,oj+,ol+,om+,on+,ox+	; vasm specific
	;opt	ow+					; show optimizations

	endc

	include "custom.i"

; -----------------------------------------------------------------------------

	macro push
	movem.l \1, -(sp)
	endm

	macro pop
	movem.l (sp)+, \1
	endm

	macro echosize
.\@	echo	\1
	printv	.\@ - \2
	endm

	macro printsize
.\@	printv	.\@ - \1
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

CC		= $FF		; sets note volume back to maximum
ENDBYTE		= $FE		; end marker for nybblepacked data

SONGTICKS	= 7		; song tempo, higher=slower
PATTERNSIZE	= 128
FADESPEED	= 2
MAXSONGLOOPS	= 6
S_VOL		= 0		; volume
S_FADE		= 1		; fade speed
CHANSIZE	= 2		; channel struct length
CHANNELS_ON	= (DMAF_SETCLR!DMAF_AUD0!DMAF_AUD1!DMAF_AUD2!DMAF_AUD3!DMAF_MASTER)

	SECTION BBLK, CODE

; -----------------------------------------------------------------------------
; Testing wrapper - for running bootblock during development
;
	if	RELEASE = 0

Main:	bsr	Init			; Call the bootblock

Quit:	lea	CUSTOM, a6
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

Init:	movem.l d0-a6, -(sp)

	lea	CUSTOM, a6
	move.l	#ChipSpace, a4

	move.l	a4, a2			; ChipSpace
	move.l	#SPACE_SIZE, d2
.clear	move.b	#0, (a2)+		; Clear chipspace
	dbf	d2, .clear

	bsr	ResetBpls

	; Draw logo			Screen in a3, Bits in a2
DrawLogo:
	;move.l	a3, a5
	lea	Logo(pc), a2		; logo pixel bits
	add.w	#40*91+2, a3		; starting line
	moveq	#5-1, d0		; y size
.y	moveq	#16-1, d1		; x size
	move.w	(a2)+, d2		; pixel bits

.x	moveq	#0, d3			; outpixel
	btst	#0, d2
	beq.b	.no			; no pixel

	subq	#1, d3			; = move.w #%1111111111111111, d3
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

	;echosize "Logo:", DrawLogo

	; Init audio channel structures
InitAudio:
	move.l	a6, a5			; CUSTOM
	lea	Sample2(a4), a3
	lea	Channel0(pc), a2	; channel info
	moveq	#4-1, d0		; channel #
.chinit move.l	a3, AUD0LCH(a5)		; sample location
	move.w	(a2)+, AUD0LEN(a5)	; sample length in words
	;move.w #$0000+FADESPEED, (a2)+ ; set vol+fade; advance to next chan.
	;move.b #64, AUD0VOL(a5)	; channel volume
	add.w	#$10, a5		; next dest regs
	;add.w	#CHANSIZE, a2		; next channel struct
	dbf	d0, .chinit

	; bass channel
	lea	Sample1(a4), a3
	move.l	a3, AUD3LCH(a6)

;	bset.b	#1, PRA_A		; turn off audio filter

Inited: lea.l	Copper(a4), a1		; set copperlist
	move.l	a1, COP1LCH(a6)

	move.w	#$4000, INTENA(a6)	; disable interrupts and set some DMAs
	move.w	#$87CF, DMACON(a6)

;	echosize "Init:", Init

Unpack: ; unpack samples
	lea	Sample1RLE(pc), a0
	lea	Sample1(a4), a1
	bsr	UnpackSample
	;lea	Sample2RLE(pc), a0
	;lea	Sample2(a4), a1
	bsr	UnpackSample	; Sample2RLE

	; unpack melody pattern data
	lea	P00_0(a4), a1
	;lea	PattMelo1(pc), a0
	;moveq	#1, d2
	moveq	#0, d6
	bsr	UnpackNybbles
	bsr	UnpackNybbles	; PattMelo2

	; unpack bass pattern data
	lea	P00_3(a4), a1
	;lea	PattBass1(pc), a0
	moveq	#1, d6
	bsr	UnpackNybbles
	bsr	UnpackNybbles	; PattBass2

	moveq	#$12, d0
	move.b	d0, -52(a1)	; patch note $E -> $12
	move.b	d0, -20(a1)

	;echosize "Unpacker:", Unpack

	; interleave the unpacked channel data from P00_0 to P00_1,2
ILeave: lea	P00_0+1(a4), a0
	moveq	#21*2-1, d3
	moveq	#0, d2
.il	move.b	(a0), PATTERNSIZE+0(a0)		; P00_1
	move.b	1(a0), PATTERNSIZE*2+1(a0)	; P00_2
	move.b	d2, (a0)
	move.b	d2, 1(a0)
	addq	#3, a0
	dbf	d3, .il

;	echosize "Interleave:", ILeave

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

	; ---------------------------------------------------------------------

.wait	move.l	VPOSR(a6), d7
	and.l	#$1ff00, d7
	cmp.l	#130<<8, d7
	bmi.b	.wait

	lea	Sample1(a4), a0

.d	moveq	#0, d0
	move.b	efxpos(a4), d0
	;add.b	row(pc), d0
	add.w	d0, a0

.line	move.b	(a0)+, d2
	;lsr.b	#1, d2
	add.b	row(pc), d2

	btst.b	#0, SongLoops(a4)
	bne.b	.scroll
;.fx1	and.w	#%00110111, d2		; !!!
;	bra.b	.scroll
.fx2	and.w	d0, d2			; !!!

.scroll	move.b	d2, BPLCON1+1(a6)	; horizontal scroll

	;add.w	#187, d0
	;add.b	efxpos(a4), d0
	;add.b	row(pc), d0
	;and.w	#%111111111, d0
	;lsl.w	#1, d0

	;add.b	d2, d0
	;and.w	#%111111, d0
	;.wait2	dbf	d2, .wait2

	move.l	VPOSR(a6), d7
	and.l	#$1ff00, d7
	cmp.l	#210<<8, d7
	bpl.b	MainLoop

	; ---------------------------------------------------------------------

	btst.b	#6, PRA_A		; loop until mouse clicked
	bne.b	.line


Exit:	move.w	#$000F, DMACON(a6)	; turn off sound
	movem.l (sp)+, d0-a6		; restore registers

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

*******************************************************************************
* Music playroutine
*******************************************************************************

PlayMusic:
	moveq	#0, d0
	lea	ticks(a4), a1
	move.b	(a1), d0
;	tst.b	d0			; enough ticks passed?
	bne	.fade			; no
	move.b	#SONGTICKS, (a1)	; yes; reset tick counter and advance

	lea	row(pc), a1
	move.b	(a1), d0
	addq	#1, d0			; advance to next row in pattern

	cmp.b	#PATTERNSIZE/2, d0
	beq	.half
	cmp.b	#PATTERNSIZE, d0	; pattern played through?
	bne.b	.rok			; not yet
.restart
	lea	SongLoops(a4), a3
	add.b	#1, (a3)
	cmp.b	#MAXSONGLOOPS, (a3)	; reset fx when looped this many times
	bne.b	.nore
	push	a1
	bsr	ResetBpls		; reset bpls and SongLoops
	pop	a1

.nore	add.w	#40, Copper+BPLPTRS+6+8(a4) ; offset bitplane
	moveq	#0, d0			; loop to pattern start

.rok	move.b	d0, (a1)		; store row number

	lea	P00_0(a4), a1		; patterndata
	moveq	#4-1, d4		; channel #
	lea	Channel0(pc), a0
	move.l	a6, a5			; CUSTOM
.chan	moveq	#0, d1
	move.b	(a1,d0), d1		; note := patterndata[row]
;	tst.b	d1
	beq.b	.next			; skip if note=0
	cmp.b	#CC, d1
	beq.b	.domaxvol		; special: reset bass vol to 64

	lea	Periods(pc), a2
	subq	#1, d1			; 0-based periodtable access
	add.b	d1, d1
	move.w	(a2,d1), AUD0PER(a5)	; periodtable[note]

	rem>
	move.w	(a2,d1), d7
	move.b	efxpos(a4), d6
	and.b	#%111, d6
	add.b	d6, d7
	move.w	d7, AUD0PER(a5)	; periodtable[note]
	<erem

	move.b	#FADESPEED, S_FADE(a0)

.maxvol moveq	#64, d2
	move.b	d2, S_VOL(a0)
	move.w	d2, AUD0VOL(a5)

.next	addq	#CHANSIZE, a0		; next channel's info
	add.w	#PATTERNSIZE, a1	; next channel's pattern data
.last	add.w	#$10, a5		; next channel's Paula regs
	dbf	d4, .chan		; process next channel

.out	move.w	#CHANNELS_ON, DMACON(a6)

.efx	lea	efxpos(a4), a5
	lea	Sample1(a4), a3
	moveq	#0, d0
	move.b	(a5), d0		; dest. pos in sample data
	and.b	#64-1, d0		; sample is 128 bytes long, don't go past it
	add.w	d0, a3
	not.b	(a3)			; simulate ProTracker EFx effect
	lea	Sample2(a4), a3
	and.w	#32-1, d0		; sample is 32 bytes long, don't go past it
	not.b	(a3,d0)			; simulate ProTracker EFx effect
	add.b	#1, (a5)
	rts

.half:	sub.w	#40, Copper+BPLPTRS+6(a4) ; offset other bitplane
	bra.b	.rok

.domaxvol:
	lea	Channel3+1(pc), a1
	move.b	#5, (a1)
	bra.b	.maxvol

.fade:	lea	Channel0(pc), a3
	move.l	a6, a5
	moveq	#0, d4			; channel #
	moveq	#0, d2
.fac	move.b	S_VOL(a3), d2		; current channel volume
	sub.b	S_FADE(a3), d2		; subtract fade speed
	bcs.b	.nxt			; stop fading
	move.b	d2, S_VOL(a3)
	move.w	d2, AUD0VOL(a5)

.nxt	addq	#CHANSIZE, a3
	add.w	#$10, a5
	addq	#1, d4
	cmp.b	#4, d4
	bne.b	.fac
	subq	#1, d0
	move.b	d0, (a1)		; ticks++
	bra.b	.efx

;	echosize "Audio:", PlayMusic+32+36+2+8+71

; -----------------------------------------------------------------------------

SetBPLPtrs:	; a0 = bitmap, a1 = location of BPLxPTH in copperlist
	move.l	a0, d0
	move.w	d0, 4(a1)
	swap	d0
	move.w	d0, (a1)
	rts

ResetBpls:
	move.b	#0, SongLoops(a4)

	; set bitplane pointers
	lea	Screen+(40*60)(a4), a0
	move.l	a0, a3			; store screen loc for future use
	;subq	#1, a0
	lea	bpl1+2(pc), a1
	bsr.b	SetBPLPtrs
	addq	#8, a1
	sub.w	#40, a0
	bsr.b	SetBPLPtrs

	; copy copperlist to chip
	lea	CopperData(pc), a0
	lea	Copper(a4), a1
	moveq	#(CopperData_End-CopperData)/4-1, d6
.copy	move.l	(a0)+, (a1)+
	dbf	d6, .copy
	rts

; -----------------------------------------------------------------------------
; a0 = rle-packed sample data
; a1 = destination
;
UnpackSample:
.copy	move.b	(a0)+, d0	; value
	cmp.b	#ENDBYTE, d0
	beq.b	.done
.rle	moveq	#0, d1
	move.b	(a0)+, d1	; length
.fill	move.b	d0, (a1)+
	dbf	d1, .fill
	bra.b	.copy
.done	rts

; -------------------------------------------
; a0 = table of nybbles to unpack
; a1 = destination address
; d2 = times to loop (to unpack same data N times in a row)
;
UnpackNybbles:
	moveq	#1, d2
	move.l	a0, a2
.loop	move.l	a2, a0
.readbyte
	move.b	(a0)+, d0
	cmp.b	#ENDBYTE, d0
	beq.b	.out
	; left nybble
.ln	move.b	d0, d1
	lsr.b	#4, d1
	cmp.b	#$F, d1
	bne.b	.lok
	; straight copy
	bsr.b	.sc
	bra.b	.rn
.lok	move.b	d1, (a1)+
	; right nybble
.rn	and.b	#$F, d0
	cmp.b	#$F, d0
	bne.b	.rok
	bsr.b	.sc
	bra.b	.readbyte
.rok	move.b	d0, (a1)+
	bra.b	.readbyte
.out	dbf	d2, .loop
	rts

	; straight copy
.sc	tst.b	d6
	beq.b	.cp
	move.b	#CC, (a1)+
	rts
.cp	move.b	(a0)+, (a1)+
	rts

	;echosize "Unpacker2:", UnpackSample

*******************************************************************************
* Data
*******************************************************************************

DataStart:

YPOS = 0

BG     = $324
BGDARK = $325
;BGDARK = $223
;BGDARK = BG

CopperData:	dc.w	FMODE,	 $0	; AGA slow fetch mode
		dc.w	DIWSTRT, $2C81	; Display window start (top left)
		dc.w	DIWSTOP, $2CB1	; Display window stop (bottom right)
		dc.w	DDFSTRT, $38	; Display data fetch start (horiz.)
		dc.w	DDFSTOP, $D0	; Display data fetch stop  (horiz.)
		dc.w	BPLCON1, $0
		dc.w	BPLCON2, $0
		dc.w	BPL1MOD, $0
		dc.w	BPL2MOD, $0

bpl1:		dc.w	BPL1PTH, 0, BPL1PTL, 0	; Bitplane pointers
bpl2:		dc.w	BPL2PTH, 0, BPL2PTL, 0	;
		dc.w	BPLCON0, PLANES_2|COLOR ; Screen on

;		COPPER_WAIT	100-YPOS,7
;		COPPER_SETCOLOR 0, $435		; line
;		COPPER_WAIT	101-YPOS, 7

		COPPER_SETCOLOR	0, $BBA
		COPPER_SETCOLOR	2, $9AE
		COPPER_SETCOLOR	3, $A93

;		COPPER_SETCOLOR	0, BG		; bar
;		COPPER_SETCOLOR 1, $424		; ->
;		COPPER_SETCOLOR 2, $334		; <-
;		COPPER_SETCOLOR 3, $327 	; text 327

		COPPER_WAIT	240-YPOS-70,7
;		COPPER_SETCOLOR 0, $201		; line
;		COPPER_WAIT	241-YPOS, 7

;		COPPER_SETCOLOR 0, BGDARK	; background
		COPPER_SETCOLOR	0, $CB9

;		dc.w BPLCON0, 	$0200		; Screen off

		COPPER_END

CopperData_End:

Channel0:	dc.b	00	; volume
		dc.b	16	; length/fade speed
Channel1:	dc.b	00	; volume
		dc.b	16	; length/fade speed
Channel2:	dc.b	00	; volume
		dc.b	16	; length/fade speed
Channel3:	dc.b	00	; volume
ch3fade:	dc.b	64	; length/fade speed

		; VOID logo
Logo:		dc.w	%0111010111101001
		dc.w	%1101000100101001
		dc.w	%1001010100101001
		dc.w	%1001010100101011
		dc.w	%1111010111101110

		; period table, 36 bytes
Periods:	dc.w	604,202,508,480,453,404,381,339,320
		dc.w	302,269,254,240,226,202,190,160,538

row:		dc.b	PATTERNSIZE-1

		; Sample 01 (64 words, 128 bytes): "bass"
Sample1RLE:	dc.b	127,83,115,2,100,6,077,4,055,4
		dc.b	039,3,020,3,12,4,250,10,ENDBYTE

		; Sample 02 (16 words, 32 bytes): "melody"
Sample2RLE:	dc.b	17,3,238,5,38,3,191,6,43,4,225,5,ENDBYTE

		;echosize "Samples:", Sample1RLE

		; 36 bytes; nybbles pointing to period table indices
		; F = add next byte to output without unpacking
PattMelo1:	dc.b	$13,$6A,$2C,$A6,$31,$68,$BC,$A7			; 8
		dc.b	$5A,$EF,$10,$2C,$A6,$36,$8A,$C2,$CA,ENDBYTE	; 9
PattMelo2:	dc.b	$46,$9D,$F2,$11,$D9,$64,$9B,$DE,$B8		; 9
		dc.b	$5A,$EF,$10,$2C,$A6,$36,$8A,$C2,$CA,ENDBYTE	; 9

PattBass1:	dc.b	$A0,$FF,$FF,$FF,$FA,$0F,$30,$FF
		dc.b	$50,$FF,$FF,$FF,$70,$FF,$70,$FF,ENDBYTE
PattBass2:	dc.b	$60,$FF,$FF,$FF,$F6,$0F,$E0,$FF
		dc.b	$50,$FF,$FF,$FF,$30,$FF,$FF,$FF,ENDBYTE

		;echosize "Song:", PattMelo1

		if	RELEASE = 1
		dc.b	"PENIS!"
		endc

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
Sample2:	rs.w	16

P00_0:		rs.b	PATTERNSIZE
P00_1:		rs.b	PATTERNSIZE
P00_2:		rs.b	PATTERNSIZE
P00_3:		rs.b	PATTERNSIZE

efxpos:		rs.b	1
SongLoops:	rs.b	1

ticks:		rs.b	1
foo:		rs.b	1

Screen:		rs.l	SCREENWIDTH/32*(SCREENHEIGHT+90);*SCREENDEPTH

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
