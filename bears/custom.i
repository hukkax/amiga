	ifnd inc_custom

inc_custom = 1

************************************************************************************
* Amiga Hardware Registers
************************************************************************************

EXEC		= $4		; ExecBase
EXECBASE	= EXEC
CUSTOM		= $DFF000	; CustomBase

OldOpenLibrary	= -408
CloseLibrary	= -414

* graphics.library

LoadView	= -222		; Use a copper list to create the current display
WaitTOF		= -270		; Wait for the top of the next video frame

OwnBlitter	= -456		; Get the blitter for private usage
DisownBlitter	= -462		; Return blitter to free state
QBlit		= -276		; Queue up a request for blitter usage
QBSBlit		= -294		; Synchronize the blitter request with the video beam.
WaitBlit	= -228		; Wait for the blitter to finish

_LVOLoadView	= LoadView
_LVOWaitTOF	= WaitTOF
;_LVOFindResident= -96
;gb_ActiView	= 34
;gb_copinit	= 38
IVBLIT		= 156
RT_INIT		= 22

* file

MODE_OLDFILE	= 1005
MODE_NEWFILE	= 1006 ; check this!

* bits

BIT15		= %1000000000000000
BIT14		= %0100000000000000
BIT13		= %0010000000000000
BIT12		= %0001000000000000
BIT11		= %0000100000000000
BIT10		= %0000010000000000
BIT09		= %0000001000000000
BIT08		= %0000000100000000
BIT07		= %0000000010000000
BIT06		= %0000000001000000
BIT05		= %0000000000100000
BIT04		= %0000000000010000
BIT03		= %0000000000001000
BIT02		= %0000000000000100
BIT01		= %0000000000000010
BIT00		= %0000000000000001

************************************************************************************
* Copper commands

COPPER_WAIT: MACRO		; 1=Y,2=X - wait for line Y, horizontal position X
	dc.w	((\1)&$FF)<<8|((\2)&$7F)<<1|1, $FFFE
	ENDM

COPPER_WAIT_X: MACRO		; 1=X - wait for horizontal position X, ignore line
	dc.w	($0&$FF)<<8|((\1)&$7F)<<1|1, $00FE
	ENDM

COPPER_WAIT_Y: MACRO		; 1=Y - wait for line Y, ignore horizontal position
	dc.w	((\1)&$FF)<<8|($0&$7F)<<1|1, $FF00
	ENDM

COPPER_WAIT_NEXT: MACRO		; wait for next rasterline
	dc.w	$80DF, $80FE
	ENDM

; F00F FFFE
COPPER_WAIT_NEXTD: MACRO	; wait for next rasterline, lower part of screen
	dc.w	$00DF, $80FE
	ENDM

COPPER_BOTTOM: MACRO		; access bottom area (>255)
	dc.w	$FFDF, $FFFE
	ENDM

COPPER_END: MACRO		; end copper list
	dc.w	$FFFF, $FFFE
	dc.w	$FFFF, $FFFE
	ENDM

COPPER_SETCOLOR: MACRO
	dc.w	COLOR00+((\1)*2), (\2)
	ENDM

* Misc

WaitBlitter:	macro	; a6 = CUSTOM
		tst.w	DMACONR+CUSTOM
.\@		btst	#6, DMACONR+CUSTOM
		bne.s	.\@
		endm

		rem
; d2 = SCANLINE<<8
; destroys a6, d0
WaitForLine:
		lea	CUSTOM, a6
.w1		move.l	VPOSR(a6), d0
		and.l	#$1ff00, d0
		cmp.l	d2, d0
		bne.b	.w1
.w2		move.l	VPOSR(a6), d0
		and.l	#$1ff00, d0
		cmp.l	d2, d0
		beq.b	.w2
		rts

WaitLine:	macro
		move.l	#\1<<8, d2
		bsr	WaitForLine
		endm
		erem

************************************************************************************
* Display

LISAID		= $07C	; Denise/Lisa (video out chip) revision level
			; 7-4	Lisa/Denise/ECS Denise Revision level (decrement to
			;	bump revision level, hex F represents 0th rev. level).
			; 3 	Maintain as a 1 for future generation
			; 2 	When low indicates AA feature set (LISA)
			; 1 	When low indicates ECS feature set (LISA or ECS DENISE)
			; 0 	Maintain as a 1 for future generation

DIWSTRT		= $08E	; Display window start (upper left vertical-horizontal pos.)
			; 	- bits 15-8 = VSTART (V7-V0)
			; 	- bits  7-0 = HSTART (H7-H0)
DIWSTOP		= $090	; Display window stop (lower right vertical-horizontal pos.)
			; 	- bits 15-8 = VSTOP  (V7-V0)
			; 	- bits  7-0 = HSTOP  (H7-H0)
DDFSTRT		= $092	; Display data fetch start (horiz. position)
			; 	(Beginning position for data fetch)
			; 	- bits 7-3 = pixel position H8-H4
DDFSTOP		= $094	; Display data fetch stop  (horiz. position)
			; 	(Ending position for data fetch)
			; 	- bits 7-3 = pixel position H8-H4
* Bitplanes

BPL1PTH		= $0E0	; Bitplane 1 pointer (high 3 bits)
BPL1PTL		= $0E2	; Bitplane 1 pointer (low 15 bits)
BPL2PTH		= $0E4	; Bitplane 2 pointer (high 3 bits)
BPL2PTL		= $0E6	; Bitplane 2 pointer (low 15 bits)
BPL3PTH		= $0E8	; Bitplane 3 pointer (high 3 bits)
BPL3PTL		= $0EA	; Bitplane 3 pointer (low 15 bits)
BPL4PTH		= $0EC	; Bitplane 4 pointer (high 3 bits)
BPL4PTL		= $0EE	; Bitplane 4 pointer (low 15 bits)
BPL5PTH		= $0F0	; Bitplane 5 pointer (high 3 bits)
BPL5PTL		= $0F2	; Bitplane 5 pointer (low 15 bits)
BPL6PTH		= $0F4	; Bitplane 6 pointer (high 3 bits)
BPL6PTL		= $0F6	; Bitplane 6 pointer (low 15 bits)
BPL7PTH		= $0F8	; Bitplane 7 pointer (high 3 bits) AGA
BPL7PTL		= $0FA	; Bitplane 7 pointer (low 15 bits) AGA
BPL8PTH		= $0FC	; Bitplane 8 pointer (high 3 bits) AGA
BPL8PTL		= $0FE	; Bitplane 8 pointer (low 15 bits) AGA
			;
			; These register pairs contain the 18-bit pointer to
			; the address of bitplane x (x = 1..6/1..8 AGA) DMA data.
			; This pointer must be reinitialized by the processor
			; or Copper to point to the beginning of bit plane data
			; every vertical blank time.

BPLCON0		= $100	; Bitplane control register (misc. control bits)
			; NOTE: Bits in this register cannot be independently set.
			;
			; Bit#	Name	Description (for OCS)
			; ----	------	------------------------------------------
			;  15	HIRES	High-resolution/low-resolution mode select
			;  14	BPU2	Number of bit-planes used
			;  13	BPU1	- 000 = only a background color
			;  12	BPU0	- 111 = not used
			;  11	HOMOD	Hold-And-Modify enable/disable
			;  10	DBLPF	Dual playfields enable/disable
			;  09  COLOR_ON	Composite video color-burst enable/disable
			;  08	GAUD	Genlock audio enable/disable
			;  07..04	Not used
			;  03	LPEN	Light pen enable/disable
			;  02	LACE	Interlaced mode enable/disable
			;  01	ERSY	External synchronization enable/disable
			;  00	ECSENA	When 0 (default), the following bits in BPLCON3
			; 		are disabled: BRDRBLNK,BRDNTRAN, ZDCLKEN,
			;		BRDSPRT, and EXTBLKEN.
			;
			; Bit#	Name	Description (changes for ECS/AGA)
			; ----	------	------------------------------------------
			;  15	HIRES	High resolution (640*200/640*400 interlace)
			;  10	DPF	Double playfield (PFI=odd FP2= even bit planes) 
			; 		now available in all resolutions.
			;		(If BPU=6 and HAM=0 and DPF=0, EHB mode)
			;  07	UHRES	Ultrahi res enables the UHRES pointers (for 1k*1k)
			;		(also needs bits in DMACON (hires chips only).
			;		Disables hard stops for vert, horiz display windows.
			;  06	SHRES	Super hi-res mode (35ns pixel width)
			;  05	BYPASS	Bitplanes are scrolled and prioritized normally, but
			;		bypass color table and 8 bit wide data appear on R(7:0)
			;  04	BPU3	See above (BPU0-2)

HIRES		= BIT15
PLANES_1 	= %0001000000000000
PLANES_2 	= %0010000000000000
PLANES_3 	= %0011000000000000
PLANES_4 	= %0100000000000000
PLANES_5 	= %0101000000000000
PLANES_6 	= %0110000000000000
HAM		= BIT11
DBLPF		= BIT10
COLOR		= BIT09

BPLCON1		= $102	; Bitplane control register (horizontal scroll control)
			;
			; Bit#	  Name		Description
			; ----	  ----------	-----------------
			; 15-8	  -		Not used
			;  7-4	  PF2H(3..0)	Playfield 2 delay
			;  3-0	  PF1H(3..0)	Playfield 1 delay
			;
BPLCON2		= $104	; Bitplane control register (video priority control)
			;
			; Bit#	Name	Description
			; ----	------	----------------------------------------------------
			; 15-12	PF2Hx	AGA Playfield 2 horizontal scroll code, x=0-7
			; 11-7	PF1Hx	AGA Playfield 1 horizontal scroll code, x=0-7
			;   6	PF2PRI	1 = Playfield 2 has priority, 0 = PF 1 has priority
			;  5-3	PF2Px	Playfield 2 sprite priority
			;  2-0	PF1Px	Playfield 1 sprite priority
PF2PRI		= BIT06
			;
BPLCON3		= $106	; AGA Bitplane control (enhanced features)
			;
			; Bit#	Name	Description
			; ----	------	------------------------------------------
			;  15	BANK2	BANKx = Selects one of eight color banks, x=0-2.
			;  14	BANK1
			;  13	BANK0
			;  12   PF2OF2	Determine bit plane color table offset when playfield
			; 		2 has priority in dual playfield mode:
			;+-----------+-------------------------------+------------+
			;| PF20F     | AFFECTED BITPLANE             | OFFSET     |
			;+---+---+---+-------------------------------+------------+
			;| 2 | 1 | 0 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | (decimal)  |
			;+---+---+---+-------------------------------+------------+
			;| 0 | 0 | 0 | - | - | - | - | - | - | - | - | none       |
			;| 0 | 0 | 1 | - | - | - | - | - | - | 1 | - | 2          |
			;| 0 | 1 | 0 | - | - | - | - | - | 1 | - | - | 4          |
			;| 0 | 1 | 1 | - | - | - | - | - | 1 | - | - | 8 (default)|
			;| 1 | 0 | 0 | - | - | - | 1 | - | - | - | - | 16         |
			;| 1 | 0 | 1 | - | - | 1 | - | - | - | - | - | 32         |
			;| 1 | 1 | 0 | - | 1 | - | - | - | - | - | - | 64         |
			;| 1 | 1 | 1 | 1 | - | - | - | - | - | - | - | 128        |
			;+---+---+---+---+---+---+---+---+---+---+---+------------+
			;
			; 11 PF2OF1
			; 10 PF2OF0
			; 09 LOCT	Dictates that subsequent color palette values will be
			;		written to a second 12- bit color palette, constituting
			;		the RGB low minus order bits. Writes to the normal hi
			; 		monus order color palette automattically copied to the
			; 		low order for backwards compatibility.
			; 08 X		Don't care- but drive to 0 for upward compatibility!
			; 07 SPRES1	Determines resolution of all 8 sprites (x=0,1)
			; 06 SPRES0	as follows:
			;	+-------+--------+---------------------------------------------+
			;	|SPRES1 | SPRES0 | SPRITE RESOLUTION                           |
			;	+-------+--------+---------------------------------------------+
			;	|0      | 0      | ECS defaults (LORES,HIRES=140ns,SHRES=70ns) |
			;	|0      | 1      | LORES (140ns)                               |
			;	|1      | 0      | HIRES (70ns)                                |
			;	|1      | 1      | SHRES (35ns)                                |
			;	+-------+--------+---------------------------------------------+
			; 05 BRDRBLNK	"Border area" is blanked instead of color (0).
BRDRBLNK 	= BIT05
			; 04 BRDNTRAN	"Border area" is non minus transparent (ZD pin is low
			;		when border is displayed).
			; 03 X		Don`t care- but drive to 0 for upward compatibility!
			; 02 ZDCLKEN	ZD pin outputs a 14MHz clock whose falling edge
			; 		coincides with hires (7MHz) video data. this bit when
			;		set disables all other ZD functions.
			; 01 BRDSPRT	Enables sprites outside the display window.
			; 00 EXTBLKEN	Causes BLANK output to be programmable instead of
			;		reflecting internal fixed decodes.
			;
			; (00-05 are disabled when ESCENA low.)
			;
BPLCON4		= $10C	; AGA Bitplane control reg. (Display masks)
			;
			; 15	BPLAM7	This 8 bit field is XOR`ed with the 8 bit
			; 14	BPLAM6	plane color address, thereby altering the
			; 13	BPLAM5	color address sent to the color table (x=1-8)
			; 12	BPLAM4
			; 11	BPLAM3
			; 10	BPLAM2
			; 09	BPLAM1
			; 08	BPLAM0
			; 07	ESPRM7	4 bit field provides the 4 high order color
			; 06	ESPRM6	table address bits for even sprites:
			; 05	ESPRM5	SPR0, SPR2, SPR4, SPR6. Default=%0001. (x=7-4)
			; 04	ESPRM4
			; 03	OSPRM7	4 bit field provides the 4 high order color
			; 02	OSPRM6	table address bits for odd sprites:
			; 01	OSPRM5	SPR1, SPR3, SPR5, SPR7. Default=%0001. (x=7-4)
			; 00	OSPRM4

BPL1MOD		= $108	; Bitplane modulo (odd-numbered  planes, playfield 1)
BPL2MOD		= $10A	; Bitplane modulo (even-numbered planes, playfield 2)
			;
			; These registers contain the modulos for the odd and even bitplanes.
			; A modulo is a number that is automatically added to the address at
			; the end of each line, so that the address then points to the start
			; of the next line. Since they have separate modulos, the odd and even
			; bitplanes may have sizes that are different from each other, as well
			; as different from the display window size.

BPL1DAT		= $110	; Bitplane 1 data (parallel-to-serial convert)
BPL2DAT		= $112	; Bitplane 2 data (parallel-to-serial convert)
BPL3DAT		= $114	; Bitplane 3 data (parallel-to-serial convert)
BPL4DAT		= $116	; Bitplane 4 data (parallel-to-serial convert)
BPL5DAT		= $118	; Bitplane 5 data (parallel-to-serial convert)
BPL6DAT		= $11A	; Bitplane 6 data (parallel-to-serial convert)
BPL7DAT		= $11C	; Bitplane 7 data (parallel-to-serial convert)
BPL8DAT		= $11E	; Bitplane 8 data (parallel-to-serial convert)
			;
			; These registers receive the DMA data fetched from RAM
			; by the bitplane address pointers BPLxPTH, BPLxPTL.
			; They may also be written by either microprocessor.
			; They act as a six-word parallel-to-serial buffer for
			; up to six memory bitplanes (x=1..6). The parallel-to-serial
			; conversion is triggered whenever bitplane #1 is written, indicating
			; the completion of all bit planes for that word (16 pixels).
			; The MSB is output first, and is therefore always on the left.

************************************************************************************
* Blitter

BLTCON0		= $040	; Blitter control register 0
BLTCON1		= $042	; Blitter control register 1
			;
			; AREA MODE ("normal")
			; ==========================================================
			; Bit#	BLTCON0	BLTCON1	Description
			; ----	-------	-------	------------------------------------
			;  15	ASH3	BSH3	Shift value of A/B source
			;  14	ASH2	BSH2	.
			;  13	ASH1	BSH1	.
			;  12	ASH0	BSH0	.
			;
			;  11	USEA	X	Mode control bit to use source A
			;  10	USEB	X	Mode control bit to use source B
			;  09	USEC	X	Mode control bit to use source C
			;  08	USED	X	Mode control bit to use destination D
			;
			;  07	LF7	X	Logic function minterm select lines
			;  06	LF6	X	.
			;  05	LF5	X	.
			;  04	LF4	EFE	Exclusive fill enable
			;
			;  03	LF3	IFE	Inclusive fill enable
			;  02	LF2	FCI	Fill carry input
			;  01	LF1	DESC	Descending (decreasing address) control bit
			;  00	LF0	LINE	Line mode control bit (set to 0)
			;
			; LINE MODE (line draw)
			; ==========================================================
			; Bit#	BLTCON0	BLTCON1	  Description
			; ----	-------	-------	  ----------------------------------
			;  15	START3	TEXTURE3  Starting point of line (0-15 hex)
			;  14	START2	TEXTURE2
			;  13	START1	TEXTURE1
			;  12	START0	TEXTURE0
			;  11	1	0
			;  10	0	0
			;  09	1	0
			;  08	1	0
			;  07	LF7	0	  Logic function minterm
			;  06	LF6	SIGN	  Sign flag
			;  05	LF5	0 	  (Reserved for new mode)
			;  04	LF4	SUD	  Sometimes up or down (=AUD*)
			;  03	LF3	SUL	  Sometimes up or left
			;  02	LF2	AUL	  Always up or left
			;  01	LF1	SING	  Single bit per horz.line for use with area fill
			;  00	LF0	LINE	  Line mode control bit (set to 1)

BLTAFWM		= $044	; Blitter first-word mask for source A
BLTALWM		= $046	; Blitter last-word  mask for source A
			;
			; The patterns in these two registers are ANDed with the first and
			; last words of each line of data from source A into the blitter.
			; A zero in any bit overrides data from source A. These registers
			; should be set to all 1s for fill mode or for line-drawing mode.

BLTAPTH		= $050	; Blitter pointer to x (high 3 bits)
BLTAPTL		= $052	; Blitter pointer to x (low 15 bits)
BLTBPTH		= $04C	; Blitter pointer to x (high 3 bits)
BLTBPTL		= $04E	; Blitter pointer to x (low 15 bits)
BLTCPTH		= $048	; Blitter pointer to x (high 3 bits)
BLTCPTL		= $04A	; Blitter pointer to x (low 15 bits)
BLTDPTH		= $054	; Blitter pointer to x (high 3 bits) (destination)
BLTDPTL		= $056	; Blitter pointer to x (low 15 bits) (destination)

BLTAMOD		= $064	; Blitter modulo A (modulo for blitter source)
BLTBMOD		= $062	; Blitter modulo B (modulo for blitter source)
BLTCMOD		= $060	; Blitter modulo C (modulo for blitter source)
BLTDMOD		= $066	; Blitter modulo D (modulo for blitter destination)
			;
			; These registers contain the modulo for blitter source (x=A,B,C)
			; or destination (x=D). A modulo is a number that is automatically
			; added to the address at the end of each line, to make the address
			; point to the start of the next line. Each source or destination
			; has its own modulo, allowing each to be a different size, while
			; an identical area of each is used in the blitter operation.

BLTADAT		= $074	; Blitter source A data register
BLTBDAT		= $072	; Blitter source B data register
BLTCDAT		= $070	; Blitter source C data register
			;
			; These registers hold source x (x=A,B,C) data for use by the blitter.
			; It is normally loaded by the blitter DMA channel; however, it may
			; also be preloaded by the microprocessor.
			;
			; LINE DRAW:
			; BLTADAT is used as an index register and must be preloaded with 8000.
			; BLTBDAT is used for texture; it must be preloaded with FF if no
			; texture (solid line) is desired.
			;
BLTDDAT		= $000	; Blitter destination data register (DUMMY ADDRESS)
			;
			; This register holds the data resulting from each word of blitter
			; operation until it is sent to a RAM destination. This is a dummy
			; address and cannot be read by the micro. The transfer is automatic
			; during blitter operation.

BLTSIZE		= $058	; Blitter start and size (window width, height)
			;
			; Contains the width and height of the blitter operation. Writing to
			; this register will start the blitter, and should be done last,
			; after all pointers and control registers have been initialized.
			;
			; BIT# 15 14 13 12 11 10 09 08 07 06  05 04 03 02 01 00
			;      h9 h8 h7 h6 h5 h4 h3 h2 h1 h0, w5 w4 w3 w2 wl w0
			;
			; h = height = vertical lines    (10 bits=1024 lines max)
			; w = width  = horizontal pixels (6  bits=64 words=1024 pixels max)
			;
			; LINE DRAW:
			; BLTSIZE controls line length and starts the line draw when written to.
			; h field controls line length (10 bits gives lines up to 1024 px long).
			; w field must be set to 02 for all line drawing.

************************************************************************************
* Copper

COPCON		= $02E	; Copper control register
			; 1-bit reg.; when 1, allows Copper to access the blitter hardware.

COPJMP1		= $088	; Copper restart at first  location
COPJMP2		= $08A	; Copper restart at second location
			;
			; These addresses are strobe addresses. When written to, they cause
			; the Copper to jump indirect using the address contained in the first
			; or second location registers described below. The Copper itself can
			; write to these addresses, causing its own jump indirect.

COP1LCH		= $080	; Copper first location register (high 3 bits)
COP1LCL		= $082	; Copper first location register (low 15 bits)
COP2LCH		= $084	; Copper second location register (high 3 bits)
COP2LCL		= $086	; Copper second location register (low 15 bits)

COPINS		= $08C	; Copper instruction fetch identify
			;
			; This is a dummy address that is generated by the Copper whenever
			; it is loading instructions into its own instruction register.
			; This actually occurs every Copper cycle except for the
			; second (IR2) cycle of the MOVE instruction.

;ENDCOPPER = $ffff,$fffe

************************************************************************************
* Interrupts

INTREQ		= $09C	; Interrupt request bits (clear or set)
INTREQR		= $01E	; Interrupt request bits (read)
			;
			; This register contains interrupt request bits (or flags). 
			; These bits may be polled by the processor; if enabled 
			; by the bits listed in the next register, they may cause
			; processor interrupts. Both a set and clear operation are
			; required to load arbitrary data into this register. 
			; These status bits are not automatically reset when the
			; interrupt is serviced, and must be reset when desired by
			; writing to this address. The bit assignments are identical
			; to the enable register below.

INTENA		= $09A	; Interrupt enable bits  (clear or set bits)
INTENAR		= $01C 	; Interrupt enable bits  (read)
;
; Bit#	Name	Level	Description
; ----	------	-----	---------------------------------------------------------
;  15	SETCLR	  -	Set or clear bits?
;  14	INTEN	  -	Master interrupt (enable only, no request)
;  13	EXTER	  6	Set when the system line called INT6* becomes a logic 0
;  12	DSKSYN	  5	Sync register (DSKSYNC) matches disk data
;  11	RBF	  5	Serial port UART receive buffer full
;  10	AUD3	  4	Audio channel X block finished. Audio DMA modes:
;  09	AUD2	  4	- Automatic: Last word in audio data stream has been accessed
;  08	AUD1	  4	- Manual:    Audio data register is ready to accept
;  07	AUD0	  4	             another word of data
;  06	BLIT	  3	Blitter finished requested data transfer
;  05	VERTB	  3	Causes an interrupt at rasterline 0
;  04	COPER	  3	Copper has reached a specific rasterline
;  03	PORTS	  2	I/O ports & timers; set when system line INT2* becomes logic 0
;  02	SOFT	  1	Reserved for software-initiated interrupt
;  01	DSKBLK	  1	Disk block finished
;  00	TBE	  1	Serial port UART transmit buffer empty

; bits
b_inten	=14
b_sync	=12
b_serial=11
b_aud3	=10
b_aud2	=9
b_aud1	=8
b_aud0	=7
b_blit	=6
b_vbl	=5
b_cop	=4
b_key	=3
b_soft	=2
b_blk	=1
b_tbe	=0

INTF_SETCLR	= BIT15
INTF_INTEN	= BIT14
INTF_VERTB	= BIT05
INTF_PORTS	= BIT03

I_setclr	= INTF_SETCLR
I_inten		= INTF_INTEN
I_ciab		= $2000
I_aud		= $3C0
I_Blit		= BIT06
I_Vbl		= INTF_VERTB
I_Cop		= $10
I_Kb		= $8
I_Soft		= $4
I_Disk		= $2



V_lev1=$64 ; software-diskblk-tbe
V_lev2=$68 ; keyboard cia-a
V_lev3=$6c ; vbl-copper-blitter
V_lev4=$70 ; audio
V_lev5=$74 ; disksync-rbf
V_lev6=$78 ; cia-b (timer)

************************************************************************************
* Joystick/Mouse

JOY0DAT		= $00A	; Joystick/mouse 0 data (left  vertical, horizontal)
JOY1DAT		= $00C	; Joystick/mouse 1 data (right vertical, horizontal)

JOYTEST		= $036	; Write to all four joystick-mouse counters at once

POT0DAT		= $012	; Pot counter data left  pair (vert, horiz)
POT1DAT		= $014	; Pot counter data right pair (vert, horiz)
POTGO		= $034	; Pot port data write and start
POTGOR		= $016	; Pot port data read (formerly called POTINP)

************************************************************************************
* Sprites

SPR0PTH		= $120	; Sprite 0 pointer (high 3 bits)
SPR0PTL		= $122	; Sprite 0 pointer (low 15 bits)
SPR0POS		= $140	; Sprite 0 vert-horiz start position data
SPR0CTL		= $142	; Sprite 0 vert stop position and control data
SPR0DATA	= $144	; Sprite 0 image data register A
SPR0DATB	= $146	; Sprite 0 image data register B

SPR1PTH		= $124	; Sprite 1 pointer (high 3 bits)
SPR1PTL		= $126	; Sprite 1 pointer (low 15 bits)
SPR1POS		= $148	; Sprite 1 vert-horiz start position data
SPR1CTL		= $14A	; Sprite 1 vert stop position and control data
SPR1DATA	= $14C	; Sprite 1 image data register A
SPR1DATB	= $14E	; Sprite 1 image data register B

SPR2PTH		= $128	; Sprite 2 pointer (high 3 bits)
SPR2PTL		= $12A	; Sprite 2 pointer (low 15 bits)
SPR2POS		= $150	; Sprite 2 vert-horiz start position data
SPR2CTL		= $152	; Sprite 2 vert stop position and control data
SPR2DATA	= $154	; Sprite 2 image data register A
SPR2DATB	= $156	; Sprite 2 image data register B

SPR3PTH		= $12C	; Sprite 3 pointer (high 3 bits)
SPR3PTL		= $12E	; Sprite 3 pointer (low 15 bits)
SPR3POS		= $158	; Sprite 3 vert-horiz start position data
SPR3CTL		= $15A	; Sprite 3 vert stop position and control data
SPR3DATA	= $15C	; Sprite 3 image data register A
SPR3DATB	= $15E	; Sprite 3 image data register B

SPR4PTH		= $130	; Sprite 4 pointer (high 3 bits)
SPR4PTL		= $132	; Sprite 4 pointer (low 15 bits)
SPR4POS		= $160	; Sprite 4 vert-horiz start position data
SPR4CTL		= $162	; Sprite 4 vert stop position and control data
SPR4DATA	= $164	; Sprite 4 image data register A
SPR4DATB	= $166	; Sprite 4 image data register B

SPR5PTH		= $134	; Sprite 5 pointer (high 3 bits)
SPR5PTL		= $136	; Sprite 5 pointer (low 15 bits)
SPR5POS		= $168	; Sprite 5 vert-horiz start position data
SPR5CTL		= $16A	; Sprite 5 vert stop position and control data
SPR5DATA	= $16C	; Sprite 5 image data register A
SPR5DATB	= $16E	; Sprite 5 image data register B

SPR6PTH		= $138	; Sprite 6 pointer (high 3 bits)
SPR6PTL		= $13A	; Sprite 6 pointer (low 15 bits)
SPR6POS		= $170	; Sprite 6 vert-horiz start position data
SPR6CTL		= $172	; Sprite 6 vert stop position and control data
SPR6DATA	= $174	; Sprite 6 image data register A
SPR6DATB	= $176	; Sprite 6 image data register B

SPR7PTH		= $13C	; Sprite 7 pointer (high 3 bits)
SPR7PTL		= $13E	; Sprite 7 pointer (low 15 bits)
SPR7POS		= $178	; Sprite 7 vert-horiz start position data
SPR7CTL		= $17A	; Sprite 7 vert stop position and control data
SPR7DATA	= $17C	; Sprite 7 image data register A
SPR7DATB	= $17E	; Sprite 7 image data register B

* Collision

CLXCON		= $098	; Collision control
CLXCON2		= $10C	; AGA Extended collision control
CLXDAT		= $00E	; Collision data register (read and clear)

************************************************************************************
* Audio

ADKCON		= $09E	; Audio, disk, UART control    (Write)
ADKCONR		= $010	; Audio, disk control register (Read)

AUD0LCH		= $0A0	; Audio channel 0 location (high 3 bits)
AUD0LCL		= $0A2	; Audio channel 0 location (low 15 bits)
AUD0LEN		= $0A4	; Audio channel 0 length
AUD0PER		= $0A6	; Audio channel 0 period
AUD0VOL		= $0A8	; Audio channel 0 volume
AUD0DAT		= $0AA	; Audio channel 0 data

AUD1LCH		= $0B0	; Audio channel 1 location (high 3 bits)
AUD1LCL		= $0B2	; Audio channel 1 location (low 15 bits)
AUD1LEN		= $0B4	; Audio channel 1 length
AUD1PER		= $0B6	; Audio channel 1 period
AUD1VOL		= $0B8	; Audio channel 1 volume
AUD1DAT		= $0BA	; Audio channel 1 data

AUD2LCH		= $0C0	; Audio channel 2 location (high 3 bits)
AUD2LCL		= $0C2	; Audio channel 2 location (low 15 bits)
AUD2LEN		= $0C4	; Audio channel 2 length
AUD2PER		= $0C6	; Audio channel 2 period
AUD2VOL		= $0C8	; Audio channel 2 volume
AUD2DAT		= $0CA	; Audio channel 2 data

AUD3LCH		= $0D0	; Audio channel 3 location (high 3 bits)
AUD3LCL		= $0D2	; Audio channel 3 location (low 15 bits)
AUD3LEN		= $0D4	; Audio channel 3 length
AUD3PER		= $0D6	; Audio channel 3 period
AUD3VOL		= $0D8	; Audio channel 3 volume
AUD3DAT		= $0DA	; Audio channel 3 data

************************************************************************************
* Color Palette
;
; Bits		Contents
; -------	--------
; 15 - 12	Unused
; 11 - 08	Red
; 07 - 04	Green
; 03 - 00	Blue
;
COLOR00		= $180	; Color table entry 00
COLOR01		= $182	; Color table entry 01
COLOR02		= $184	; Color table entry 02
COLOR03		= $186	; Color table entry 03
COLOR04		= $188	; Color table entry 04
COLOR05		= $18A	; Color table entry 05
COLOR06		= $18C	; Color table entry 06
COLOR07		= $18E	; Color table entry 07
COLOR08		= $190	; Color table entry 08
COLOR09		= $192	; Color table entry 09
COLOR10		= $194	; Color table entry 10
COLOR11		= $196	; Color table entry 11
COLOR12		= $198	; Color table entry 12
COLOR13		= $19A	; Color table entry 13
COLOR14		= $19C	; Color table entry 14
COLOR15		= $19E	; Color table entry 15
COLOR16		= $1A0	; Color table entry 16
COLOR17		= $1A2	; Color table entry 17
COLOR18		= $1A4	; Color table entry 18
COLOR19		= $1A6	; Color table entry 19
COLOR20		= $1A8	; Color table entry 20
COLOR21		= $1AA	; Color table entry 21
COLOR22		= $1AC	; Color table entry 22
COLOR23		= $1AE	; Color table entry 23
COLOR24		= $1B0	; Color table entry 24
COLOR25		= $1B2	; Color table entry 25
COLOR26		= $1B4	; Color table entry 26
COLOR27		= $1B6	; Color table entry 27
COLOR28		= $1B8	; Color table entry 28
COLOR29		= $1BA	; Color table entry 29
COLOR30		= $1BC	; Color table entry 30
COLOR31		= $1BE	; Color table entry 31

************************************************************************************
* Peripherals

* 8520-A

PRA_A		= $BFE001	; Peripheral data register A
CIA_A		= PRA_A
CIAA		= PRA_A
CIAAPRA		= PRA_A
PRB_A		= $BFE101	; Peripheral data register B
DDRB_A		= $BFE201	; Data direction register A
DDRA_A		= $BFE301	; Data direction register B
TALO_A		= $BFE401	; TIMER A low register
TAHI_A		= $BFE501	; TIMER A high register
TBLO_A		= $BFE601	; TIMER B low register
TBHI_A		= $BFE701	; TIMER B high register
EV1_A		= $BFE801	; Event LSB
EV2_A		= $BFE901	; Event 8 - 15
EV3_A		= $BFEA01	; Event MS8
NOCONNECT_A	= $BFEB01	; No connect
SDR_A		= $BFEC01	; Serial data register
ICR_A		= $BFED01	; Interrupt control register
CRA_A		= $BFEE01	; Control register A
CRB_A		= $BFEF01	; Control register B

* 8520-B

PRA_B		= $BFD000	; Peripheral data register A
CIA_B		= PRA_B
CIAB		= PRA_B
PRB_B		= $BFD100	; Peripheral data register B
CIAAPRB		= PRB_B
DDRB_B		= $BFD200	; Data direction register A
DDRA_B		= $BFD300	; Data direction register B
TALO_B		= $BFD400	; TIMER A low register
TAHI_B		= $BFD500	; TIMER A high register
TBLO_B		= $BFD600	; TIMER B low register
TBHI_B		= $BFD700	; TIMER B high register
EV1_B		= $BFD800	; Event LSB
EV2_B		= $BFD900	; Event 8 - 15
EV3_B		= $BFDA00	; Event MS8
NOCONNECT_B	= $BFDB00	; No connect
SDR_B		= $BFDC00	; Serial data register
ICR_B		= $BFDD00	; Interrupt control register
CRA_B		= $BFDE00	; Control register A
CRB_B		= $BFDF00	; Control register B

************************************************************************************
* Serial port

SERDAT		= $030	; Serial port data and stop bits write (transmit data buffer)
SERDATR		= $018	; Serial port data and status read (receive data buffer)
SERPER		= $032	; Serial port period and control

************************************************************************************
* Miscellaneous

* DMA

DMACON		= $096	; DMA control (clear or set)       write-only
DMACONR		= $002	; DMA control (and blitter status) read-only
		;
		; Bit#	Name	Description
		; ----	----	--------------------------------------------
		; 15	SET/CLR	Set or clear bits?
		; 14	BBUSY	Blitter busy status (read-only)
		; 13	BZERO	Blitter zero status (read-only).
		;		Remains 1 if, during a blitter operation,
		;		the blitter output was always zero.
		; 12	-	Unassigned
		; 11	-	Unassigned
		; 10	BLTPRI	Blitter DMA priority. Also called "blitter-nasty".
		;		When 1, blitter has full priority over 68000.
		; 09	DMAEN	DMA enable. Enables all DMA below.
		; 08	BPLEN	Bitplane DMA enable
		; 07	COPEN	Copper DMA enable
		; 06	BLTEN	Blitter DMA enable
		; 05	SPREN	Sprite DMA enable
		; 04	DSKEN	Disk DMA enable
		; 03	AUD3EN	Audio channel 3 DMA enable
		; 02	AUD2EN	Audio channel 2 DMA enable
		; 01	AUD1EN	Audio channel 1 DMA enable
		; 00	AUD0EN	Audio channel 0 DMA enable
		;
DMAF_SETCLR	= $8000
DMAF_AUD0	= $0001
DMAF_AUD1	= $0002
DMAF_AUD2	= $0004
DMAF_AUD3	= $0008
DMAF_DISK	= $0010
DMAF_SPRITE	= $0020
DMAF_BLITTER	= $0040
DMAF_COPPER	= $0080
DMAF_RASTER	= $0100
DMAF_MASTER	= $0200
DMAF_BLITHOG	= $0400
DMAF_BLTNZERO	= $2000
DMAF_BLTDONE	= $4000
DMAF_ALL	= $01FF

	rem
	D_setclr	= %1000000000000000
	D_inten		= %100000000000000
	D_priB		= %10000000000
	D_All		= %1000000000
	D_Btpl		= %100000000
	D_Copp		= %10000000
	D_Blit		= %1000000
	D_sprt		= %10100
	D_disk		= %10000
	D_aud		= %1111
	D_BlitHog	= %1111111111
	erem

* Disk access

DSKPTH		= $020	; Disk pointer (high 3 bits)
DSKPTL		= $022	; Disk pointer (low 15 bits)
DSKLEN		= $024	; Disk length
DSKDAT		= $026	; Disk DMA data write
DSKDATR		= $008	; Disk DMA data read (early read dummy address)
DSKBYTR		= $01A	; Disk data byte and status read
DSKSYNC		= $07E	; Disk sync, holds the match code for disk read synchro

* Other

STREQU		= $038	; Strobe for horizontal sync with VBL and EQU
STRVBL		= $03A	; Strobe for horizontal sync with VBL (vertical blank)
STRHOR		= $03C	; Strobe for horizontal sync
STRLONG		= $03E	; Strobe for identification of long horizontal line

VPOSR		= $004	; Read  vertical most significant bit (and frame flop)
VPOSW		= $02A	; Write vertical most significant bit (and frame flop)
VHPOSR		= $006	; Read  vertical and horiz position of beam/lightpen
VHPOSW		= $02C	; Write vertical and horiz position of beam/lightpen

REFPTR		= $028	; Refresh pointer
			; This register is used as a dynamic RAM refresh 
			; address generator. It is writeable for test purposes
			; only, and should never be written by the CPU.

; AllocMem flags
;
CHIPRAM		= 2
FASTRAM		= 4
CLEAR		= $10000
LARGEST		= $20000

; General Registers (mainly library routines)
;
LeftMouse	= 6
BlitterReady	= 6
SystemCopper1	= $26
SystemCopper2	= $32

* AGA / Hires

HTOTAL		= $1C0	; Highest number count in horiz line (VARBEAMEN = 1)
HSSTOP		= $1C2	; Horiz line pos for HSYNC stop
HBSTRT		= $1C4	; Horiz line pos for HBLANK start
HBSTOP		= $1C6	; Horiz line pos for HBLANK stop
VTOTAL		= $1C8	; Highest numbered vertical line (VARBEAMEN = 1)
VSSTOP		= $1CA	; Vert line for VBLANK start
VBSTRT		= $1CC	; Vert line for VBLANK start
VBSTOP		= $1CE	; Vert line for VBLANK stop
SPRHSTRT 	= $1D0	; UHRES sprite vertical start
SPRHSTOP	= $1D2	; UHRES sprite vertical stop
BPLHSTRT	= $1D4	; UHRES bit plane vertical stop
BPLHSTOP	= $1D6	; UHRES bit plane vertical stop
HHPOSW		= $1D8	; DUAL mode hires H beam counter write
HHPOSR		= $1DA	; DUAL mode hires H beam counter read
BEAMCON0	= $1DC	; Beam counter control register (SHRES,UHRES,PAL)
HSSTRT		= $1DE	; Horizontal sync start (VARHSY)
VSSTRT		= $1E0	; Vertical sync start (VARVSY)
HCENTER		= $1E2	; Horizontal pos for vsync on interlace
DIWHIGH		= $1E4	; Display window upper bits for start/stop
BPLHMOD		= $1E6	; UHRES bit plane modulo
SPRHPTH		= $1E8	; UHRES sprite pointer (high 5 bits)
SPRHPTL		= $1EA	; UHRES sprite pointer (low 15 bits)
BPLHPTH		= $1EC	; VRam (UHRES) bitplane pointer (hi 5 bits)
BPLHPTL		= $1EE	; VRam (UHRES) bitplane pointer (lo 15 bits)
FMODE		= $1FC	; Sprite mode

************************************************************************************
	endif
