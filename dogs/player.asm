
	xref P61_Init
	xref P61_End
	xref P61_Music
	;xref P61_SetPosition
	xref P61_E8

usecode		= $2806BE7C

editor_efx=0	; 1 to play EFx (Invert Loop) like ProTracker does in the
		; editor, which is different to the normal playroutine

split4=1	;Great time gain, but INCOMPATIBLE with F03, F02, and F01
		;speeds in the song! That's the ONLY reason it's default 0.
		;So ==> PLEASE try split4=1 in ANY mode!
		;Overrides splitchans to decrunch 1 chan/frame.
		;See ;@@ note for P61_SetPosition.

P61mode=1	;1=CIA, 2=VBLANK
		;Try other modes ONLY IF there are no Fxx commands >= 20.

P61pl=usecode&$400000

splitchans=1	;#channels to be split off to be decrunched at "playtime frame"
		;0=use normal "decrunch all channels in the same frame"
		;Experiment to find minimum rastertime, but it should be 1 or 2
		;for 3-4 channels songs and 0 or 1 with less channels.

visuctrs=0	;enables visualizers in this example: P61_visuctr0..3.w
		;containing #frames (#lev6ints if cia=1) elapsed since last
		;instrument triggered. (0=triggered this frame.)
		;Easy alternative to E8x or 1Fx sync commands.

asmonereport=0	;ONLY for printing a settings report on assembly. Use
		;if you get problems (only works in AsmOne/AsmPro, tho)

p61system=1	;1=system-friendly. Use for DOS/Workbench programs.

p61exec=0	;0 if execbase is destroyed, such as in a trackmo.

p61fade=0	;enable channel volume fading from your demo

channels=4	;<4 for game sound effects in the higher channels. Incompatible
		; with splitchans/split4.

playflag=0	;1=enable music on/off capability (at run-time). .If 0, you can
		;still do this by just, you know, not calling P61_Music...
		;It's a convenience function to "pause" music in CIA mode.

p61bigjtab=0	;1 to waste 480b and save max 56 cycles on 68000.

opt020=0	;1=enable optimizations for 020+. Please be 68000 compatible!
		;splitchans will already give MUCH bigger gains, and you can
		;try the MAXOPTI mode.

p61jump=0	;0 to leave out P61_SetPosition (size gain)
		;1 if you need to force-start at a given position fex in a game

C=0		;If you happen to have some $dffxxx value in a6, you can
		;change this to $xxx to not have to load it before P61_Music.

clraudxdat=0	;enable smoother start of quiet sounds. probably not needed.

optjmp=1	;0=safety check for jump beyond end of song. Clear it if you
		;play unknown P61 songs with erroneous Bxx/Dxx commands in them

oscillo=0	;1 to get a sample window (ptr, size) to read and display for
		;oscilloscope type effects (beta, noshorts=1, pad instruments)
		;IMPORTANT: see ;@@ note about chipmem dc.w buffer.

use1Fx=0	;Optional extra effect-sync trigger (*). If your module is free
		;from E commands, and you add E8x to sync stuff, this will
		;change the usecode to include a whole code block for all E
		;commands. You can avoid this by only using 1Fx. (You can
		;also use this as an extra sync command if E8x is not enough,
		;of course.)

quietstart=0	;attempt to avoid the very first click in some modules
		;IMPORTANT: see ;@@ note about chipmem dc.w buffer.

; -----------------------------------------------------------------------------------------------
; CIA mode options

	ifeq P61mode-1

p61cia	=1	;call P61_Music on the CIA interrupt instead of every frame.

lev6	=1	;1=keep the timer B int at least for setting DMA.
		;0="FBI mode" - ie. "Free the B-timer Interrupt".

		;0 requires noshorts=1, p61system=0, and that YOU make sure DMA
		;is set at 11 scanlines (700 usecs) after P61_Music is called.
		;AsmOne will warn you if requirements are wrong.

		;DMA bits will be poked in the address you pass in A4 to
		;P61_init. (Update P61_DMApokeAddr during playing if necessary,
		;for example if switching Coppers.)

		;P61_Init will still save old timer B settings, and initialize
		;it. P61_End will still restore timer B settings from P61_Init.
		;So don't count on it 'across calls' to these routines.
		;Using it after P61_Init and before P61_End is fine.

noshorts=0	;1 saves ~1 scanline, requires Lev6=0. Use if no instrument is
		;shorter than ~300 bytes (or extend them to > 300 bytes).
		;It does this by setting repeatpos/length the next frame
		;instead of after a few scanlines,so incompatible with MAXOPTI

dupedec	=0	;0=save 500 bytes and lose 26 cycles - I don't blame you. :)
		;1=splitchans or split4 must be on.

suppF01	=1	;0 is incompatible with CIA mode. It moves ~100 cycles of
		;next-pattern code to the less busy 2nd frame of a notestep.
		;If you really need it, you have to experiment as the support
		;is quite complex. Basically set it to 1 and try the various
		;P61modes, if none work, change some settings.
	endc

; -----------------------------------------------------------------------------------------------
; VBLANK mode options

	ifeq P61mode-2
p61cia	= 0
lev6	= 1	; still set sound DMA with a simple interrupt.
noshorts= 0	; try 1 (and pad short instruments if nec) for 1 scanline gain
dupedec	= 0
suppF01	= P61pl	; if 1, split4=1 may cause sound errors. but try it anyway. :)
	endc

; -----------------------------------------------------------------------------------------------

StopMusic:
	bra	P61_End

; -----------------------------------------------------------------------------------------------

	SECTION "PLAYER", CODE

;Note: if this is put in its own section (or compiled as separate binary), then
;jsr <addr>+P61_InitOffset,P61_MusicOffset,P61_EndOffset,P61_SetPositionOffset
;to call the routines.

Playrtn:	include	"P6112-Play_hr.i"

; -----------------------------------------------------------------------------------------------
