
;IT CAN BE USED IN 4 MODES:
;-------------------------
; 1	CIA mode. Plays any song. Uses the CIA interrupt.
;	 - (default, P61_Music should not be called, easiest to use)

; 2	VBLANK mode. No F20 or higher commands must be used in the song.
;	 - (use when you want to decide when P61_Music should be called.)

; 3	COPPER mode. No F20 or higher commands must be used in the song.
;	 - (requires a timed call to P61_Music and a timed copper command 
;	    11 scanlines later to set sound DMA.)

; 4	MAXOPTI mode. No F20 or higher commands must be used in the song.
;	 - (requires a timed call to P61_Music and a timed copper command 
;	    11 scanlines later to set sound DMA. Maximum optimized.)


P61mode	=2	;Try other modes ONLY IF there are no Fxx commands >= 20.
		;(f.ex., P61.new_ditty only works with P61mode=1)


;;    ---  options common to all P61modes  ---

;usecode	= $3fffffff
usecode = -1
; Optimized version for all 9 modules of music disk 2
; SPEED PROBLEMS and SAMPLES PLAY BAD
;usecode = $0000BD5E | $2C00B61F | $0000945E | $0402BD5E | $2E00B65E | $0000B616 | $00009D5F  | $0000951E  | $0000B406
;usecode = $2E42BF5F ; OR of all 9 modules = Tempo and sample problems.
;usecode = $AE42FFFF

         ; -1 means use all features
        ;CHANGE! to the USE hexcode from P61con for a big 
		;CPU-time gain! (See module usecodes at end of source)
		;Multiple songs, single playroutine? Just "OR" the usecodes together!
		;...STOP! Have you changed it yet!? ;)
		;You will LOSE RASTERTIME AND FEATURES if you don't.

P61pl=usecode&$400000

split4	=0 ; MEL'O'DEE 2 : 	
        ;Great time gain, but INCOMPATIBLE with F03, F02, and F01
		;speeds in the song! That's the ONLY reason it's default 0.
		;So ==> PLEASE try split4=1 in ANY mode!
		;Overrides splitchans to decrunch 1 chan/frame.
		;See ;@@ note for P61_SetPosition.


splitchans=0	;#channels to be split off to be decrunched at "playtime frame"
		;0=use normal "decrunch all channels in the same frame"
		;Experiment to find minimum rastertime, but it should be 1 or 2
		;for 3-4 channels songs and 0 or 1 with less channels.

visuctrs=0	;enables visualizers in this example: P61_visuctr0..3.w 
		;containing #frames (#lev6ints if cia=1) elapsed since last
		;instrument triggered. (0=triggered this frame.)
		;Easy alternative to E8x or 1Fx sync commands.

asmonereport	=0	;ONLY for printing a settings report on assembly. Use
			;if you get problems (only works in AsmOne/AsmPro, tho)

p61system=0	;1=system-friendly. Use for DOS/Workbench programs.

p61exec	=0	;0 if execbase is destroyed, such as in a trackmo.

p61fade	=1	;enable channel volume fading from your demo

channels=4	;<4 for game sound effects in the higher channels. Incompatible
		; with splitchans/split4.

playflag=0	;1=enable music on/off capability (at run-time). .If 0, you can
		;still do this by just, you know, not calling P61_Music...
		;It's a convenience function to "pause" music in CIA mode.

p61bigjtab=0	;1 to waste 480b and save max 56 cycles on 68000.

opt020	=0	;1=enable optimizations for 020+. Please be 68000 compatible!
		;splitchans will already give MUCH bigger gains, and you can
		;try the MAXOPTI mode.

p61jump	=0	;0 to leave out P61_SetPosition (size gain)
		;1 if you need to force-start at a given position fex in a game

C	=0	;If you happen to have some $dffxxx value in a6, you can 
		;change this to $xxx to not have to load it before P61_Music.

clraudxdat=0	;enable smoother start of quiet sounds. probably not needed.

optjmp	=1	;0=safety check for jump beyond end of song. Clear it if you 
		;play unknown P61 songs with erroneous Bxx/Dxx commands in them

oscillo	=0	;1 to get a sample window (ptr, size) to read and display for 
		;oscilloscope type effects (beta, noshorts=1, pad instruments)
		;IMPORTANT: see ;@@ note about chipmem dc.w buffer.

quietstart=0	;attempt to avoid the very first click in some modules
		;IMPORTANT: see ;@@ note about chipmem dc.w buffer.

use1Fx=1	
        ;Optional extra effect-sync trigger (*). If your module is free
		;from E commands, and you add E8x to sync stuff, this will 
		;change the usecode to include a whole code block for all E 
		;commands. You can avoid this by only using 1Fx. (You can 
		;also use this as an extra sync command if E8x is not enough, 
		;of course.)

;(*) Slideup values>116 causes bugs in Protracker, and E8 causes extra-code 
;for all E-commands, so I used this. It's only faster if your song contains 0
;E-commands, so it's only useful to a few, I guess. Bit of cyclemania. :)

;Just like E8x, you will get the trigger after the P61_Music call, 1 frame 
;BEFORE it's heard. This is good, because it allows double-buffered graphics 
;or effects running at < 50 fps to show the trigger synced properly.


;;    ---  CIA mode options (default) ---

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
    
;;    ---  VBLANK mode options ---

	ifeq P61mode-2

p61cia	=0
lev6	=1	;still set sound DMA with a simple interrupt.
noshorts=0	;try 1 (and pad short instruments if nec) for 1 scanline gain
dupedec	=0
suppF01	=P61pl	;if 1, split4=1 may cause sound errors. but try it anyway. :)
	
	endc

	
	include "kernel.inc"
	
	
	ifeq P61mode-3

p61cia	=0
lev6	=0	;don't set sound DMA with an interrupt.
		;(use the copper to set sound DMA 11 scanlines after P61_Music)
noshorts=1	;You must pad instruments < 300 bytes for this mode to work.
dupedec	=0
suppF01	=P61pl	;if 1, split4=1 may cause sound errors. but try it anyway. :)

	endc


	bra.w	pr_init             ; 0
	bra.w	P61_End				; 4
	bra.w	pr_replay			; 8
	bra.w	musicGetInfo		; 12
	bra.w	musicSetVolume		; 16
	bra.w	musicSetPos			; 20
    bra.w	musicGetVolume		; 24
    bra.w   musicGetTrigger     ; 28 [CK]

	
musicGetInfo:
    move.w  P61_Pos(pc),d0
	moveq	#63,d1					; P61 player row moves backward
    sub.w	P61_rowpos(pc),d1
    rts 

musicSetVolume:
    move.l  a0,-(sp)
	lea		P61_Master(pc),a0
	move.w	d0,(a0)
    move.l  (sp)+,a0
	rts
    
musicGetVolume:
    move.l  a0,-(sp)
	lea		P61_Master(pc),a0
	move.w	(a0),d0
    move.l  (sp)+,a0
	rts    
    
musicSetPos:
    move.l  a0,-(sp)
	lea		LDOSSeqForceJump(pc),a0
	move.b	d0,(a0)
    move.l  (sp)+,a0
	rts
    
musicGetTrigger:
    move.l  a0,-(sp)
    ifne use1Fx    
	lea		P61_PTrig(pc),a0
	move.w	(a0),d0
    move.w #0,(a0)
    endc
    move.l  (sp)+,a0
    rts    
	
	; input: a0: music score
	; 		 a1: samples
pr_init:
    lea		P61_Pos(pc),a2
	clr.w	(a2)

	sub.l a2,a2 ; A2 [LONG] = Address of sample buffer
	moveq #1,d0			; PAL
	sub.l a4,a4 ; A4 [LONG] = Address where 'DMA ON' byte (low 8 bits of DMACON)
    
	bsr P61_Init
	
    ; CK : irq for music is CIA, so this might be important
	moveq	#125,d0
	move.l	(LDOS_BASE).w,a6
	jsr		LDOS_SET_KERNEL_BPM(a6)
	
	rts

pr_replay:
	lea		$dff000,a6
	bsr		P61_Music
	rts
	
	
	;include "P6110-Play.i"
    include "P6112-Play.i" ; CK Update
