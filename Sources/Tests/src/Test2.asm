; --------------------------------------------------------------------
;
; Music Disk #3
;
; Test2. Triple buffer and one moving point
; --------------------------------------------------------------------

; -----------------------------------------------------------
; DEFINE

SCREENW					=	320
SCREENH					=	112
MUSIC					=	0
CIA_PLAYER				=	0
SPRITE_COUNT            =   8
FRAME_COUNT             =   512
LINE_PITCH	            =	40		; 40 octets par ligne
PROFILING				=	0

bitplanesizebytes = LINE_PITCH*SCREENH

SKIPINTRO=0

	code

	include "../../ldos/kernel.inc"
    
    ; Clean mem
    lea BufferChips,a0 ; must be aligned 4 bytes
    move.l #((endfx1chip-BufferChips)/16)-1,d0
.cleanmem:
    move.l #0,(a0)+
    move.l #0,(a0)+
    move.l #0,(a0)+
    move.l #0,(a0)+    
    dbra d0,.cleanmem

    bsr DoScreenSet
    bsr SetSpriteInCopper
  
    ; install interrupt handler
    bsr		pollVSync		
    move.w	#(1<<5),$dff09a			; disable VBL
    move.w	#(1<<5),$dff09c
    lea		copper,a0
    bsr		copperInstall
    move.l	#InterruptLevel3,$6c.w		;ma vbl
    move.w	#$8000|(1<<5)| (1<<6)|(1<<7)|(1<<8)|(1<<10),$dff096	; Sprite, Blitter, Copper, Bitplans, Nasty Bit
    move.w	#$c000|(1<<4),$dff09a		;interruption copper

    ; -- FX ---------------------------------------------------
    ; Init FX
    move.w #1,StepCurrent
    Bsr Fx1_Init
Loop1:
    Bsr Fx1_Loop
    cmp.w #1,StepCurrent
    beq Loop1 ; endless loop

    move.w #50*3,d0
    bsr WaitFrames ; Wait

    ; Stop with fade
	move.l (LDOS_BASE).w,a6
	jsr		LDOS_MUSIC_STOP(a6)  ; Stop with fade
    move.w #10,d0
    bsr WaitFrames ; Wait a bit (music fade out)       
    ; Unalloc Music space
    move.l	(LDOS_BASE).w,a6
    jsr		LDOS_FREE_MEM_MUSIC(a6)    
    move.l	(LDOS_BASE).w,a6
    jsr		LDOS_PRELOAD_NEXT_FX(a6)
    ; we now can terminate this part by RTS. Next part will execute a start music command
    rts ; End main loop, end of FX

StepCurrent:
    dc.w    0 ; 1 = Fx 1, 2 = Fx 2, 3 , 4
VBLCount:
    dc.w 0
WaitCount:
    dc.w 0    

WaitFrames:
    move.w #0,WaitCount
.wait:
    cmp.w WaitCount,d0
    bpl .wait
    rts

; ------------------------------------------------------		
InterruptLevel3:
    btst	#4,$dff01f
    beq.s	.intError
    ;IFNE	PROFILING
    ;move.w	#7,copPal+2
    ;ENDC
    movem.l	d0-a6,-(a7)

    add.w #1,VBLCount
    add.w #1,WaitCount
    
    cmp.w #1,StepCurrent
    bne .nostep1
    bsr Fx1_Irq
    bra .Irq_end
.nostep1:

.Irq_end:
    
    movem.l	(a7)+,d0-a6

    ;IFNE	PROFILING
    ;move.w	#0,$dff180
    ;ENDC
.none:		
    move.w	#1<<4,$dff09c		;clear copper interrupt bit
    move.w	#1<<4,$dff09c		;clear VBL interrupt bit
    nop
    rte
    
    ; -----------------------------------------------------------------
			
.intError:	
    illegal
			
pollVSync:	
    btst	#0,$dff005
    beq.s	pollVSync
.wdown:		
    btst	#0,$dff005
	bne.s	.wdown
	rts

copperInstall:
    move.w	#(1<<7),$dff096		; swith OFF copper DMA
    move.l	a0,$dff080
    move.w	#($8000|(1<<7)),$dff096
    rts

setPalette:	    
    lea		.palette(pc),a0
    lea		copPal,a1
    moveq	#8-1,d0
.Loop2:	        
    move.w	(a0)+,d1
    move.w	d1,2(a1)
    addq #4,a1
    dbf	d0,.Loop2
    rts

.palette:		
    dc.w	$000,$ddd,$ddd,$fff,$747,$605,$323,$555

; *****************************************************************************
; dfm_palette_fade0
; Fade a palette from or to zero
; color sources are interpolated to 0 using mul/div (current step, total step).
; Colors are words and result is copied into same size buffer as input.
; [in]	a0.l : Source palette (list of words)
; [in]	a1.l : Dest palette 
; [in]	d0.w : colors count
; [in]	d5.w : multiplier (Current steps ?)
; [in]	d6.w : decay. divider (number of steps ?) ... 

dfm_palette_fade0:
	move.w	d0,d4 ; backup color count
	sub.w	#1,d4
.calCol:
	clr.w	d0							; dest color
	; blue
	move.w	(a0),d3						; input color
	and.l	#$f,d3
	mulu	d5,d3
	ext.l	d3
	lsr.l	d6,d3
	cmp.w	#$f,d3
	ble.s	.noClampBlue
	move.w	#$f,d3
.noClampBlue	
	move.w	d3,d0
	; green
	move.w	(a0),d3						; input color
	lsr.w	#4,d3
	and.l	#$f,d3
	mulu	d5,d3
	ext.l	d3
	lsr.l	d6,d3
	cmp.w	#$f,d3
	ble.s	.noClampGreen
	move.w	#$f,d3
.noClampGreen
	lsl.w	#4,d3
	or.w	d3,d0
	; red
	move.w	(a0)+,d3						; input color
	lsr.w	#4,d3
	lsr.w	#4,d3
	and.l	#$f,d3
	mulu	d5,d3
	ext.l	d3
	lsr.l	d6,d3
	cmp.w	#$f,d3
	ble.s	.noClampRed
	move.w	#$f,d3
.noClampRed
	lsl.w	#4,d3
	lsl.w	#4,d3
	or.w	d3,d0
	move.w	d0,(a1)+
	dbra	d4,.calCol ; Big loop
	rts		

erasebackscreen:
    ; Erase HALF of the screen. Right part, we do not need to erase the other half
	move.l SCR3,a0
    add.l #20,a0    
    bsr waitblitter ; Wait blitter to be ready
	move.w #20,$dff066			;destination modulo
	move.l #$01000000,$dff040	;set operation type in BLTCON0/1
    move.l a0,$dff054   ;destination address
	move.w #(SCREENH*64)+(LINE_PITCH/2/2),$dff058 ;blitter operation size    
    rts

triplebufferswap:
    ; SWAP
	; -- Swap screens
	; Work -> Displayed 
	; WorkNext -> Work
	; Displayed -> WorkNext
	move.l ScreenDisplayed,a0
	move.l ScreenWork,a1
	move.l ScreenWorkNext,a2
	move.l a1,ScreenDisplayed	
	move.l a2,ScreenWork
	move.l a0,ScreenWorkNext
	
    move.b #1,ScreenAskSwap ; Ask swap buffer (in IRQ)
	move.b #0,ScreenSwapDone
    rts
    
WaitNextFrame:	
	cmp.b #1,ScreenSwapDone ; Wait for IRQ to be executed
	bne WaitNextFrame
    move.b #0,ScreenSwapDone    
    rts
    
waitblitter:	
    ;tst.w	$dff002
.bltwt:	
    btst	#6,$dff002
    bne.s   .bltwt
    rts
; --------------------------------------------------------	
DoScreenSet:
	; set bitplans
	move.l ScreenDisplayed,d0
	Lea copScrSet,a1
	swap d0
	move.w d0,2(a1)
	swap d0
	move.w d0,6(a1)
	add.l #8,a1
	add.l #bitplanesizebytes,d0
	; Plane2
	;swap d0
	;move.w d0,2(a1)
	;swap d0
	;move.w d0,6(a1)
	;add.l #8,a1
	;add.l #bitplanesizebytes,d0
	; Plane 3
	;swap d0
	;move.w d0,2(a1)
	;swap d0
	;move.w d0,6(a1)
    ; Clear flags
	move.b #0,ScreenAskSwap
	move.b #1,ScreenSwapDone
	rts
    
ClearScreen:
	bsr waitblitter
    
    ; Clean 96 pixels (12  bytes)
    move.w #0,$dff066			;destination modulo
	move.l #$01000000,$dff040	;set operation type in BLTCON0/1
	move.l ScreenWorkNext,d0 ; Clean 3rd buffer
    clr.l d1
    move.w 0,d1
	add.l d1,d0 ; Decay with. 
	move.l d0,$dff054   ;destination address
	move.w #SCREENH*64+(40/2),$dff058 ;blitter operation size   

    rts
    
    
; Triple buffer system
; The buffer we currently seeing
; The buffer we work in
; The buffer we worked after (to avoid transition problems)
ScreenDisplayed: ; What we see
		dc.l	screen1
ScreenWork: ; What is computed
		dc.l	screen2
ScreenWorkNext: ; This one is cleared
		dc.l	screen3		
		
        CNOP 0,4
        
ScreenAskSwap:
		dc.b	0
		even
ScreenSwapDone:
		dc.b	0
		even
countertest:
		dc.w	0

; ------------------------------------------------------
; FX1
; ------------------------------------------------------

Fx1_Init:

    
    rts

Fx1_Loop:

    ; Wait IRQ signal
    ; Swap screen
    ; Clear screen
    ; Compute position of points
    ; Draw triangles
    
    ; Swap screen
    bsr triplebufferswap
    ; Wait IRQ signal (swap of screen is done in IRQ)
    bsr WaitNextFrame
    ; Clear screen
    bsr ClearScreen 
    ; Compute position of points
    bsr ComputeAll
    ; Draw triangles
    bsr DrawAll

    rts

Fx1_Irq:
	; All code is outside the IRQ, we only synchronise the display
	; Check if we need to swap buffers
	cmp.b #1,ScreenAskSwap
	bne 	noswap
    bsr DoScreenSet
noswap:

    rts


Point1X:
    dc.w    0
Point1Y:
    dc.w    64
    

ComputeAll:

    move.w Point1X,d0
    add.w #1,d0
    and.w #$00ff,d0 ; 256 max
    move.w d0,Point1X

    move.w Point1Y,d0
    add.w #1,d0
    and.w #$003f,d0 ; 64 max
    move.w d0,Point1Y     

    rts
    
    ; Draw triangles
DrawAll:

    move.w Point1X,d1
    move.w Point1Y,d2
    bsr Fx1_DrawPixel
    
    

    rts

; ----------------------------------
; d1.w = X, d2.w = Y
Fx1_DrawPixel:
    movem.l a1/d1-d4,-(a7)
    ; Center screen
    ;add.w #(320-(79+85)-10)/2,d1 ; Center X
    ;add.w #(87-30)/2,d2 ; Center Y

    mulu.w #40,d2 ; Y
    move.b d1,d3 ; X
    lsr.b #3,d1 ; /8 = byte (0 to 40)
    and.l #$000000ff,d1
    and.l #$00000007,d3 ; pixel (0 to 7)
    ;move.b d1,$102
    ;move.b d3,$103
    move.l ScreenWork,a1
    add.l d2,a1 ; Y
    add.w d1,a1 ; X byte
    move.b #$80,d4
    lsr.b d3,d4
    or.b d4,(a1)
    movem.l (a7)+,a1/d1-d4   
    rts

; ------------------------------------------------------
; FX3 : Rotating letters, scrolling
; ------------------------------------------------------

Fx3_Init:
    bsr setPalette 
    ;bsr SetBackPalette
    
    move.w #SCROLLMINI,scrollcount ; For faster machines, be sure that scroll have done the minimum steps. USe SCROLLMINI so will not work for first time.
    
    ; Clean video memory
    bsr waitblitter ; Wait blitter to be ready
	move.w #0,$dff066			;destination modulo
	move.l #$01000000,$dff040	;set operation type in BLTCON0/1
	move.l #BufferChips,a0
    move.l a0,$dff054   ;destination address
	move.w #(SCREENH*4*64)+(LINE_PITCH/2),$dff058 ; blitter operation size. Triple buffer + scrolling plane
    bsr waitblitter ; Wait end of clean operation.  
    
    ;bsr BackgroundAnimation_TriggerBackAnim
    bsr triplebufferswap
    
    move.l #BigScroll,d0
    lea     copScrSet,a1
    addq.w  #8,a1
    move.w  d0,6(a1)
    swap    d0
    move.w  d0,2(a1)

    ;lea Planes87,a0
    ;move.w #($0200|(2<<12)),2(a0) ; 2 plane
    
    ;bsr Fx2_DisableCenterGradient ; disable copper gradient
    
    ; Clean array
    lea ArrayQuad,a0
    move.w #255,d0
.erase
    move.w #0,(a0)+
    dbra d0,.erase
    rts

; -------------------------------------------------------------------------------
Fx3_Loop:
    bsr waitblitter ; Be sure that erase is finished (for faster machines)
    bsr triplebufferswap
    bsr erasebackscreen ; SCR3
    bsr Fx3_drawletter ; This can take 1, 2, 3 frames depending on machine speed
.testVBL 
    cmp.w #2,VBLCount ; Some machine already have 2 VBL when coming here (some have 3).
    bge .ok
    bsr pollVSync
    bra .testVBL
.ok:
    move.w #0,VBLCount ; Reset VBL count.
    rts
    
; -------------------------------------------------------------------------------
Fx3_Irq:
    ;bsr Fx3_DoScroll ; Need to be done after scroll display.
    cmp.w #0,forceWait
    beq .notinuse
    sub.w #1,forceWait ; Decrease this counter here, so it is constant
.notinuse:    
    rts   
    
NBSTEPS = 18 ; Animation steps

    CNOP 0,8

drawstep:
    dc.l  (NBSTEPS-1)*100 ; 0 to 100*49  
currentletter:
    dc.l LettersSequence
LettersSequence:
    dc.l 0
  
    dc.l $ffffffff
  
forceWait: ; For Space character
    dc.w    0
scrollcount:
    dc.w    0 ; Be sure that scroll have enough moved before launching next letter.
    
SCROLLMINI = 58 ; // Minimum pixels before drawing next letter (for faster machines)

; -----------------------------------------------------
Fx3_drawletter:

    ; For space, need to wait some time before next letter.
    ; The counter decrease is done in IRQ, so time is constant on fast machines.
    ;move.w forceWait,$100
    cmp.w #0,forceWait
    beq .nowait
    ;sub.w #1,forceWait ; Done in IRQ
    rts
.nowait:
    ; If draw steps is over
    ;move.w scrollcount,$100
    ;move.l drawstep,$102
    cmp.l #(NBSTEPS-1)*100,drawstep ; Do we just ended a letter ?
    bne .nowaitscroll
    cmp.w #SCROLLMINI,scrollcount ; for faster machines, be sure scrolling have move enough
    bhi .nowaitscroll
    rts
.nowaitscroll:

    cmp.l #(NBSTEPS-1)*100,drawstep ; Do we just ended a letter ?
    bne .noresetscroll
    move.w #0,scrollcount ; reset scroll count
.noresetscroll:

    ; Draw Letter
    move.l currentletter,a1 ; A1 current polygon
    move.l (a1),a1
    cmp.l #0,a1 ; Space Again ?
    beq drawletter_nextletter
    cmp.l #$ffffffff,a1
    beq drawletter_EndSequence ; End sequence

    ; Draw a letter
    ;lea Pointrotating,a0
    ;add.l drawstep,a0
    
    ; Reset min and max. (will need to be updated)
    move.b #255,ArrayQuadMinY
    move.b #0,ArrayQuadMaxY ; This is max Y

drawletter_looppolygon:

    cmp.b #$fe,1(a1)
    beq drawletter_endpoly
    
    ; Draw line between index (a1) and 1(a1)
    moveq #0,d0
    moveq #0,d1
    move.b (a1),d0
    move.b 1(a1),d1
    subq #1,d0
    subq #1,d1
    bsr drawlineinarrayfromindex ; Draw line in array

    addq #1,a1
    bra drawletter_looppolygon ; Next line

drawletter_endpoly:
    
    bsr DrawArray
    addq #2,a1

    cmp.b #$ff,(a1)
    beq drawletter_end ; Last quad reached.
    
    bra drawletter_looppolygon ; Next polygon

drawletter_end:
    ; Next frame
    sub.l #100,drawstep
    cmp.l #-100,drawstep ; Last draw step was 0
    bne drawletter_end2
    ; End of one letter
    bsr copylettertoscroll
    move.l #(NBSTEPS-1)*100,drawstep
    ; -- next letter ------------------
drawletter_nextletter: 
    add.l #4,currentletter
    move.l currentletter,a0
    ; Test end sequence
    cmp.l #$ffffffff,(a0)
    bne drawletter_noend
drawletter_EndSequence:     
    ;move.w #$4,$100
    move.w #4,StepCurrent ; End current FX, go to next
    move.w #100,forceWait
    bra drawletter_end2
drawletter_noend:
    ; Test Space
    cmp.l #$0,(a0)
    bne drawletter_end2
    move.w #SCROLLMINI-15,forceWait ; Wait a bit to create space
    add.l #4,currentletter
    ;bra drawletter_end2
drawletter_end2:
    rts
    ; -------------------------
    ; d0 index 1
    ; d1 index 2
drawlineinarrayfromindex:
    movem.l a0-a1,-(a7)
    ; Get pixel coords
    lsl.l #1,d0 ; index Point 1
    lsl.l #1,d1 ; Index Point 2
    ;move.b #1,$102
    lea dataline,a4
    move.b 1(a0,d0.w),d2; 3(a4) ; Y1    
    move.b 1(a0,d1.w),d3; 7(a4) ; Y2
    ; Y going up or down ?
    ; Default is down
    ;move.w 2(a4),d6
    ;cmp.w 6(a4),d6
    cmp.b d2,d3
    bhi .nochange
    ; Going up, so need to switch points and draw on right side of array
    ; -- Exchange both points.
    move.l #1,ArrayOffsetIfUp
    move.b (a0,d1.w),1(a4) ; X2
    move.b (a0,d0.w),5(a4) ; X1    
    move.b d2,7(a4) ; Exchange Y
    move.b d3,3(a4)
    ; Exchange D2 and D3
    exg.b d2,d3
    bra .next
.nochange
    move.l #0,ArrayOffsetIfUp ; First descend
    move.b (a0,d0.w),1(a4) ; X1
    move.b d2,3(a4) ; No Exchange Y
    move.b (a0,d1.w),5(a4) ; X2 
    move.b d3,7(a4)
    
.next:
    ; -- Check min and max of Y
    ;move.b 3(a4),d0
    cmp.b ArrayQuadMinY,d2 ; start at 255, we want the min value
    bhi .nomin
    move.b d2,ArrayQuadMinY
.nomin
    ;move.b 7(a4),d0
    cmp.b ArrayQuadMaxY,d3 ; start at 0, we want the max value
    bmi .nomax
    move.b d3,ArrayQuadMaxY
.nomax

    lea dataline,a1
    bsr drawlineinarray

    movem.l (a7)+,a0-a1
    rts

;    Pointrotating: ; 49 points ; 50 frames. Reverse. 
; include "d:\Kristof\Amiga_Dev\OriensSequencerTool\Bin\framepoints.txt"
;
;LetterA:
;    dc.b 2,7,40,45,46,41,32,2,$fe
;    dc.b 2,32,29,2,$fe
;    dc.b 2,29,30,26,11,5,2,$fe
;    dc.b 30,35,43,48,49,44,26,30,$fe
;    dc.b $ff

; Letter is in SCR3 + 26 (to 40)
; Big scroll + 26 to 40
; 87 lines.

copylettertoscroll:
    movem.l a0-a1/d0,-(a7)
    ; Source
    move.l SCR2,a0
    add.l #26,a0
    add.l #21*40,a0 ; First lines are empty
    ; Dest
    move.l #BigScroll,a1
    add.l #26,a1
    add.l #21*40,a1 ; First lines are empty
    move.w #47-1,d0
.loop
    move.w (a0)+,d1
    or.w d1,(a1)+
    move.w (a0)+,d1
    or.w d1,(a1)+
    move.w (a0)+,(a1)+
    move.w (a0)+,(a1)+
    move.w (a0)+,(a1)+
    add.l #40-10,a0
    add.l #40-10,a1
    dbra.w d0,.loop
    
    ; Clear source.
    move.l SCR2,a0
    add.l #26,a0
    add.l #21*40,a0 ; First lines are empty
    move.w #47-1,d0
.loop2
    clr.w (a0)+
    clr.w (a0)+
    clr.w (a0)+
    clr.w (a0)+
    clr.w (a0)+
    add.l #40-10,a0
    dbra.w d0,.loop2
    
    movem.l (a7)+,a0-a1/d0
    rts
  
; ----------------------------------
; Draw array of 256 lines, with pair of X start X end.
; First pixel is 1 .... 8 is last pixel of first byte.
; 9 is first pixel of bytes 2 and so on.
DrawArray:
    ;move.w #$0f0,$dff180

    movem.l a0-a6/d0-d7,-(a7)

    moveq #0,d0
    moveq #0,d2
    move.b ArrayQuadMinY,d0
    move.b d0,d2 ; Keep start Y
    lsl.w #1,d0
    lea ArrayQuad,a0
    add.l d0,a0 ; Start Y adress
    
    moveq #0,d1
    move.b ArrayQuadMaxY,d1
    lsl.l #1,d1
    lea ArrayQuad,a1
    add.l d1,a1 ; End Y adress
    
    moveq #0,d1
    
    ; A3 = Line to draw
    moveq #0,d3
    move.l d2,d3
    move.l d2,d4
    
    ;mulu #40,d3
    lsl.l #5,d3;*32
    lsl.l #3,d4;*8

    move.l SCR2,a3
    add.l d3,a3 ; Start line Y
    add.l d4,a3 ; Start line Y
    add.l #26,a3 ; Offset X.
    
    ;move.b #1,$104 ; For breakpoint    
    
.drawArray
    ;move.w #$040,$dff180
    moveq.l #0,d1
    move.b (a0),d1 ; X start
    ;cmp.b #0,d1 ; Not possible to have 0 on X
    beq .next
    ; Same pixel ?
    move.b 1(a0),d0
    cmp.b d0,d1
    bne .noegal
    bsr DrawPixelOnLine ; A3 is start of current line
    bra .next
.noegal: 
    ;move.w #$060,$dff180
    cmp.b d0,d1 ; 1(a0) X start should always be lower than D2.
    bhi .next ; TODO should never happend (else line trace is bad) 

    ;moveq.l #0,d2
    moveq.l #0,d5  
    
    ; Get X start and X end.
    subq #1,d1 ; Align to rest from 0 to 7 (instead of 1 to 8)
    move.b d1,d5 
    and.b #$07,d5 ; Rest of division by 8, means, number of pixels, not to draw
    lsr.w #3,d1 ; Start byte
    move.l a3,a4 ; Copy line screen
    add.l d1,a4 ; Add start bytes screen 
    moveq #0,d2
    moveq #0,d6 ; Important
    move.b d0,d2 ; X End 1(a0)
    subq #1,d2
    move.b d2,d6
    and.b #$07,d6 ; Rest of division by 8, means, number of pixels, not to draw
    lsr.w #3,d2 ; End BYTE
    
    ; -- Test if start byte is same as end byte.
    cmp.b d1,d2
    bne .nosamebyte
    ;move.w #$080,$dff180
    ; Need to draw only the difference pixels
    ; D5 to D6
    ; Example Xstart=12, Xend=14.... Same byte. Pixel start 4 , pixel end 6.
    moveq #0,d7 ; Important
    move.b #$7,d0
    sub.b d5,d0 ; Start bit.
    ; Count bits to set
    sub.b d5,d6 ; Number of bits to set.
.setbits
    bset.b d0,d7
    subq #1,d0
    dbra d6,.setbits
    or.b d7,(a4)+
    bra .next ; -- End on same byte   
.nosamebyte:     
    ; -- Test if first byte is complete or not.
    cmp.b #0,d5
    beq .firstbyteiscomplete
    ;move.w #$0a0,$dff180
    ; process first byte
    move.w #$FF,d7 ; Full 8 pixels, decayed on right
    lsr.w d5,d7
    or.b d7,(a4)+ ; Print pixels
    addq #1,d1 ; Skip first byte
.firstbyteiscomplete: 
    ;move.w #$0c0,$dff180
    sub.b d1,d2 ; Number of steps
    beq .processlastbyte ; maybe we do not have some middle values, so go to end value
    subq #1,d2
.loopline ; -- Process all middle bytes
    ;cmp.b d1,d2 ; D1 should always be lower or equal than D2.
    ;beq .processlastbyte ; maybe we do not have some middle values, so go to end value
    ; Process all middle bytes (this can be optimized)
    ; D1 to D2 (not included)
    move.b #$ff,(a4)+
    ;add.l #1,a4 ; DEBUG, display nothing
    addq #1,d1    
    ;bra .loopline
    dbra d2,.loopline
    ; -- Process last byte
.processlastbyte
    ;move.w #$0e0,$dff180
    cmp.b #7,d6 ; 0 to 7
    beq .lastbyteiscomplete2
    move.w #$00FF,d7 ; Full 8 pixels, decayed on left
    moveq #$7,d2
    sub.w d6,d2 ; 1 to 7 converted to 7 to 1
    lsl.w d2,d7
    or.b d7,(a4) ; Print pixels
    bra .next
.lastbyteiscomplete2: 
    move.b #$FF,(a4)
.next:
    ;move.w #$0f0,$dff180
    
    addq #2,a0 ; next pair of X/X
    add.l #40,a3 ; Next line on screen.

    ; -- Test end of array
    cmp.l a0,a1
    beq EndDrawArray
    bra .drawArray
EndDrawArray:
    
    movem.l (a7)+,a0-a6/d0-d7
    
    ;move.w #$000,$dff180
    rts

; ----------------------------------
; d1.w = X, a3=Start of line
DrawPixelOnLine:
    movem.l a3/d1-d4,-(a7)
    subq #1,d1 ; Be sure to draw pixel 1 to 8 on one byte
    move.w d1,d3 ; X
    lsr.w #3,d1 ; /8 = byte (0 to 40)
    and.l #$000000ff,d1
    and.l #$00000007,d3 ; pixel (1 to 8) convert to 0 to 7
;    bne .no0
;    move.w #8,d3 ; If value is 0 then convert to 8
;    sub.l #1,d1
;.no0:
    add.w d1,a3 ; X byte
    move.w #$80,d4
    lsr.w d3,d4
    or.b d4,(a3)
    movem.l (a7)+,a3/d1-d4   
    rts
;---------------------------------------------------------------
; Toujours tracer vers Y en bas (augmentent). Seuls X peuvent aller droite ou gauche
; 0(a1) X1
; 2(a1) Y1
; 4(a1) X2
; 6(a1) Y2
drawlineinarray:
    ;move.w #$00F,$dff180
    movem.l d0-d6/a0-a2,-(a7)
    
    lea ArrayQuad,a2
    moveq #0,d3
    move.w 2(a1),d3
    lsl #1,d3 ; *2
    add.l d3,a2

    move.b 3(a1),d1 ; Same Y ? TODO special case, one line only
    cmp.b 7(a1),d1
    beq exitarrayOneLine
    
    add.l ArrayOffsetIfUp,a2 ; 0 or 1, column left or right    

    ; X2 = 0 is not possible
    cmp.b #0,5(a1)
    beq exitarray
    
    ; X1 = 0 is not possible
    cmp.b #0,1(a1)
    beq exitarray    

    bsr z3_calc_chiffre ; Calc increment.
    
    ; Trace ligne
    moveq #0,d1
    move.w (a1),d1 ; X start
    moveq #0,d2
    move.w 2(a1),d2 ; Y Start
    ;move.b #1,$101 ; Debug breakpoint
    move.w etape3d,d6
    subq #1,d6
    move.l incrx3d,d5
    move.l incry3d,d3 ; 1 or -1
    ;move.l incry3d,d7 ; 1 or -1
    ;move.l d7,d3 ; -1 or 1
    lsl.l #1,d3 ; -2 or 2 ; Increment Y
;    cmp.b #1,ArrayOffsetIfUp
;    bne .noup
;    add.l d3,a2 ; Next line in Y array (up or down) ; CK : Why ?
;.noup

    ; Add 0.5 to everyone, so do not change line for small values.
    ; USELESS
    ;add.l #$00008000,d1 ; Add 0.5 to round the pixel position


    ; Go left or right
    ; This seem bugged !! ==> Yes, USELESS
;    swap d1
;    cmp.l #0,d5
;    bpl .goright
;    ;sub.l #$00008000,d1 ; Go left, sub 0.5 to round the pixel position
;    sub.l #$00000800,d1 ; Go left, sub 0.5 to round the pixel position
;    bra .goend
;.goright    
;    ;add.l #$00008000,d1 ; Add 0.5 to round the pixel position
;    add.l #$00000800,d1 ; Add 0.5 to round the pixel position
;.goend
;    swap d1
    
.alllines
    move.b d1,(a2) ; Fill array
    ;bsr DrawPixel ; DEBUG d1=X d2=Y
    
    swap d1
    add.l d5,d1 ; Increment X
    swap d1
    
    ;add.l d7,d2 ; next Y
    add.l d3,a2 ; Next line in Y array (up or down)
    
    dbra d6,.alllines
    
exitarray:    
    movem.l (a7)+,d0-d6/a0-a2
    ;move.w #$000,$dff180
    rts
    
exitarrayOneLine
    ; Fill only one line, so fill X start, X end and exit.
    move.b 1(a1),(a2)
    move.b 5(a1),1(a2)
    ; 0(a1) X1
    ; 2(a1) Y1
    ; 4(a1) X2    
    movem.l (a7)+,d0-d6/a0-a2
    ;move.w #$000,$dff180
    rts  

    CNOP 0,4    

ArrayQuad:
    blk.w 256 ; // Each line is 2 bytes. X start, X end.
ArrayQuadMinY;
    dc.b    0
ArrayQuadMaxY;
    dc.b    0    
ArrayOffsetIfUp: ; 0 first column, 1 second column (going up)
    dc.l    0
    
; Point1 (X/Y) 12/14
; Point2       70/120
; Delta X = 70-12 = 58
; Delta Y = 120-14 = 106
; Chaque Y, X avance de 58/106 = 0,5471698113207547169811320754717
; En 16 bits 
; 35859,320754716981132075471698113 = $8c13

;$00008c00 .... Increment en float fixe. = incrx3d
;  incry3d = 1 or -1
;  etape3d.w = $6a = 106
; invert_incr_3x = 0 si on increment les X, sinon 1 si on decremente

dataline:
    dc.w    0,0
    dc.w    0,0
    
    CNOP 0,4

;---------------------------------------------------------------
incrx3d:		dc.l	0
incry3d:		dc.l	1
etape3d:		dc.w	0
		even
;----------------------------------------------------------------------
; .w (a1)  X1
; .w 2(a1) Y1
; .w 4(a1) X2
; .w 6(a1) Y2
; Will compute incrx3d, incry3d, etape3d and invert_incr_3x
z3_calc_chiffre:
    ;move.w #$f00,$dff180 ; RED
	move.l	#1,incry3d
	clr.b	d5; invert_incr_3x
	move.w	2(a1),d1 ; Y1
	move.w	6(a1),d0 ; Y2
	ext.l	d1
	ext.l	d0
	cmp.l	d1,d0
	bpl	ok_3y
	exg.l	d0,d1
	neg.l	incry3d
ok_3y:
	sub.l	d1,d0		; diff des y
    add.l   #1,d0       ; Add one to draw last Y. We want [Y1 to Y2] both included
	move.w	d0,etape3d	; nombre de points en y
    ; X
	move.w	(a1),d2
	move.w	4(a1),d1
	ext.l	d2
	ext.l	d1
	cmp.l	d2,d1
	bpl	ok_3x
	exg.l	d2,d1
	move.b	#1,d5; invert_incr_3x
ok_3x:	
    sub.l	d2,d1
    add.l #1,d1 ; Add 1 to have correct increment.
	; d0.l difference des y
	; d1.l difference des x
	move.l	d1,d2
	divu.w	d0,d2		; y x
	and.l	#$0000ffff,d2	; partie entière
	lsl.l	#8,d1	;x	; mulu $10000		pb si d1 > 255
	divu.w	d0,d1		; d1 = incrementation
	lsl.l	#8,d1		; ...	
	and.l	#$0000ffff,d1
	swap	d2
	add.l	d2,d1		; d1 le chiffre a vigule
	tst.b	d5; invert_incr_3x
	beq	.no_invert
	neg.l	d1
.no_invert
	move.l	d1,incrx3d
    ;move.w #$00F,$dff180
	rts
    
    CNOP 0,4

SCR1:           dc.l    screenBuffer1
SCR2:           dc.l    screenBuffer2
SCR3:           dc.l    screenBuffer3
frame:          dc.w    0

   
; ------------------------------------------- 
; Set Minimal Sprite in copper 
; While Sprite zone is computed
SetSpriteInCopper:
    ; Set 8 sprites to 0
 	Lea	SpritesCopper,a0
    move.l	#NullSprite,d0
    move.w #8-1,d1
.setnulsprite:    
	move.w	d0,6(a0)
	swap	d0
	move.w	d0,2(a0)
    swap    d0
    add.l #8*1,a0    
    dbra d1,.setnulsprite  

    ; Set test sprite
    ;Lea	SpritesCopper,a0
    ;move.l	#SpriteLine,d0
    ;Move.w	d0,6(a0)
	;Swap	d0
	;Move.w	d0,2(a0)

    ;Lea	SpritesCopper+8,a0
    ;move.l	#SpriteLine2,d0
    ;Move.w	d0,6(a0)
	;Swap	d0
	;Move.w	d0,2(a0)
    
    ; Also set blank planes and negative modulos
    
    ;move.l #EmpyLine,d0
    ;lea copScrSetPlan1,a1
    ;move.w  d0,6(a1)
    ;swap    d0
    ;move.w  d0,2(a1) 
    ; modulo is -40 so will loop on same line again and again

    ;move.l #EmpyLine,d0
    ;lea copScrSetPlan1b,a1
    ;move.w  d0,6(a1)
    ;swap    d0
    ;move.w  d0,2(a1) 
    ; modulo is -40 so will loop on same line again and again
    
    rts
 
; ----------------------------------------
; Data Fast mem
; ----------------------------------------
 
	data_f

        

; -----------------------------------------
; Data chip
; -----------------------------------------

	data_c

copper:	
                dc.l	$01fc0000
				; screen 320*256
				dc.l	$008e2881
				dc.l	$009028c1
				dc.l	$00920038
				dc.l	$009400d0				
copperdecay:
				dc.l	$01020000 ; Decay
				dc.l	$01060000
ModuloStart:
				dc.l	$01080000 ; Modulo 
				dc.l	$010a0000 ; Modulo
				dc.l	$01000200 | (0 <<12) ; 0 Planes

				dc.l	$009c8000 |(1<<4)		; fire copper interrupt
                
SpritesCopper: ; 8 sprites		 
        dc.l	$01200000,$01220000 ; Spr 0
		dc.l	$01240000,$01260000 ; Spr 1
        dc.l    $01280000,$012a0000 ; Spr 2
		dc.l	$012c0000,$012e0000 ; Spr 3
        dc.l    $01300000,$01320000 ; Spr 4
		dc.l	$01340000,$01360000 ; Spr 5
        dc.l    $01380000,$013a0000 ; Spr 6
		dc.l	$013c0000,$013e0000 ; Spr 7
        ; registres
        dc.l    $01040024 ; BPLCON2. All sprite in front.                

				dc.l	(24<<24)|($09fffe)      ; wait few lines so CPU has time to patch copper list
copPal:         
                dc.l    $01800008,$01820fff,$01840000,$01860000
                dc.l    $01880000,$018a0000,$018c0000,$018e0000
                dc.l    $01900000,$01920000,$01940000,$01960000
                dc.l    $01980000,$019a0000,$019c0000,$019e0000
                dc.l    $01a00000,$01a20000,$01a40000,$01a60000 ; all colors blacks
                dc.l    $01a80000,$01aa0000,$01ac0000,$01ae0000
                dc.l    $01b00000,$01b20000,$01b40000,$01b60000
                dc.l    $01b80000,$01ba0000,$01bc0000,$01be0000                
copScrSetPlans1:      
                dc.l    $00e00000,$00e20000
                
                ; -- Display xx planes, in far right of the screen.
                dc.l	(($28+72)<<24)|($09fffe)
                dc.l    $01800000
copScrSet:                 
                dc.l    $00e00000,$00e20000
                dc.l    $00e40000,$00e60000
                dc.l    $00e80000,$00ea0000
                dc.l    $00ec0000,$00ee0000
                dc.l    $00f00000,$00f20000
                dc.l    $00f40000,$00f60000
                ;dc.l	$01000200 | (6 <<12)
                dc.l	$01000200 | (1 <<12)

                dc.l	(($28+72+SCREENH)<<24)|($09fffe) ; 171
                dc.l	$01000200
                ; End planes
                dc.l    $01800008

                 ; Wait 255
                 dc.w $ffdf,$fffe  ; 255

				dc.l	-2

EmpyLine:
NullSprite:
  dc.l 0 ; Stop (4 bytes)
  blk.b 36,0 ; 40 bytes
  

; ----------------------------------------
; bss fast
; ----------------------------------------     
        
        bss_f

; ----------------------------------------
; Chip mem
; ----------------------------------------

        bss_c

        even

BufferChips:
     ds.b 80640 ; 112 lines 6 planes , 3 screens
     ds.b 40*20 ; Security

; Mapping
screen1= BufferChips
screen2= screen1 + bitplanesizebytes*6
screen3= screen2 + bitplanesizebytes*6
safezone= screen3 + bitplanesizebytes*6 ; blk.b 40*50 = 2000 ,$77
pointsprojeted= safezone + 40*50 ; blk.w 512*2 = 2048 ; Space for projected points  ; to screen (with center added)
endfx1chip=pointsprojeted

; Mapping for FX3 (Rotating letters, scrolling)
screenBuffer1 = BufferChips
screenBuffer2 = screenBuffer1 + LINE_PITCH*SCREENH
screenBuffer3 = screenBuffer2 + LINE_PITCH*SCREENH
BigScroll     = screenBuffer3 + LINE_PITCH*SCREENH



Sp1			=		4
Sp2			=		6
Sp3			=		10
Sp4			=		8
Dp1			=		46*2
Dp2			=		48*2
Dp3			=		36*2
Dp4			=		40*2

coprbase=$dff000
custom=coprbase
dmaconr=2
joy0dat=$a
joy1dat=$c
bltddat=$76
bltadat=$74
bltbdat=$72
bltcdat=$70
bltafwm=$44
bltalwm=$46
bltamod=$64
bltbmod=$62
bltcmod=$60
bltdmod=$66
bltcon0=$40
bltcon1=$42
bltsize=$58
bltapth=$50
bltaptl=$52
bltbpth=$4c
bltcpth=$48
bltdpth=$54
cop1lch=$80
copjmp1=$88
