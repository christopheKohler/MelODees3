; --------------------------------------------------------------------
;
; Music Disk #3
;
; Test3. Triple buffer and rotating triangle
; --------------------------------------------------------------------

; 3D system. Objects: in 8:8 format. 
; These objects are rotated in local, then translated in world. world placement is 24:8 format.
; 8:8 mul 8:8 give 8:8x , need to shift 8 bits right to come back to 8:8
;
;
;
;

; -----------------------------------------------------------
; DEFINE FOR DEBUG
PROFILING				=	0
MUSIC					=	0

; -----------------------------------------------------------
; DEFINE

SCREENW					=	320
SCREENH					=	112
CIA_PLAYER				=	0
;SPRITE_COUNT            =   8
;FRAME_COUNT             =   512
LINE_PITCH	            =	40		; 40 octets par ligne
bitplanesizebytes = LINE_PITCH*SCREENH

; -----------------------------------------------------------
; CODE START

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

    ; ----------------------------------------------------
    ; The main logic is here. We can chain several FX
    ; This is the main loop, out of IRQ
    move.w #1,StepCurrent
    Bsr Fx1_Init
Loop1:
    Bsr Fx1_Loop
    cmp.w #1,StepCurrent
    beq Loop1 ; endless loop
    ; -----------------------------------------------------

    ; Exiting

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

; ------------------------------------------------------	

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
.loop:	        
    move.w	(a0)+,d1
    move.w	d1,2(a1)
    addq #4,a1
    dbf	d0,.loop
    rts

.palette:		
    dc.w	$000,$ddd,$ddd,$fff,$747,$605,$323,$555

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

; Main CPU loop (out of IRQ)
; This can take as much VBL as needed. 
Fx1_Loop:
 
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


; Compute rotating triangle
ComputeAll:

    ; Angle, increase
    add.w   #2,Angle
    and.w #$3FF,Angle
    
    ; Rotation around 0
    clr.l d0
    move.w Angle,d0
    
    lea PointsSource,a0
    lea PointsTransformed,a1

    ; 2D Rotation formula:
    ; x′=x*cosθ – y*sinθ
    ; y′=x*sinθ + y*cosθ
    ; cos_tab_data:
    ; sin_tab_data:    
    ; index 0   is 0
    ; index 256 is 180
    ; index 512 is 360

    ; ----------------
    ; cosθ = d5
    ; sinθ = d6
    clr.l d5
    clr.l d6
    lea cos_tab_data,a2
    move.w (a2,d0.w),d5 ; cos 7:9
    ext.l d5
    lsr.l #1,d5 ; 8:8
    lea sin_tab_data,a2
    move.w (a2,d0.w),d6 ; sin 7:9
    ext.l d6
    lsr.l #1,d6 ; 8:8
    
    ; Rotate all points of table, one by one
looprotate:
    clr.l d0
    clr.l d1
    clr.l d4 ; temp
    move.w (a0)+,d0 ; X
    move.w (a0)+,d1 ; Y
    clr.l d2 ; X'
    clr.l d3 ; Y'
    ; -------------- Compute x′=x*cosθ–y*sinθ
    move.w d0,d2
    muls.w d5,d2 
    lsr.l #8,d2 ; x′=x*cosθ   ( – y*sinθ )
    move.w d1,d4 ; Y
    muls.w d6,d4 
    lsr.l #8,d4 ; y*sinθ
    sub.l d4,d2 ; 8:8 x′=x*cosθ–y*sinθ )
    ; -------------- Compute y′=x*sinθ + y*cosθ
    move.w d0,d3
    muls.w d6,d3 
    lsr.l #8,d3 ; x*sinθ
    move.w d1,d4 ; Y
    muls.w d5,d4 
    lsr.l #8,d4 ; y*cosθ
    add.l d4,d3 ; 8:8 y′=x*sinθ + y*cosθ
    ; -- Compute pixel
    lsr.l #8,d2
    ;add.l #160,d2
    add.l #60,d2
    lsr.l #8,d3
    add.l #(112/2),d3   
    move.w d2,(a1)+
    move.w d3,(a1)+
    ; Next point
    cmp.w #$ffff,(a0)
    bne looprotate

;    move.w Point1X,d0
;    add.w #1,d0
;    and.w #$00ff,d0 ; 256 max
;    move.w d0,Point1X
;
;    move.w Point1Y,d0
;    add.w #1,d0
;    and.w #$003f,d0 ; 64 max
;    move.w d0,Point1Y     

    rts
    

; 0 to 112. Middle is 55. Rotate 40 arond 50.
; rotation 160,50.
; 3 points around 0
PointsSource:
    dc.w    0,-40<<8 ; Counter clock wise
    dc.w    -30<<8,40<<8    
    dc.w    30<<8,40<<8

    dc.w $ffff,$ffff
    
PointsTransformed:
    dc.w    160,10
    dc.w    190,90
    dc.w    130,90
    dc.w     0,0

Angle:
    dc.w    0    
    
    
    ; Draw triangles
DrawAll:
    bsr DrawPolygon
    

    ; Draw pixels.
    ;    lea PointsTransformed,a0
    ;    move.w (a0)+,d1
    ;    move.w (a0)+,d2
    ;    bsr DrawPixel
    ;    move.w (a0)+,d1
    ;    move.w (a0)+,d2
    ;    bsr DrawPixel    
    ;    move.w (a0)+,d1
    ;    move.w (a0)+,d2
    ;    bsr DrawPixel   

    rts

; ----------------------------------
; d1.w = X, d2.w = Y
; Debug routine for drawing a pixel
DrawPixel:
    movem.l a1/d1-d4,-(a7)
    mulu.w #40,d2 ; Y
    move.b d1,d3 ; X
    lsr.b #3,d1 ; /8 = byte (0 to 40)
    and.l #$000000ff,d1
    and.l #$00000007,d3 ; pixel (0 to 7)
    move.l ScreenWork,a1
    add.l d2,a1 ; Y
    add.w d1,a1 ; X byte
    move.b #$80,d4
    lsr.b d3,d4
    or.b d4,(a1)
    movem.l (a7)+,a1/d1-d4   
    rts



;alpha:		dc.w	0
;Compt_alpha:	dc.w	0
;mode_sinus:	dc.w	2	; 1 2 3 4 6
;angle_initial:	dc.w	0

;Current_objet:		Dc.l	PointsSource
;Current_objet_Dest:	Dc.l	PointsTransformed

; Choisir format de nombre a virgule.
; Le meme que dans The Fall. 

	; 8:8 format is entire:fixed float.
	; 00:00 is 0.0f
	; 00:80 is 0.5f
	; 00:C0 is 0.75f
	; 01:00 is 1.0f
	; FF:80 is -0.5f
	; FF:40 is -0.75f
	; FF:00 is -1
	;
	; 04:80 (4.50) * 02:c0 (2.75) = 12.375 ( 0c:60 ) 
	;
	; 8 bits signed numbers:
	; Negative numbers (complements 2)
	; NOT +1
	; 80 -128
	; FF -1
	; 00 0
	; 01 1
	; 7F +12
    
; Rotation 2D, formule theorique.
;
; x′=x*cosθ – y*sinθ
; y′=x*sinθ + y*cosθ
;
; Travailler sur les echelles !! Definir les limites. C'est important.
;
;
;
; Ecrire en 68000
;
;
;



;----------------------------------------- 
; DrawPolygon 
; First compute three lines and store the result into a array of 256 lines
; In that array, start and end on a scanlines is stored. 
; The array is drawn at end when all lines are computed.
; When drawing the line, we know if this is the start or end of the scanline
; start is stored ledt in array, and end is stored right in array.
DrawPolygon:
    ; Reset min and max. (will need to be updated)
    move.b #255,ArrayQuadMinY
    move.b #0,ArrayQuadMaxY ; This is max Y
    
    ; Draw line between index (a1) and 1(a1)
    lea PointsTransformed,a0
    moveq #0,d0
    moveq #0,d1
    move.w #0,d0
    move.w #1,d1
    bsr drawlineinarrayfromindex ; Draw line in array
    move.w #1,d0
    move.w #2,d1
    bsr drawlineinarrayfromindex ; Draw line in array
    move.w #2,d0
    move.w #0,d1
    bsr drawlineinarrayfromindex ; Draw line in array
    
    bsr DrawArray ; Once polygon is full line drawn, then draw each scanline.
    rts

    ; -------------------------
    ; d0 index 1
    ; d1 index 2
drawlineinarrayfromindex:
    movem.l a0-a1,-(a7)
    clr.l d2
    clr.l d3
    ; Get pixel coords
    lsl.l #2,d0 ; index Point 1. x4 to get 2 words.
    lsl.l #2,d1 ; Index Point 2
    ;move.b #1,$102
    lea dataline,a4
    move.w 2(a0,d0.w),d2; 3(a4) ; Y1    
    move.w 2(a0,d1.w),d3; 7(a4) ; Y2
    
    ; Y going up or down ?
    ; Default is down
    cmp.w d2,d3
    bhi .nochange
    ; Going up, so need to switch points and draw on right side of array
    ; -- Exchange both points.
    move.l #1,ArrayOffsetIfUp
    move.w (a0,d1.w),(a4) ; X2
    move.w (a0,d0.w),4(a4) ; X1    
    move.w d2,6(a4) ; Exchange Y
    move.w d3,2(a4)
    ; Exchange D2 and D3
    exg.w d2,d3
    bra .next
.nochange
    move.l #0,ArrayOffsetIfUp ; First descend
    move.w (a0,d0.w),(a4) ; X1
    move.w d2,2(a4) ; No Exchange Y
    move.w (a0,d1.w),4(a4) ; X2 
    move.w d3,6(a4)
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
    move.w 2(a1),d3; Y1
    lsl #1,d3 ; *2
    add.l d3,a2 ; ArrayQuad

    move.w 2(a1),d1 ; Same Y ? TODO special case, one line only
    cmp.w 6(a1),d1
    beq exitarrayOneLine
    
    add.l ArrayOffsetIfUp,a2 ; 0 or 1, column left or right    

    ; X2 = 0 is not possible
    cmp.w #0,4(a1)
    beq exitarray
    
    ; X1 = 0 is not possible
    cmp.w #0,(a1)
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
    lsl.l #1,d3 ; -2 or 2 ; Increment Y
    
.alllines
    move.b d1,(a2) ; Fill array ; TODO in words !!
    
    ;bsr DrawPixel ; DEBUG d1=X d2=Y
    
    swap d1
    add.l d5,d1 ; Increment X
    swap d1
    
    ;add.l #1,d2 ; next Y DEBUG
    
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

;----------------------------------------------------------------------
; Compute line increment
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
    
incrx3d:		dc.l	0
incry3d:		dc.l	1
etape3d:		dc.w	0

; ----------------------------------
; DrawArray
; This is the place where we really draw the polygon on screen.
; Each line of array is a scanline, with pixel start and pixel stop.
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
    add.l d0,a0 ; ArrayQuad : Start Y adress
    
    moveq #0,d1
    move.b ArrayQuadMaxY,d1
    lsl.l #1,d1
    lea ArrayQuad,a1
    add.l d1,a1 ; ArrayQuad :End Y adress
    
    moveq #0,d1
    
    ; A3 = Line to draw
    moveq #0,d3
    move.l d2,d3
    move.l d2,d4
    
    ;mulu #40,d3 ; Y * 40
    lsl.l #5,d3;*32
    lsl.l #3,d4;*8

    move.l ScreenWork,a3
    add.l d3,a3
    add.l d4,a3 ; Start line Y
    ;add.l #26,a3 ; Offset X.
    
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

    CNOP 0,4    

ArrayQuad: ; This is the array representing the polygon on screen.
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
    
    rts
    
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

; ---------------------------------------------
; Clipping routine to check

Xm=319
Ym=256

Clip_it:
	move.w	d4,Point_avantsauve
	move.w	d3,Point_avantsauve+2

	Movem.l	d0-d2/a0-a2,-(sp)

	clr.l	Retour_point

	movem.l	Blank,d0-d2

	move.w	d4,d0
	move.w	d3,d1

	Bsr Valide

	tst.l	Clip_Bilan
	bne	Clip_oblige

	cmp.w	#1,Clip_ok
	beq	no_clip	
	
	move.l	Clip_bilansauve,Clip_Bilan

	move.l	#4,Retour_point
	move.w	#1,Clip_ok
	bra	Clip_suite		; fabrique un point manquant
Clip_oblige:
	move.w	#-1,Clip_ok
Clip_suite:
	Bsr	Clip

	move.w	d0,d4
	move.w	d1,d3
no_clip:
	Movem.l	(sp)+,d0-d2/a0-a2

	move.w	d4,(a1)+		; pose dans table de construction
	move.w	d3,(a1)+

	move.w	Point_avantsauve,Point_avant
	move.w 	Point_avantsauve+2,Point_avant+2

	move.l	Clip_Bilan,Clip_bilansauve

	sub.l	Retour_point,a0

	Rts
;------------------------------------	
Retour_point:	
    dc.l	0
;------------------------------------	
; Point valide dans ecran ? .........
;------------------------------------
Valide:
	clr.l	Clip_Bilan

	cmp.w	#0,d0		; X < 0 ?
	bpl	No_x_minus	
	move.b	#1,Clip_Bilan	
No_x_minus:
	cmp.w	#Xm,d0		; X > 320 ?
	bmi	No_x_max	
	move.b	#1,Clip_Bilan+1
No_x_max:
	cmp.w	#0,d1		; Y < 0 ?
	bpl	No_y_minus	
	move.b	#1,Clip_Bilan+2	
No_y_minus:
	cmp.w	#Ym,d1		; Y > 256 ?
	bmi	No_y_max	
	move.b	#1,Clip_Bilan+3
No_y_max:
    rts
;-------------------------------------------------------------------
Clip_ok:		dc.w	1

Point_avant:		dc.w	0,0		; sauvegarde du dernier point
Point_avantsauve:	dc.w	0,0		; sauvegarde du dernier point
Clip_Bilan:		dc.l	0		; bilan des test clipping
Clip_bilansauve:	dc.l	0		; bilan des test clipping
Blank:			blk.l	6,0		; vide
;-------------------------------------------------------------------
Clip:
	move.w	d0,d2
	move.w	d1,d3

	move.w	d0,d4
	move.w	d1,d5
;-----------------------------------------
C_gauche:
	tst.b	Clip_Bilan
	beq	No_x_Upperflow

	lea	Point_avant,a0		; Clip sur la gauche X=0

	sub.w	(a0),d2	
	sub.w	2(a0),d3

	muls	(a0),d3		; 
	divs	d2,d3		;d2
	neg	d3

	add.w	2(a0),d3

	cmp.w	#0,d3		
	bmi	C_haut	

	cmp.w	#Ym,d3
	bpl	C_Bas	
	
;	move.w	d0,Point_avant
;	move.w	d1,Point_avant+2

	movem.l	Blank,d0-d1
	move.w	d3,d1		; Y
	
	Rts
;-----------------------------------------
No_x_Upperflow:

	Tst.b	Clip_Bilan+2		; Y < 0 
	Beq	No_y_Upperflow
C_haut:
	move.w	d4,d0
	move.w	d5,d1

	lea	Point_avant,a0

	Sub.w	2(a0),d3
	Sub.w	(a0),d2	

	muls	d1,d2
	divs	d3,d2
	
	Neg	d2
	Add.w	d0,d2

;	move.w	d0,Point_avant
;	move.w	d1,Point_avant+2

	movem.l	Blank,d0-d1

	move.w	d2,d0		; X

	Rts
;-----------------------------------------
No_y_Upperflow:

	Tst.b	Clip_Bilan+1	; X > 320
	Beq	No_x_Overflow
C_Droite:
	lea	Point_avant,a0

	Sub.w	2(a0),d3
	Sub.w	(a0),d2	

	move.w	d0,d4
	move.w	d1,d5

	Neg	d0

	add.w	#Xm,d0
	muls	d3,d0
	divs	d2,d0
	add.w	d1,d0

	Cmp.w	#0,d0		
	Bmi	C_haut	

	Cmp.w	#Ym,d0
	Bpl	C_Bas	

;	move.w	d4,point_avant
;	move.w	d5,point_avant+2

	clr.l	d1
	move.w	d0,d1		; Y
	move.l	#Xm,d0		

	Rts
;-----------------------------------------
No_x_Overflow:

	Tst.b	Clip_Bilan+3	; Y > 256
	Beq	No_y_Overflow
C_Bas:
	move.w	d4,d0
	move.w	d5,d1

	lea	Point_avant,a0

	Sub.w	2(a0),d3
	Sub.w	(a0),d2	

;	move.w	d0,Point_avant
;	move.w	d1,Point_avant+2

	neg	d1

	add.w	#Ym,d1
	muls	d2,d1
	divs	d3,d1
	add.w	d0,d1

	clr.l	d0
	move.w	d1,d0		; X
	move.l	#Ym,d1		

	Rts
;-----------------------------------------
No_y_Overflow:
	Rts
 
; ----------------------------------------
; Data Fast mem
; ----------------------------------------
 
	data_f

; cos and sin tabs with 512 entries multipled by 512
; 512 values. index 0 to 1024 (10bits). We can use 8:8 and lsr by 6 to get index. 
; means our angle go from 0 to FF:FF in 8:8 (no need to mask can never overflow). Need to be pair, so and last bit
; 9 bits precision.
; 1 is 0100 in 8:8
; here 1 is 511 = 9 bits, need to be shift one bit right
; index 0   is 0
; index 256 is 180
; index 512 is 360

; [BF START]
cos_tab_data:
	dc.w 511,511,511,511,511,511,510,510,509,508
	dc.w 508,507,506,505,504,503,502,500,499,498
	dc.w 496,495,493,491,489,488,486,484,482,479
	dc.w 477,475,473,470,468,465,462,460,457,454
	dc.w 451,448,445,442,439,435,432,429,425,422
	dc.w 418,414,411,407,403,399,395,391,387,383
	dc.w 379,375,370,366,362,357,353,348,343,339
	dc.w 334,329,324,319,314,310,304,299,294,289
	dc.w 284,279,273,268,263,257,252,246,241,235
	dc.w 230,224,218,213,207,201,195,190,184,178
	dc.w 172,166,160,154,148,142,136,130,124,118
	dc.w 112,106,99,93,87,81,75,68,62,56
	dc.w 50,43,37,31,25,18,12,6,0,-7
	dc.w -13,-19,-26,-32,-38,-44,-51,-57,-63,-69
	dc.w -76,-82,-88,-94,-100,-107,-113,-119,-125,-131
	dc.w -137,-143,-149,-155,-161,-167,-173,-179,-185,-191
	dc.w -196,-202,-208,-214,-219,-225,-231,-236,-242,-247
	dc.w -253,-258,-264,-269,-274,-280,-285,-290,-295,-300
	dc.w -305,-311,-315,-320,-325,-330,-335,-340,-344,-349
	dc.w -354,-358,-363,-367,-371,-376,-380,-384,-388,-392
	dc.w -396,-400,-404,-408,-412,-415,-419,-423,-426,-430
	dc.w -433,-436,-440,-443,-446,-449,-452,-455,-458,-461
	dc.w -463,-466,-469,-471,-474,-476,-478,-480,-483,-485
	dc.w -487,-489,-490,-492,-494,-496,-497,-499,-500,-501
	dc.w -503,-504,-505,-506,-507,-508,-509,-509,-510,-511
	dc.w -511,-511,-511,-511,-511,-511,-511,-511,-511,-511
	dc.w -511,-511,-511,-511,-510,-509,-509,-508,-507,-506
	dc.w -505,-504,-503,-501,-500,-499,-497,-496,-494,-492
	dc.w -490,-489,-487,-485,-483,-480,-478,-476,-474,-471
	dc.w -469,-466,-463,-461,-458,-455,-452,-449,-446,-443
	dc.w -440,-436,-433,-430,-426,-423,-419,-415,-412,-408
	dc.w -404,-400,-396,-392,-388,-384,-380,-376,-371,-367
	dc.w -363,-358,-354,-349,-344,-340,-335,-330,-325,-320
	dc.w -315,-311,-305,-300,-295,-290,-285,-280,-274,-269
	dc.w -264,-258,-253,-247,-242,-236,-231,-225,-219,-214
	dc.w -208,-202,-196,-191,-185,-179,-173,-167,-161,-155
	dc.w -149,-143,-137,-131,-125,-119,-113,-107,-100,-94
	dc.w -88,-82,-76,-69,-63,-57,-51,-44,-38,-32
	dc.w -26,-19,-13,-7,-1,6,12,18,25,31
	dc.w 37,43,50,56,62,68,75,81,87,93
	dc.w 99,106,112,118,124,130,136,142,148,154
	dc.w 160,166,172,178,184,190,195,201,207,213
	dc.w 218,224,230,235,241,246,252,257,263,268
	dc.w 273,279,284,289,294,299,304,310,314,319
	dc.w 324,329,334,339,343,348,353,357,362,366
	dc.w 370,375,379,383,387,391,395,399,403,407
	dc.w 411,414,418,422,425,429,432,435,439,442
	dc.w 445,448,451,454,457,460,462,465,468,470
	dc.w 473,475,477,479,482,484,486,488,489,491
	dc.w 493,495,496,498,499,500,502,503,504,505
	dc.w 506,507,508,508,509,510,510,511,511,511
	dc.w 511,511
sin_tab_data:
	dc.w 0,6,12,18,25,31,37,43,50,56
	dc.w 62,68,75,81,87,93,99,106,112,118
	dc.w 124,130,136,142,148,154,160,166,172,178
	dc.w 184,190,195,201,207,213,218,224,230,235
	dc.w 241,246,252,257,263,268,273,279,284,289
	dc.w 294,299,304,310,314,319,324,329,334,339
	dc.w 343,348,353,357,362,366,370,375,379,383
	dc.w 387,391,395,399,403,407,411,414,418,422
	dc.w 425,429,432,435,439,442,445,448,451,454
	dc.w 457,460,462,465,468,470,473,475,477,479
	dc.w 482,484,486,488,489,491,493,495,496,498
	dc.w 499,500,502,503,504,505,506,507,508,508
	dc.w 509,510,510,511,511,511,511,511,511,511
	dc.w 511,511,511,511,510,510,509,508,508,507
	dc.w 506,505,504,503,502,500,499,498,496,495
	dc.w 493,491,489,488,486,484,482,479,477,475
	dc.w 473,470,468,465,462,460,457,454,451,448
	dc.w 445,442,439,435,432,429,425,422,418,414
	dc.w 411,407,403,399,395,391,387,383,379,375
	dc.w 370,366,362,357,353,348,343,339,334,329
	dc.w 324,319,314,310,304,299,294,289,284,279
	dc.w 273,268,263,257,252,246,241,235,230,224
	dc.w 218,213,207,201,195,190,184,178,172,166
	dc.w 160,154,148,142,136,130,124,118,112,106
	dc.w 99,93,87,81,75,68,62,56,50,43
	dc.w 37,31,25,18,12,6,0,-7,-13,-19
	dc.w -26,-32,-38,-44,-51,-57,-63,-69,-76,-82
	dc.w -88,-94,-100,-107,-113,-119,-125,-131,-137,-143
	dc.w -149,-155,-161,-167,-173,-179,-185,-191,-196,-202
	dc.w -208,-214,-219,-225,-231,-236,-242,-247,-253,-258
	dc.w -264,-269,-274,-280,-285,-290,-295,-300,-305,-311
	dc.w -315,-320,-325,-330,-335,-340,-344,-349,-354,-358
	dc.w -363,-367,-371,-376,-380,-384,-388,-392,-396,-400
	dc.w -404,-408,-412,-415,-419,-423,-426,-430,-433,-436
	dc.w -440,-443,-446,-449,-452,-455,-458,-461,-463,-466
	dc.w -469,-471,-474,-476,-478,-480,-483,-485,-487,-489
	dc.w -490,-492,-494,-496,-497,-499,-500,-501,-503,-504
	dc.w -505,-506,-507,-508,-509,-509,-510,-511,-511,-511
	dc.w -511,-511,-511,-511,-511,-511,-511,-511,-511,-511
	dc.w -511,-511,-510,-509,-509,-508,-507,-506,-505,-504
	dc.w -503,-501,-500,-499,-497,-496,-494,-492,-490,-489
	dc.w -487,-485,-483,-480,-478,-476,-474,-471,-469,-466
	dc.w -463,-461,-458,-455,-452,-449,-446,-443,-440,-436
	dc.w -433,-430,-426,-423,-419,-415,-412,-408,-404,-400
	dc.w -396,-392,-388,-384,-380,-376,-371,-367,-363,-358
	dc.w -354,-349,-344,-340,-335,-330,-325,-320,-316,-311
	dc.w -306,-300,-295,-290,-285,-280,-274,-269,-264,-258
	dc.w -253,-247,-242,-236,-231,-225,-219,-214,-208,-202
	dc.w -196,-191,-185,-179,-173,-167,-161,-155,-149,-143
	dc.w -137,-131,-125,-119,-113,-107,-100,-94,-88,-82
	dc.w -76,-69,-63,-57,-51,-44,-38,-32,-26,-19
	dc.w -13,-7     

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
