
; update oct 2017 by Leonard/Oxygene
;  - remove second CIA interrupt (now replen is set next frame if needed)
;  - support smaples in chip mem and music score in other ram
;  - various minor optimization


	bra	pr_init
	bra	pr_end
	bra	pr_music


musicGetInfo:
	move.w	pr_currentpattern(pc),d0
	move.w	pr_patternct(pc),d1
	rts
	
;*****************************************
;*					*
;* prorunner v1.0			*
;* --------------			*
;* coded by cosmos of sanity in 1992	*
;*					*
;*****************************************
;*					*
;* supporting the following effects:	*
;*					*
;*	- fade sound in/out		*
;*	- finetune			*
;*	- normal play or arpeggio	*
;*	- slide frequenz up		*
;*	- slide	frequenz down		*
;*	- tone portamento		*
;*	- vibrato			*
;*	- tone portamento+volume slide	*
;*	- vibrato + volume slide	*
;*	- tremolo			*
;*	- set sampleoffset		*
;*	- volume slide			*
;*	- position jump			*
;*	- set volume			*
;*	- pattern break			*
;*	- set speed			*
;* - e-commands:				*
;*	- set filter			*
;*	- fine slide up			*
;*	- fine slide down		*
;*	- glissando control		*
;*	- set vibrato waveform		*
;*	- set finetune			*
;*	- set loop / jump to loop	*
;*	- set tremolo waveform		*
;*	- retrig note			*
;*	- fine volumeslide up		*
;*	- fine volumeslide down		*
;*	- notecut			*
;*	- notedelay			*
;*	- patterndelay			*
;*	- funkrepeat			*
;*					*
;*****************************************

yes				=	1
no				=	0


samplelengthoffset		=	4
samplevolumeoffset		=	6
samplerepeatpointoffset		=	8
samplewithloop			=	12
samplerepeatlengthoffset	=	14
samplefinetuneoffset		=	16

;* init-routine *******************************************************

; A0: music score
; A1: sample bank data (chip mem)
; d0: music score size
pr_init:
	lea	pr_framecounter(pc),a6
	move.l	d0,-(a7)
	pea		(a1)					; sample bank
pr_init1:
	cmp.l	#'M.K.',1080(a0)
	beq.s	pr_init2
	cmp.l	#'SNT.',1080(a0)
	beq.s	pr_init2
	rts
pr_init2:
	lea	20(a0),a1
	lea	pr_sampleinfos(pc),a2
	moveq.l	#32,d7
	moveq	#30,d0
pr_init3:
	lea	22(a1),a1		; samplenamen überspringen
	move.w	(a1)+,samplelengthoffset(a2)	; samplelength in words
	lea	pr_periods(pc),a3
	move.b	(a1)+,d2		; finetuning
	and.w	#$f,d2
	mulu.w	#36*2,d2
	add.l	d2,a3
	move.l	a3,samplefinetuneoffset(a2)
	moveq	#0,d1
	move.b	(a1)+,d1
	move.w	d1,samplevolumeoffset(a2)	; volume
	moveq.l	#0,d1
	move.w	(a1)+,d1		; repeatpoint in bytes
	add.l	d1,d1
	move.l	d1,samplerepeatpointoffset(a2)
	move.w	(a1)+,d1
	clr.w	samplewithloop(a2)
	cmp.w	#1,d1
	bls.s	pr_init3_2
	addq.w	#1,samplewithloop(a2)
pr_init3_2:
	move.w	d1,samplerepeatlengthoffset(a2)	; repeatlength
	add.l	d7,a2
	dbf	d0,pr_init3

	moveq	#0,d0
	move.b	950(a0),d0		; number of patterns
	subq.w	#1,d0
	move.w	d0,pr_highestpattern-pr_framecounter(a6)
	moveq.l	#0,d1
	lea	952(a0),a1		; 1. patternpos
	lea	1084(a0),a2		; 1. patterndata
	lea	pr_patternpositions(pc),a3
pr_init4:
	move.b	(a1)+,d2		; x. patternpos
	moveq.l	#0,d3
	move.b	d2,d3
	lsl.l	#8,d3
	lsl.l	#2,d3
	add.l	a2,d3
	move.l	d3,(a3)+
	dbf	d0,pr_init4
	
		move.l	(a7)+,d1					; sample bank ad
		lea		pr_sampleinfos(pc),a2
		moveq	#31-1,d0
.iloop:	move.l	d1,(a2)						; sample ad
		add.l	d1,samplerepeatpointoffset(a2)
		moveq	#0,d2
		move.w	samplelengthoffset(a2),d2
		add.l	d2,d2
		add.l	d2,d1						; next sample ad
		lea		32(a2),a2
		dbf		d0,.iloop

	
	cmp.l	#'SNT.',1080(a0)
	beq.w	pr_init7
	
	lea	1084(a0),a1
;	move.l	pr_sampleinfos(pc),a2		; music score end
	movea.l	a0,a2
	add.l	(a7)+,a2					; end music score ad
	move.b	#$f0,d6
	move.w	#$fff,d7
pr_init5:
	move.b	(a1),d0
	move.b	2(a1),d1
	move.w	(a1),d2
	and.w	d7,2(a1)
	and.w	d7,d2
	
	and.b	d6,d0
	lsr.b	#4,d1
	or.b	d1,d0
	move.b	d0,(a1)
	
	tst.w	d2
	beq.s	pr_init5_3
	lea	pr_periods(pc),a4
	moveq	#0,d1
pr_init5_2:
	addq.w	#1,d1
	cmp.w	(a4)+,d2
	bne.s	pr_init5_2
	move.b	d1,1(a1)
pr_init5_3:
	addq.l	#4,a1
	cmp.l	a2,a1
	blt.s	pr_init5	

	move.l	#'SNT.',1080(a0)
pr_init7:
	lea	pr_arpeggiofastlist(pc),a2
	lea	pr_arpeggiofastlistperiods(pc),a1
	lea	35*2(a1),a1		; to the end of list...
	moveq	#0,d0
	moveq	#35,d1
	move.w	#999,d2
	moveq	#0,d6
pr_init8:
	move.w	-(a1),d7
	addq.w	#1,d6
pr_init8_2:
	cmp.w	d7,d0
	blt.s	pr_init8_4
	subq.w	#1,d1
	tst.b	d1
	bne.s	pr_init8
pr_init8_3:
	move.b	d1,(a2)+
	dbf	d2,pr_init8_3
	bra.s	pr_init8_5	
pr_init8_4:
	move.b	d1,(a2)+
	addq.w	#1,d0
	dbf	d2,pr_init8_2
pr_init8_5:

	lea	pr_channel0(pc),a1
	move.w	#1,(a1)
	move.w	#1,pr_channel1-pr_channel0(a1)
	move.w	#1,pr_channel2-pr_channel0(a1)
	move.w	#1,pr_channel3-pr_channel0(a1)
	addq.l	#2,a1
	moveq	#(pr_channel1-pr_channel0)/2-2,d0
pr_init9_2:
	clr.w	(a1)
	clr.w	pr_channel1-pr_channel0(a1)
	clr.w	pr_channel2-pr_channel0(a1)
	clr.w	pr_channel3-pr_channel0(a1)
	addq.l	#2,a1
	dbf	d0,pr_init9_2

	lea	pr_fastperiodlist(pc),a1
	lea	pr_periods(pc),a2
	move.l	a2,(a1)
	moveq.l	#36*2,d1
	moveq	#14,d0
pr_init9_3:
	move.l	(a1)+,d2
	add.l	d1,d2
	move.l	d2,(a1)
	dbf	d0,pr_init9_3
		
	lea	pr_arpeggiofastdivisionlist(pc),a1
	moveq	#0,d1
	move.w	#$ff,d0
pr_init9_4:
	move.b	d1,(a1)+
	subq.b	#1,d1
	btst	#7,d1
	beq.s	pr_init9_4_2
	moveq	#2,d1
pr_init9_4_2:
	dbf	d0,pr_init9_4
	
	move.w	#6,pr_speed-pr_framecounter(a6)
	move.w	pr_speed(pc),(a6)
	clr.w	pr_patternct-pr_framecounter(a6)
	move.w	pr_highestpattern(pc),d0
	move.w	pr_startposition(pc),d1
	blt.s	pr_init9_5
	cmp.w	d0,d1
	bls.s	pr_init9_5_2
pr_init9_5:
	clr.w	pr_startposition-pr_framecounter(a6)
pr_init9_5_2:
	move.w	pr_startposition(pc),pr_currentpattern-pr_framecounter(a6)
	
	lea	pr_patternpositions(pc),a3
	move.l	a3,d0
	moveq.l	#0,d1
	move.w	pr_startposition(pc),d1
	lsl.l	#2,d1
	add.l	d1,d0
	move.l	d0,pr_patternpt-pr_framecounter(a6)
	move.l	pr_patternpt(pc),a5
	move.l	(a5),pr_currentposition-pr_framecounter(a6)
	
	lea	$dff000,a5
	lea	$bfd000,a0
	move.w	#$2000,$9c(a5)
	move.l	$78.w,pr_old78-pr_framecounter(a6)
	lea	pr_int(pc),a3
	move.l	a3,$78.w
	move.b	#$7f,$d00(a0)
	move.b	#$08,$e00(a0)
	move.b	#$80,$400(a0)
	move.b	#$01,$500(a0)
pr_init10:
	btst	#0,$bfdd00
	beq.s	pr_init10
	move.b	#$81,$d00(a0)
	move.w	#$a000,$9a(a5)
	move.w	#$f,$96(a5)
	clr.w	$a8(a5)
	clr.w	$b8(a5)
	clr.w	$c8(a5)
	clr.w	$d8(a5)
	bset	#1,$bfe001
	rts

;* end-routine *********************************************************

pr_end:
	lea	$dff000,a5
	move.w	#$f,$96(a5)
	clr.w	$a8(a5)
	clr.w	$b8(a5)
	clr.w	$c8(a5)
	clr.w	$d8(a5)
	move.w	#$2000,$9a(a5)
;	move.l	pr_old78(pc),$78.w
	bclr	#1,$bfe001
	rts

;* music-fading ********************************************************

	
;* macros **************************************************************


pr_checkchannel	macro
{
		bsr		pr_checkfunkrepeat
		moveq	#0,d0
		move.b	4(a4),d0
		add.w	d0,d0
		lea		pr_effectchecklist(pc),a0
		add.w	0(a0,d0.w),a0
		jsr		(a0)
}

pr_playchannel macro
{
		moveq	#0,d0
		move.b	2(a6),d0
		add.w	d0,d0
		lea		pr_playchannellist(pc),a0
		add.w	0(a0,d0.w),a0					; 5
		jsr	(a0)								; 4
		move.w	2(a4),6(a3)
		move.w	12(a4),8(a3)		
}
		
;* music-routine *******************************************************

pr_music:
	lea	$dff000,a5
	
		move.w	dmaJustActived(pc),d0
		beq.s	.noD

		lea	pr_channel0+6(pc),a6
		btst	#0,d0
		beq.s	.noA
		move.l	(a6),$a0(a5)
		move.w	4(a6),$a4(a5)

.noA:	btst	#1,d0
		beq.s	.noB
		move.l	pr_channel1-pr_channel0(a6),$b0(a5)
		move.w	4+pr_channel1-pr_channel0(a6),$b4(a5)

.noB:	btst	#2,d0
		beq.s	.noC
		move.l	pr_channel2-pr_channel0(a6),$c0(a5)
		move.w	4+pr_channel2-pr_channel0(a6),$c4(a5)

.noC:	btst	#3,d0
		beq.s	.noD
		move.l	pr_channel3-pr_channel0(a6),$d0(a5)
		move.w	4+pr_channel3-pr_channel0(a6),$d4(a5)
.noD:	
	
		lea		dmaJustActived(pc),a2
		clr.w	(a2)	
	
	lea	pr_framecounter(pc),a2
	subq.w	#1,(a2)
	beq.s	pr_music2
	bsr	pr_checkeffects
	rts
pr_music2:
	cmp.b	#1,pr_patterndelaytime-pr_framecounter+1(a2)
	blt.s	pr_music2_2
	bsr	pr_checkeffects
	bra.w	pr_music2_9
pr_music2_2:

	move.l	pr_currentposition(pc),a6
	lea	pr_channel0(pc),a4
	lea	$a0(a5),a3
	moveq	#1,d7
	pr_playchannel
	
	addq.l	#4,a6
	lea	pr_channel1(pc),a4
	lea	$b0(a5),a3
	moveq	#2,d7
	pr_playchannel

	addq.l	#4,a6
	lea	pr_channel2(pc),a4
	lea	$c0(a5),a3
	moveq	#4,d7
	pr_playchannel

	addq.l	#4,a6
	lea	pr_channel3(pc),a4
	lea	$d0(a5),a3
	moveq	#8,d7
	pr_playchannel
	
	move.b	#$19,$bfde00

pr_music2_9:
	move.w	pr_speed(pc),(a2)
	tst.w	pr_patternhasbeenbreaked-pr_framecounter(a2)
	bne.s	pr_music3
	tst.w	pr_patterndelaytime-pr_framecounter(a2)
	beq.s	pr_music3_1
	subq.w	#1,pr_patterndelaytime-pr_framecounter(a2)
	beq.s	pr_music3_1
	bra.s	pr_nonextpattern
pr_music3:
	clr.w	pr_patternhasbeenbreaked-pr_framecounter(a2)
	tst.w	pr_patterndelaytime-pr_framecounter(a2)
	beq.s	pr_music3_1
	subq.w	#1,pr_patterndelaytime-pr_framecounter(a2)
pr_music3_1:
	moveq.l	#16,d0
	add.l	d0,pr_currentposition-pr_framecounter(a2)
	lea	pr_patternct(pc),a1
	addq.w	#1,(a1)
	moveq	#64,d1
	cmp.w	(a1),d1
	bgt.s	pr_nonextpattern
	sub.w	d1,(a1)
	bne		mtrap
	moveq.l	#0,d1
	move.w	(a1),d1
	lsl.w	#4,d1
	addq.l	#4,pr_patternpt-pr_framecounter(a2)
	lea	pr_currentpattern(pc),a0
	addq.w	#1,(a0)
	move.w	(a0),d0
	cmp.w	pr_highestpattern-pr_framecounter(a2),d0
	bls.s	pr_nohighestpattern
	lea	pr_patternpositions(pc),a1
	move.l	a1,pr_patternpt-pr_framecounter(a2)
	clr.w	(a0)
pr_nohighestpattern:
	move.l	pr_patternpt-pr_framecounter(a2),a6
	move.l	(a6),d0
	add.l	d1,d0
	move.l	d0,pr_currentposition-pr_framecounter(a2)
pr_nonextpattern:
	rts

mtrap:	lea	.txt(pc),a0
		trap	#0
.txt:	dc.b	'Player!',0
		even
		
pr_int:
	tst.b	$bfdd00
	move.w	pr_dmacon(pc),$dff096
	move.w	#$2000,$dff09c
	rte

pr_playchannellist:
	dc.w	pr_playnormalchannel-pr_playchannellist		; 0
	dc.w	pr_playnormalchannel-pr_playchannellist		; 1
	dc.w	pr_playnormalchannel-pr_playchannellist		; 2
	dc.w	pr_playtpchannel-pr_playchannellist		; 3
	dc.w	pr_playnormalchannel-pr_playchannellist		; 4
	dc.w	pr_playtpchannel-pr_playchannellist		; 5
	dc.w	pr_playnormalchannel-pr_playchannellist		; 6
	dc.w	pr_playnormalchannel-pr_playchannellist		; 7
	dc.w	pr_playnormalchannel-pr_playchannellist		; 8
	dc.w	pr_playsochannel-pr_playchannellist		; 9
	dc.w	pr_playnormalchannel-pr_playchannellist		; a
	dc.w	pr_playnormalchannel-pr_playchannellist		; b
	dc.w	pr_playnormalchannel-pr_playchannellist		; c
	dc.w	pr_playnormalchannel-pr_playchannellist		; d
	dc.w	pr_playnormalchannel-pr_playchannellist		; e
	dc.w	pr_playnormalchannel-pr_playchannellist		; f
	
;* kanal normal spielen ************************************************

pr_playnormalchannel:
	lea	pr_sampleinfos(pc),a0
	move.l	a0,a1
	lea	samplefinetuneoffset(a1),a1
	moveq	#0,d1
	move.b	1(a6),d1
	moveq	#0,d0
	move.b	(a6),d0
	beq.s	pr_playnormalnonewsample	; irgendein sample ?
	move.w	d0,(a4)				; trage samplenummer ein
	tst.b	d1
	bne.s	pr_playnormalsample
	subq.b	#1,d0
	lsl.l	#5,d0
	add.l	d0,a0
	addq.l	#6,a0
	move.w	(a0)+,12(a4)
	move.l	(a0)+,d2
	move.l	d2,6(a4)
	tst.w	(a0)+
	beq.s	pr_playnormalchannel2
	move.l	d2,36(a4)
	move.l	d2,40(a4)
pr_playnormalchannel2:
	move.w	(a0)+,10(a4)
	bra.w	pr_playnormalnonewperiod
pr_playnormalsample:
	move.w	d7,$96(a5)
	or.w	d7,pr_dmacon-pr_framecounter(a2)
	or.w	d7,dmaJustActived-pr_framecounter(a2)
	subq.b	#1,d0
	lsl.l	#5,d0
	add.l	d0,a0
	move.l	(a0)+,(a3)		; setze samplestart
	move.w	(a0)+,4(a3)		; setze audiodatenlänge
	move.w	(a0)+,12(a4)		; setze samplelautstärke
	move.l	(a0)+,d2
	move.l	d2,6(a4)		; samplerepeatpoint eintragen
	tst.w	(a0)+
	beq.s	pr_playnormalsample2
	move.l	d2,36(a4)
	move.l	d2,40(a4)
pr_playnormalsample2:
	move.w	(a0)+,10(a4)		; samplerepeatlength eintragen
	bra.s	pr_playnormalnewperiod
pr_playnormalnonewsample:
	clr.l	14(a4)
	tst.b	d1
	beq.s	pr_playnormalnonewperiod	; irgend ne neue frequenz ?
	move.w	(a4),d0			; alte samplenummer holen
	move.w	d7,$96(a5)
	or.w	d7,pr_dmacon-pr_framecounter(a2)
	or.w	d7,dmaJustActived-pr_framecounter(a2)
	subq.b	#1,d0
	lsl.l	#5,d0
	add.l	d0,a0
	move.l	(a0)+,(a3)		; setze samplestart
	move.w	(a0)+,4(a3)		; setze audiodatenlänge
	addq.l	#2,a0
	move.l	(a0)+,d2
	move.l	d2,6(a4)		; samplerepeatpoint eintragen
	tst.w	(a0)+
	beq.s	pr_playnormalnonewsample2
	move.l	d2,36(a4)
	move.l	d2,40(a4)
pr_playnormalnonewsample2:
	move.w	(a0)+,10(a4)		; samplerepeatlength eintragen
pr_playnormalnewperiod:
	subq.b	#1,d1
	add.b	d1,d1
	move.w	(a4),d0
	subq.b	#1,d0
	lsl.w	#5,d0
	move.l	(a1,d0.w),a1
	move.w	(a1,d1.w),2(a4)		; frequenz eintragen
pr_playnormalnonewperiod:
	move.w	2(a6),4(a4)
	bra.w	pr_playeffect

;* kanal mit offset spielen *********************************************

pr_playsochannel:
	lea	pr_sampleinfos(pc),a0
	move.l	a0,a1
	lea	samplefinetuneoffset(a1),a1
	moveq	#0,d1
	move.b	1(a6),d1
	moveq	#0,d0
	move.b	(a6),d0
	beq.w	pr_playsononewsample	; irgendein sample ?
	move.w	d0,(a4)				; trage samplenummer ein
	tst.b	d1
	bne.s	pr_playsosample
	subq.b	#1,d0
	lsl.l	#5,d0
	add.l	d0,a0
	addq.l	#6,a0
	move.w	(a0)+,12(a4)
	move.l	(a0)+,d2
	move.l	d2,6(a4)
	tst.w	(a0)+
	beq.s	pr_playsochannel2
	move.l	d2,36(a4)
	move.l	d2,40(a4)
pr_playsochannel2:
	move.w	(a0)+,10(a4)
	bra.w	pr_playsononewperiod
pr_playsosample:
	move.w	d7,$96(a5)
	or.w	d7,pr_dmacon-pr_framecounter(a2)
	or.w	d7,dmaJustActived-pr_framecounter(a2)
	moveq.l	#0,d6
	move.b	3(a6),d6
	lsl.w	#7,d6
	subq.b	#1,d0
	lsl.l	#5,d0
	add.l	d0,a0
	move.l	(a0)+,d2
	move.w	(a0)+,d3
	cmp.w	d3,d6
	bge.s	pr_playsosample2
	sub.w	d6,d3
	add.l	d6,d6
	add.l	d6,d2
	move.l	d2,(a3)			; setze samplestart
	move.w	d3,4(a3)		; setze audiodatenlänge
	move.w	(a0)+,12(a4)		; setze samplelautstärke
	move.l	(a0)+,d2
	move.l	d2,6(a4)		; samplerepeatpoint eintragen
	tst.w	(a0)+
	beq.s	pr_playsosample1
	move.l	d2,36(a4)
	move.l	d2,40(a4)
pr_playsosample1:
	move.w	(a0)+,10(a4)		; samplerepeatlength eintragen
	bra.w	pr_playsonewperiod
pr_playsosample2:
	move.w	(a0)+,12(a4)
	move.l	(a0),(a3)
	move.w	4(a0),4(a3)
	move.l	(a0)+,d2
	move.l	d2,6(a4)
	tst.w	(a0)+
	beq.s	pr_playsosample4
	move.l	d2,36(a4)
	move.l	d2,40(a4)
pr_playsosample4:
	move.w	(a0)+,10(a4)
	bra.s	pr_playsonewperiod
pr_playsononewsample:
	clr.l	14(a4)
	tst.b	d1
	beq.w	pr_playsononewperiod	; irgend ne neue frequenz ?
	move.w	(a4),d0			; alte samplenummer holen
	move.w	d7,$96(a5)
	or.w	d7,pr_dmacon-pr_framecounter(a2)
	or.w	d7,dmaJustActived-pr_framecounter(a2)
	moveq.l	#0,d6
	move.b	3(a6),d6
	lsl.w	#7,d6
	subq.b	#1,d0
	lsl.l	#5,d0
	add.l	d0,a0
	move.l	(a0)+,d2
	move.w	(a0)+,d3
	cmp.w	d3,d6
	bge.s	pr_playsosample3
	sub.w	d6,d3
	add.l	d6,d6
	add.l	d6,d2
	move.l	d2,(a3)			; setze samplestart
	move.w	d3,4(a3)		; setze audiodatenlänge
	addq.l	#2,a0
	move.l	(a0)+,d2
	move.l	d2,6(a4)		; samplerepeatpoint eintragen
	tst.w	(a0)+
	beq.s	pr_playsononewsample2
	move.l	d2,36(a4)
	move.l	d2,40(a4)
pr_playsononewsample2:
	move.w	(a0)+,10(a4)		; samplerepeatlength eintragen
	bra.s	pr_playsonewperiod
pr_playsosample3:
	addq.l	#2,a0
	move.l	(a0),(a3)
	move.w	4(a0),4(a3)
	move.l	(a0)+,d2
	move.l	d2,6(a4)
	tst.w	(a0)+
	beq.s	pr_playsosample5
	move.l	d2,36(a4)
	move.l	d2,40(a4)
pr_playsosample5:
	move.w	(a0)+,10(a4)
	bra.w	pr_playsonewperiod
pr_playsonewperiod:
	subq.w	#1,d1
	add.b	d1,d1
	move.w	(a4),d0
	subq.b	#1,d0
	lsl.w	#5,d0
	move.l	(a1,d0.w),a1
	move.w	(a1,d1.w),2(a4)		; frequenz eintragen
pr_playsononewperiod:
	move.w	2(a6),4(a4)
	bra.w	pr_playeffect

;* kanal spielen mit tone portamento **********************************

pr_playtpchannel:
	lea	pr_sampleinfos(pc),a0
	move.l	a0,a1
	lea	samplefinetuneoffset(a1),a1
	moveq	#0,d1
	move.b	1(a6),d1
	moveq	#0,d0
	move.b	(a6),d0
	beq.s	pr_playtpnonewsample	; irgendein sample ?
	move.w	d0,(a4)			; trage samplenummer ein
	subq.b	#1,d0
	lsl.l	#5,d0
	add.l	d0,a0
	addq.l	#6,a0
	move.w	(a0)+,12(a4)		; lautstärke eintragen
	move.l	(a0)+,d2
	move.l	d2,6(a4)		; repeatpoint eintragen
	tst.w	(a0)+
	beq.s	pr_playtpchannel2
	move.l	d2,36(a4)
	move.l	d2,40(a4)
pr_playtpchannel2:
	move.w	(a0)+,10(a4)		; repeatlength eintragen
pr_playtpnonewsample:
	tst.b	d1
	beq.s	pr_playtpnonewperiod	; irgend ne neue frequenz ?
pr_playtpnewperiod:
	move.w	2(a4),14(a4)
	subq.w	#1,d1
	add.b	d1,d1
	move.w	(a4),d0
	subq.b	#1,d0
	lsl.w	#5,d0
	move.l	(a1,d0.w),a1
	move.w	(a1,d1.w),d2
	move.w	d2,16(a4)		; frequenz eintragen
	bra.s	pr_playtpallowed
pr_playtpnonewperiod:
	tst.w	16(a4)
	bne.s	pr_playtpallowed
	clr.w	14(a4)
	clr.l	26(a4)
pr_playtpallowed:
	move.w	2(a6),4(a4)
	bra.w	pr_playeffect

pr_playeffect:
	bsr	pr_checkfunkrepeat
	moveq	#0,d0
	move.b	2(a6),d0
	add.w	d0,d0
	move.w	pr_normaleffectlist(pc,d0.w),d0
	jmp		pr_normaleffectlist(pc,d0.w)
pr_playnoeffect:
	rts

pr_normaleffectlist:
	dc.w	pr_playnoeffect-pr_normaleffectlist		; 0
	dc.w	pr_playnoeffect-pr_normaleffectlist		; 1
	dc.w	pr_playnoeffect-pr_normaleffectlist		; 2
	dc.w	pr_preptoneportamento-pr_normaleffectlist	; 3
	dc.w	pr_prepvibrato-pr_normaleffectlist		; 4
	dc.w	pr_playnoeffect-pr_normaleffectlist		; 5
	dc.w	pr_prepvibandvolslide-pr_normaleffectlist	; 6
	dc.w	pr_preptremolo-pr_normaleffectlist		; 7
	dc.w	pr_playnoeffect-pr_normaleffectlist		; 8
	dc.w	pr_playnoeffect-pr_normaleffectlist		; 9
	dc.w	pr_playnoeffect-pr_normaleffectlist		; a
	dc.w	pr_jumptopattern-pr_normaleffectlist		; b
	dc.w	pr_newvolume-pr_normaleffectlist		; c
	dc.w	pr_patternbreak-pr_normaleffectlist		; d
	dc.w	pr_play_e_command-pr_normaleffectlist		; e
	dc.w	pr_newspeed-pr_normaleffectlist			; f

pr_play_e_command:
	moveq	#0,d0
	move.b	3(a6),d0
	lsr.b	#4,d0
	add.w	d0,d0
	move.w	pr_e_commandeffectlist(pc,d0.w),d0
	jmp		pr_e_commandeffectlist(pc,d0.w)
	
pr_e_commandeffectlist:
	dc.w	pr_setfilter-pr_e_commandeffectlist		; 0
	dc.w	pr_fineslideup-pr_e_commandeffectlist		; 1
	dc.w	pr_fineslidedown-pr_e_commandeffectlist		; 2
	dc.w	pr_setglissandocontrol-pr_e_commandeffectlist	; 3
	dc.w	pr_setvibratowaveform-pr_e_commandeffectlist	; 4
	dc.w	pr_playfinetune-pr_e_commandeffectlist		; 5
	dc.w	pr_jumptoloop-pr_e_commandeffectlist		; 6
	dc.w	pr_settremolowaveform-pr_e_commandeffectlist	; 7
	dc.w	pr_playnoeffect-pr_e_commandeffectlist		; 8
	dc.w	pr_prepretrignote-pr_e_commandeffectlist	; 9
	dc.w	pr_finevolumeslideup-pr_e_commandeffectlist	; a
	dc.w	pr_finevolumeslidedown-pr_e_commandeffectlist	; b
	dc.w	pr_prepnotecut-pr_e_commandeffectlist		; c
	dc.w	pr_prepnotedelay-pr_e_commandeffectlist		; d
	dc.w	pr_preppatterndelay-pr_e_commandeffectlist	; e
	dc.w	pr_prepfunkrepeat-pr_e_commandeffectlist	; f

pr_preppatterndelay:
	cmp.b	#1,pr_patterndelaytime-pr_framecounter+1(a2)
	bge.s	pr_preppatterndelayend
	move.b	5(a4),d0
	and.b	#$f,d0
	addq.b	#1,d0
	move.b	d0,pr_patterndelaytime-pr_framecounter+1(a2)
pr_preppatterndelayend:
	rts

pr_setvibratowaveform:
	move.b	5(a4),d0
	moveq	#$f,d1
	and.w	d1,d0
	move.w	d0,50(a4)
	rts

pr_settremolowaveform:
	move.b	5(a4),d0
	moveq	#$f,d1
	and.w	d1,d0
	move.w	d0,52(a4)
	rts

pr_setglissandocontrol:
	move.b	5(a4),d0
	moveq	#$f,d1
	and.w	d1,d0
	move.w	d0,48(a4)
	rts

pr_playfinetune:
	move.b	5(a4),d0
	moveq	#$f,d1
	and.w	d1,d0
	lsl.w	#2,d0
	lea	pr_fastperiodlist(pc),a0
	move.l	(a0,d0.w),a0
	moveq	#0,d1
	move.b	1(a6),d1
	beq.s	pr_playfinetuneend
	subq.b	#1,d1
	add.w	d1,d1
	move.w	(a0,d1.w),2(a4)		; frequenz eintragen
pr_playfinetuneend:
	rts
	
pr_jumptoloop:
	move.b	5(a4),d0
	moveq	#$f,d1
	and.b	d1,d0
	beq.s	pr_prepjumptoloop
	addq.b	#1,47(a4)
	cmp.b	47(a4),d0
	blt.s	pr_jumptoloopend
	moveq.l	#0,d0
	move.w	44(a4),d0
	subq.b	#1,d0
	move.w	d0,pr_patternct-pr_framecounter(a2)
	move.l	pr_patternpt(pc),a0
	move.l	(a0),d5
	lsl.l	#4,d0
	add.l	d0,d5
	move.l	d5,pr_currentposition-pr_framecounter(a2)
	rts
pr_jumptoloopend:
	clr.w	46(a4)
	rts
pr_prepjumptoloop:
	tst.w	46(a4)
	bne.s	pr_prepjumptoloopend
	move.w	pr_patternct-pr_framecounter(a2),44(a4)
	clr.w	46(a4)
pr_prepjumptoloopend:
	rts

pr_prepnotedelay:
	tst.w	(a6)
	beq.s	pr_prepnotedelayend
	move.b	5(a4),d0
	and.b	#$f,d0
	beq.s	pr_prepnotedelayend
	move.w	d7,d0
	not.w	d0
	and.w	d0,pr_dmacon-pr_framecounter(a2)
	clr.w	18(a4)
	rts
pr_prepnotedelayend:
	move.w	#$fff,18(a4)
	rts

pr_prepretrignote:
	clr.w	18(a4)
	tst.w	(a6)
	bne.s	pr_prepretrignoteend
	bsr	pr_checkretrignote2	
pr_prepretrignoteend:
	rts

pr_prepnotecut:
	clr.w	18(a4)
	move.b	5(a4),d0
	moveq	#$f,d1
	and.b	d1,d0
	tst.b	d0
	bne.s	pr_prepnotecutend
	clr.w	12(a4)
pr_prepnotecutend:
	rts
	
pr_finevolumeslideup:
	move.b	3(a6),d0
	moveq	#$f,d1
	and.w	d1,d0
	move.w	12(a4),d1
	add.w	d0,d1
	moveq	#64,d0
	cmp.w	d0,d1
	bls.s	pr_finevolumeslideup2
	move.w	d0,d1
pr_finevolumeslideup2:
	move.w	d1,12(a4)
	rts

pr_finevolumeslidedown:
	move.b	3(a6),d0
	moveq	#$f,d1
	and.w	d1,d0
	move.w	12(a4),d1
	sub.w	d0,d1
	btst	#15,d1
	beq.s	pr_finevolumeslidedown2
	moveq	#0,d1
pr_finevolumeslidedown2:
	move.w	d1,12(a4)
	rts

pr_fineslideup:
	move.b	3(a6),d0
	moveq	#$f,d1
	and.w	d1,d0
	move.w	2(a4),d1
	sub.w	d0,d1
	cmp.w	#108,d1
	bge.s	pr_fineslideup2
	move.w	#108,d1
pr_fineslideup2:
	move.w	d1,2(a4)
	rts

pr_fineslidedown:
	move.b	3(a6),d0
	moveq	#$f,d1
	and.w	d1,d0
	move.w	2(a4),d1
	add.w	d0,d1
	cmp.w	#907,d1
	bls.s	pr_fineslidedown2
	move.w	#907,d1
pr_fineslidedown2:
	move.w	d1,2(a4)
	rts

pr_setfilter:
	btst	#0,3(a6)
	beq.s	pr_setfilteron
pr_setfilteroff:
	bset	#1,$bfe001
	rts
pr_setfilteron:
	bclr	#1,$bfe001
	rts

pr_prepvibandvolslide:
	cmp.b	#1,pr_speed-pr_framecounter+1(a2)
	beq.s	pr_prepvibandvolslide2
	tst.b	1(a6)
	beq.s	pr_prepvibandvolslide2
	clr.w	18(a4)
pr_prepvibandvolslide2:
	rts

pr_preptoneportamento:
	tst.b	3(a6)
	beq.s	pr_preptoneportamento2
	move.w	2(a6),22(a4)
pr_preptoneportamento2:
	rts

pr_prepvibrato:
	cmp.b	#1,pr_speed-pr_framecounter+1(a2)
	beq.s	pr_prepvibrato2
	tst.b	1(a6)
	beq.s	pr_prepvibrato0
	clr.w	18(a4)
pr_prepvibrato0:
	move.b	5(a4),d0
	move.b	d0,d1
	lsr.b	#4,d1
	beq.s	pr_prepvibrato1
	move.b	d1,24(a4)
pr_prepvibrato1:
	and.b	#$f,d0
	beq.s	pr_prepvibrato2
	move.b	d0,25(a4)
pr_prepvibrato2:
	rts

pr_preptremolo:
	cmp.b	#1,pr_speed-pr_framecounter+1(a2)
	beq.s	pr_preptremolo2
	tst.b	1(a6)
	beq.s	pr_preptremolo0
	clr.w	18(a4)
pr_preptremolo0:
	move.w	12(a4),20(a4)
	move.b	5(a4),d0
	move.b	d0,d1
	lsr.b	#4,d1
	beq.s	pr_preptremolo1
	move.b	d1,30(a4)
pr_preptremolo1:
	and.b	#$f,d0
	beq.s	pr_preptremolo2
	move.b	d0,31(a4)
pr_preptremolo2:
	rts

pr_newvolume:
	move.b	3(a6),d0
	cmp.b	#64,d0
	bls.s	pr_newvolumeend
	moveq	#64,d0
pr_newvolumeend:
	move.b	d0,13(a4)
	rts

pr_newspeed:
	move.b	3(a6),d0
	tst.b	d0
	bne.s	pr_newspeed2
	moveq	#1,d0
pr_newspeed2:
	move.b	d0,pr_speed-pr_framecounter+1(a2)
	rts

pr_patternbreak:
	moveq	#0,d0
	move.b	3(a6),d0
	add.w	#63,d0
	move.w	d0,pr_patternct-pr_framecounter(a2)
	addq.w	#1,pr_patternhasbeenbreaked-pr_framecounter(a2)
	rts
		
pr_jumptopattern:
	moveq.l	#0,d0
	move.b	3(a6),d0
	subq.b	#1,d0
	btst	#7,d0
	beq.s	pr_playjumptopattern2
	move.w	#128,d0
pr_playjumptopattern2:
	move.b	d0,pr_currentpattern-pr_framecounter+1(a2)
	lsl.l	#2,d0
	lea	pr_patternpositions(pc),a0
	add.l	a0,d0
	move.l	d0,pr_patternpt-pr_framecounter(a2)
	move.w	#63,pr_patternct-pr_framecounter(a2)
	addq.w	#1,pr_patternhasbeenbreaked-pr_framecounter(a2)
	rts

;* control fx every frame **********************************************

pr_checkeffects:
	moveq	#1,d7
	lea	$a0(a5),a3
	lea	pr_channel0(pc),a4
	move.w	12(a4),54(a4)
	pr_checkchannel
	move.w	54(a4),8(a3)
	
	moveq	#2,d7
	lea	$b0(a5),a3
	lea	pr_channel1(pc),a4
	move.w	12(a4),54(a4)
	pr_checkchannel
	move.w	54(a4),8(a3)

	moveq	#4,d7
	lea	$c0(a5),a3
	lea	pr_channel2(pc),a4
	move.w	12(a4),54(a4)
	pr_checkchannel
	move.w	54(a4),8(a3)

	moveq	#8,d7
	lea	$d0(a5),a3
	lea	pr_channel3(pc),a4
	move.w	12(a4),54(a4)
	pr_checkchannel
	move.w	54(a4),8(a3)

	move.b	#$19,$bfde00
	rts

;***********************************************************************

pr_checknotchannel:
	rts

pr_check_e_commands
	moveq	#0,d0
	move.b	5(a4),d0
	lsr.b	#4,d0
	add.w	d0,d0
	move.w	pr_e_command_checklist(pc,d0.w),d0	; 4
	jmp		pr_e_command_checklist(pc,d0.w)		; 4
	
	
pr_e_command_checklist:
	dc.w	pr_checknotchannel-pr_e_command_checklist	; 0
	dc.w	pr_checknotchannel-pr_e_command_checklist	; 1
	dc.w	pr_checknotchannel-pr_e_command_checklist	; 2
	dc.w	pr_checknotchannel-pr_e_command_checklist	; 3
	dc.w	pr_checknotchannel-pr_e_command_checklist	; 4
	dc.w	pr_checknotchannel-pr_e_command_checklist	; 5
	dc.w	pr_checknotchannel-pr_e_command_checklist	; 6
	dc.w	pr_checknotchannel-pr_e_command_checklist	; 7
	dc.w	pr_checknotchannel-pr_e_command_checklist	; 8
	dc.w	pr_checkretrignote-pr_e_command_checklist	; 9
	dc.w	pr_checknotchannel-pr_e_command_checklist	; a
	dc.w	pr_checknotchannel-pr_e_command_checklist	; b
	dc.w	pr_checknotecut-pr_e_command_checklist		; c
	dc.w	pr_checknotedelay-pr_e_command_checklist	; d
	dc.w	pr_checknotchannel-pr_e_command_checklist	; e
	dc.w	pr_checknotchannel-pr_e_command_checklist	; f

pr_effectchecklist:
	dc.w	pr_checkarpeggio-pr_effectchecklist		; 0
	dc.w	pr_checkperiodslideup-pr_effectchecklist	; 1
	dc.w	pr_checkperiodslidedown-pr_effectchecklist	; 2
	dc.w	pr_checktoneportamento-pr_effectchecklist	; 3
	dc.w	pr_checkvibrato-pr_effectchecklist		; 4
	dc.w	pr_checktpandvolslide-pr_effectchecklist	; 5
	dc.w	pr_checkvibandvolslide-pr_effectchecklist	; 6
	dc.w	pr_checktremolo-pr_effectchecklist		; 7
	dc.w	pr_checknotchannel-pr_effectchecklist		; 8
	dc.w	pr_checknotchannel-pr_effectchecklist		; 9
	dc.w	pr_checkvolumeslide-pr_effectchecklist		; a
	dc.w	pr_checknotchannel-pr_effectchecklist		; b
	dc.w	pr_checknotchannel-pr_effectchecklist		; c
	dc.w	pr_checknotchannel-pr_effectchecklist		; d
	dc.w	pr_check_e_commands-pr_effectchecklist		; e
	dc.w	pr_checknotchannel-pr_effectchecklist		; f

pr_prepfunkrepeat:
	move.b	5(a4),d0
	moveq	#$f,d1
	and.b	d1,d0
	move.b	d0,33(a4)
	tst.b	d0
	bne.s	pr_checkfunkrepeat
	rts
pr_checkfunkrepeat:
	move.w	32(a4),d0
	beq.s	pr_checkfunkrepeatend
	lea	pr_funktable(pc),a0
	move.b	(a0,d0.w),d0
	move.b	35(a4),d1
	add.b	d0,d1
	btst	#7,d1
	bne.s	pr_checkfunkrepeat2
	move.b	d1,35(a4)
	rts
pr_checkfunkrepeat2:
	clr.b	35(a4)

	move.l	36(a4),d0
	beq.s	pr_checkfunkrepeatend
	move.l	d0,d2
	moveq.l	#0,d1
	move.w	10(a4),d1
	add.l	d1,d0
	add.l	d1,d0
	move.l	40(a4),a0
	addq.l	#1,a0
	cmp.l	d0,a0
	blo.s	pr_checkfunkrepeatok
	move.l	d2,a0
pr_checkfunkrepeatok:
	move.l	a0,40(a4)
	moveq	#-1,d0
	sub.b	(a0),d0
	move.b	d0,(a0)
pr_checkfunkrepeatend:
	rts

pr_checknotedelay:
	move.b	5(a4),d0
	moveq	#$f,d1
	and.w	d1,d0
	bne.s	pr_checknotedelay2
	rts
pr_checknotedelay2:
	move.w	18(a4),d1
	addq.w	#1,d1
	cmp.w	d0,d1
	bne.s	pr_checknotedelayend
	move.w	d7,$96(a5)
	or.w	d7,pr_dmacon-pr_framecounter(a2)
	or.w	d7,dmaJustActived-pr_framecounter(a2)
	move.w	(a4),d0
	subq.w	#1,d0
	lsl.w	#5,d0
	lea	pr_sampleinfos(pc),a0
	move.l	(a0,d0.w),(a3)
	move.w	4(a0,d0.w),4(a3)
pr_checknotedelayend:
	move.w	d1,18(a4)
	rts

pr_checkretrignote:
	move.b	5(a4),d0
	moveq	#$f,d1
	and.w	d1,d0
	move.w	18(a4),d1
	addq.w	#1,d1
	cmp.w	d0,d1
	bne.s	pr_checkretrignoteend
pr_checkretrignote2:
	moveq	#0,d1
	move.w	d7,$96(a5)
	or.w	d7,pr_dmacon-pr_framecounter(a2)
	or.w	d7,dmaJustActived-pr_framecounter(a2)
	move.w	(a4),d0
	subq.w	#1,d0
	lsl.w	#5,d0
	lea	pr_sampleinfos(pc),a0
	move.l	(a0,d0.w),(a3)
	move.w	4(a0,d0.w),4(a3)
pr_checkretrignoteend:
	move.w	d1,18(a4)
	rts

pr_checknotecut:
	move.b	5(a4),d0
	moveq	#$f,d1
	and.w	d1,d0
	addq.w	#1,18(a4)
	move.w	18(a4),d1
	cmp.w	d0,d1
	blt.s	pr_checknotecutend
	clr.w	12(a4)
	clr.w	54(a4)
pr_checknotecutend:
	rts

pr_checkarpeggio:
	tst.b	5(a4)
	bne.s	pr_checkarpeggio0
	rts
pr_checkarpeggio0:
	move.w	(a2),d0
	lea	pr_arpeggiofastdivisionlist(pc),a1
	move.b	(a1,d0.w),d0
	beq.s	pr_checkarpeggio2
	cmp.b	#2,d0
	beq.s	pr_checkarpeggio1
	moveq	#0,d0
	move.b	5(a4),d0
	lsr.b	#4,d0
	bra.s	pr_checkarpeggio3
pr_checkarpeggio2:
	move.w	2(a4),6(a3)
	rts
pr_checkarpeggio1:
	moveq	#0,d0
	move.b	5(a4),d0
	and.b	#$f,d0
pr_checkarpeggio3:
	asl.w	#1,d0
	move.w	(a4),d1
	lsl.w	#5,d1
	lea	pr_sampleinfos+samplefinetuneoffset(pc),a0
	move.l	(a0,d1.w),a0
	move.w	2(a4),d1
	lea	pr_arpeggiofastlist(pc),a1
	moveq.l	#0,d2
	move.b	(a1,d1.w),d2
	add.b	d2,d2
	add.l	d2,a0
	moveq	#36,d7
pr_checkarpeggioloop:
	cmp.w	(a0)+,d1
	bhs.s	pr_checkarpeggio4
	dbf	d7,pr_checkarpeggioloop
	rts
pr_checkarpeggio4:
	subq.l	#2,a0
	move.w	(a0,d0.w),6(a3)
	rts

pr_checktpandvolslide:
	bsr	pr_checkvolumeslide
	moveq	#0,d2
	move.b	23(a4),d2
	move.w	26(a4),d0
	move.w	28(a4),d1
	bsr.s	pr_checktoneportamento2
	move.w	14(a4),26(a4)
	rts
	
pr_checktoneportamento:
	moveq	#0,d2
	move.b	5(a4),d2
	bne.s	pr_checktoneportamento1
	move.b	23(a4),d2
pr_checktoneportamento1:
	move.w	14(a4),d0
	move.w	16(a4),d1
pr_checktoneportamento2:
	cmp.w	d0,d1
	bgt.s	pr_checktoneportamentoplus
	blt.s	pr_checktoneportamentominus
	cmp.w	#1,(a2)
	beq.s	pr_savetpvalues
	rts
pr_checktoneportamentoplus:
	add.w	d2,d0
	cmp.w	d0,d1
	bgt.s	pr_checktoneportamentoend
	move.w	d1,d0
	move.w	d1,14(a4)
	move.w	d1,2(a4)
	tst.w	48(a4)
	bne.s	pr_checktoneportamentoglissando
	move.w	d1,6(a3)
	cmp.w	#1,(a2)
	beq.s	pr_savetpvalues
	rts
pr_checktoneportamentominus:
	sub.w	d2,d0
	cmp.w	d0,d1
	blt.s	pr_checktoneportamentoend
	move.w	d1,d0
	move.w	d1,14(a4)
	move.w	d1,2(a4)
	tst.w	48(a4)
	bne.s	pr_checktoneportamentoglissando
	move.w	d1,6(a3)
	cmp.w	#1,(a2)
	beq.s	pr_savetpvalues
	rts
pr_checktoneportamentoend:
	move.w	d0,14(a4)
	move.w	d0,2(a4)
	tst.w	48(a4)
	bne.s	pr_checktoneportamentoglissando
	move.w	d0,6(a3)
	cmp.w	#1,(a2)
	beq.s	pr_savetpvalues
	rts	
pr_savetpvalues:
	move.l	14(a4),26(a4)
	rts
pr_checktoneportamentoglissando:
	move.w	(a4),d1
	lsl.w	#5,d1
	lea	pr_sampleinfos+samplefinetuneoffset(pc),a0
	move.l	(a0,d1.w),a0
	lea	pr_arpeggiofastlist(pc),a1
	moveq.l	#0,d2
	move.b	(a1,d0.w),d2
	add.w	d2,d2
	add.l	d2,a0
	moveq	#0,d3
	moveq	#36*2,d1
pr_checktoneportamentoglissandoloop:
	cmp.w	(a0,d3.w),d0
	bhs.s	pr_checktoneportamentoglissando2
	addq.w	#2,d3
	cmp.w	d1,d3
	blo.s	pr_checktoneportamentoglissandoloop
	moveq	#35*2,d3
pr_checktoneportamentoglissando2:
	move.w	(a0,d3.w),6(a3)
	cmp.w	#1,(a2)
	beq.s	pr_savetpvalues
	rts

pr_checkvolumeslide:
	moveq	#0,d0
	move.b	5(a4),d0
	move.w	d0,d1
	lsr.b	#4,d1
	beq.s	pr_checkvolumeslidedown
	move.w	12(a4),d2
	add.w	d1,d2
	btst	#15,d2
	bne.s	pr_checkvolumeslide0
	moveq	#64,d0
	cmp.w	d0,d2
	bgt.s	pr_checkvolumeslide64
	move.w	d2,12(a4)
	move.w	d2,54(a4)
	rts
pr_checkvolumeslidedown:	
	and.b	#$f,d0
	move.w	12(a4),d2
	sub.w	d0,d2
	btst	#15,d2
	bne.s	pr_checkvolumeslide0
	moveq	#64,d0
	cmp.w	d0,d2
	bgt.s	pr_checkvolumeslide64
	move.w	d2,12(a4)
	move.w	d2,54(a4)
	rts
pr_checkvolumeslide64:
	move.w	d0,12(a4)
	move.w	d0,54(a4)
	rts
pr_checkvolumeslide0:
	clr.w	12(a4)
	clr.w	54(a4)
	rts
	
pr_checkperiodslidedown:
	moveq	#0,d0
	move.b	5(a4),d0
	add.w	d0,2(a4)
	cmp.w	#907,2(a4)
	bls.s	pr_checkperiodslidedown2
	move.w	#907,2(a4)
pr_checkperiodslidedown2:
	move.w	2(a4),6(a3)
	rts

pr_checkperiodslideup:
	moveq	#0,d0
	move.b	5(a4),d0
	sub.w	d0,2(a4)
	cmp.w	#108,2(a4)
	bge.s	pr_checkperiodslideup2
	move.w	#108,2(a4)
pr_checkperiodslideup2:
	move.w	2(a4),6(a3)
	rts

pr_checkvibandvolslide:
	bsr	pr_checkvolumeslide
	moveq.l	#0,d0
	moveq.l	#0,d1
	move.b	25(a4),d0
	move.b	24(a4),d1
	bra.s	pr_checkvibrato4

pr_checkvibrato:
	moveq.l	#0,d0
	moveq.l	#0,d1
	move.b	5(a4),d0	; tiefe
pr_checkvibrato2:
	move.w	d0,d1		; geschwindigkeit
	and.w	#$f,d0
	bne.s	pr_checkvibrato3
	move.b	25(a4),d0
pr_checkvibrato3:
	lsr.b	#4,d1
	bne.s	pr_checkvibrato4
	move.b	24(a4),d1
pr_checkvibrato4:
	move.w	18(a4),d2	;position
	lsr.w	#2,d2
	and.w	#$1f,d2
	move.w	50(a4),d3
	beq.s	pr_checkvibratosine
	btst	#0,d3
	bne.s	pr_checkvibratorampdown
	move.b	#255,d3
	bra.s	pr_checkvibratoset
pr_checkvibratorampdown:
	lsl.b	#3,d2
	tst.b	19(a4)
	bpl.s	pr_checkvibratorampdown2
	move.b	#255,d3
	sub.b	d2,d3
	bra.s	pr_checkvibratoset
pr_checkvibratorampdown2:
	move.b	d2,d3
	bra.s	pr_checkvibratoset
pr_checkvibratosine:
	lea	pr_vibratotable(pc),a0
	moveq	#0,d3
	move.b	(a0,d2.w),d3
pr_checkvibratoset:
	mulu.w	d0,d3
	lsr.w	#7,d3
	move.w	2(a4),d2
	tst.b	19(a4)
	bmi.s	pr_checkvibratoneg
	add.w	d3,d2
	bra.s	pr_checkvibrato5
pr_checkvibratoneg:
	sub.w	d3,d2
pr_checkvibrato5:
	move.w	d2,6(a3)
	lsl.w	#2,d1
	add.b	d1,19(a4)
	rts

pr_checktremolo:
	moveq	#0,d0
	moveq.l	#0,d1
	move.b	5(a4),d0	; tiefe
pr_checktremolo2:
	move.w	d0,d1		; geschwindigkeit
	and.w	#$f,d0
	bne.s	pr_checktremolo3
	move.b	31(a4),d0
pr_checktremolo3:
	lsr.b	#4,d1
	bne.s	pr_checktremolo4
	move.b	30(a4),d1
pr_checktremolo4:
	move.w	18(a4),d2	;position
	lsr.w	#2,d2
	and.w	#$1f,d2
	move.w	52(a4),d3
	beq.s	pr_checktremolosine
	btst	#0,d3
	bne.s	pr_checktremolorampdown
	move.b	#255,d3
	bra.s	pr_checktremoloset
pr_checktremolorampdown:
	lsl.b	#3,d2
	tst.b	19(a4)
	bpl.s	pr_checktremolorampdown2
	move.b	#255,d3
	sub.b	d2,d3
	bra.s	pr_checktremoloset
pr_checktremolorampdown2:
	move.b	d2,d3
	bra.s	pr_checktremoloset
pr_checktremolosine:
	lea	pr_vibratotable(pc),a0
	moveq	#0,d3
	move.b	(a0,d2.w),d3
pr_checktremoloset:
	mulu.w	d0,d3
	lsr.w	#6,d3
	move.w	20(a4),d2
	tst.b	19(a4)
	bmi.s	pr_checktremoloneg
	add.w	d3,d2
	moveq	#64,d4
	cmp.w	d4,d2
	bls.s	pr_checktremolo5
	move.w	d4,d2
	bra.s	pr_checktremolo5
pr_checktremoloneg:
	sub.w	d3,d2
	btst	#15,d2
	beq.s	pr_checktremolo5
	moveq	#0,d2
pr_checktremolo5:
	move.w	d2,54(a4)
	lsl.w	#2,d1
	add.b	d1,19(a4)
	rts

pr_vibratotable:	
	dc.b    0,  24,  49,  74,  97, 120, 141, 161
	dc.b  180, 197, 212, 224, 235, 244, 250, 253
	dc.b  255, 253, 250, 244, 235, 224, 212, 197
	dc.b  180, 161, 141, 120,  97,  74,  49,  24
pr_funktable:
	dc.b  0,5,6,7,8,10,11,13,16,19,22,26,32,43,64,128

	even
	
;* variables ***********************************************************

pr_module:			dc.l	0
pr_startposition:		dc.w	0
pr_speed:			dc.w	6
pr_highestpattern:		dc.w	0
pr_currentpattern:		dc.w	0
pr_framecounter:		dc.w	0
pr_patterndelaytime:		dc.w	0
pr_patternhasbeenbreaked:	dc.w	0
pr_patternpositions:		ds.l	128
pr_patternpt:			dc.l	0
pr_currentposition:		dc.l	0
pr_patternct:			dc.w	0
pr_channel0:	dc.w	1
		ds.w	27
pr_channel1:	dc.w	1
		ds.w	27
pr_channel2:	dc.w	1
		ds.w	27
pr_channel3:	dc.w	1
		ds.w	27
dmaJustActived:		dc.w	0
pr_dmacon:	dc.w	$8000
pr_old78:	dc.l	0
pr_arpeggiofastlist:	ds.b	1000
pr_arpeggiofastdivisionlist:	ds.b	$100
pr_fastperiodlist:	ds.l	16
pr_sampleinfos:		ds.b	32*32

pr_periods:
; tuning 0, normal
	dc.w	856,808,762,720,678,640,604,570,538,508,480,453
	dc.w	428,404,381,360,339,320,302,285,269,254,240,226
	dc.w	214,202,190,180,170,160,151,143,135,127,120,113
; tuning 1
	dc.w	850,802,757,715,674,637,601,567,535,505,477,450
	dc.w	425,401,379,357,337,318,300,284,268,253,239,225
	dc.w	213,201,189,179,169,159,150,142,134,126,119,113
; tuning 2
	dc.w	844,796,752,709,670,632,597,563,532,502,474,447
	dc.w	422,398,376,355,335,316,298,282,266,251,237,224
	dc.w	211,199,188,177,167,158,149,141,133,125,118,112
; tuning 3
	dc.w	838,791,746,704,665,628,592,559,528,498,470,444
	dc.w	419,395,373,352,332,314,296,280,264,249,235,222
	dc.w	209,198,187,176,166,157,148,140,132,125,118,111
; tuning 4
	dc.w	832,785,741,699,660,623,588,555,524,495,467,441
	dc.w	416,392,370,350,330,312,294,278,262,247,233,220
	dc.w	208,196,185,175,165,156,147,139,131,124,117,110
; tuning 5
	dc.w	826,779,736,694,655,619,584,551,520,491,463,437
	dc.w	413,390,368,347,328,309,292,276,260,245,232,219
	dc.w	206,195,184,174,164,155,146,138,130,123,116,109
; tuning 6
	dc.w	820,774,730,689,651,614,580,547,516,487,460,434
	dc.w	410,387,365,345,325,307,290,274,258,244,230,217
	dc.w	205,193,183,172,163,154,145,137,129,122,115,109
pr_arpeggiofastlistperiods:
; tuning 7
	dc.w	814,768,725,684,646,610,575,543,513,484,457,431
	dc.w	407,384,363,342,323,305,288,272,256,242,228,216
	dc.w	204,192,181,171,161,152,144,136,128,121,114,108
; tuning -8
	dc.w	907,856,808,762,720,678,640,604,570,538,508,480
	dc.w	453,428,404,381,360,339,320,302,285,269,254,240
	dc.w	226,214,202,190,180,170,160,151,143,135,127,120
; tuning -7
	dc.w	900,850,802,757,715,675,636,601,567,535,505,477
	dc.w	450,425,401,379,357,337,318,300,284,268,253,238
	dc.w	225,212,200,189,179,169,159,150,142,134,126,119
; tuning -6
	dc.w	894,844,796,752,709,670,632,597,563,532,502,474
	dc.w	447,422,398,376,355,335,316,298,282,266,251,237
	dc.w	223,211,199,188,177,167,158,149,141,133,125,118
; tuning -5
	dc.w	887,838,791,746,704,665,628,592,559,528,498,470
	dc.w	444,419,395,373,352,332,314,296,280,264,249,235
	dc.w	222,209,198,187,176,166,157,148,140,132,125,118
; tuning -4
	dc.w	881,832,785,741,699,660,623,588,555,524,494,467
	dc.w	441,416,392,370,350,330,312,294,278,262,247,233
	dc.w	220,208,196,185,175,165,156,147,139,131,123,117
; tuning -3
	dc.w	875,826,779,736,694,655,619,584,551,520,491,463
	dc.w	437,413,390,368,347,328,309,292,276,260,245,232
	dc.w	219,206,195,184,174,164,155,146,138,130,123,116
; tuning -2
	dc.w	868,820,774,730,689,651,614,580,547,516,487,460
	dc.w	434,410,387,365,345,325,307,290,274,258,244,230
	dc.w	217,205,193,183,172,163,154,145,137,129,122,115
; tuning -1
	dc.w	862,814,768,725,684,646,610,575,543,513,484,457
	dc.w	431,407,384,363,342,323,305,288,272,256,242,228
	dc.w	216,203,192,181,171,161,152,144,136,128,121,114
