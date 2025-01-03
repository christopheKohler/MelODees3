;---------------------------------------------------------
;
;	LDOS (Leonard Demo Operating System)
;	AMIGA version
;	Written by Leonard/Oxygene
;
;	Relocation code
;
;---------------------------------------------------------


		include	"kernelPrivate.inc"

		


MAX_HUNKS	=	32


relocCrcStart:


amigaReloc:
				
			lea		-m_relocSizeof(a7),a7
			movea.l	a7,a6

		; parse Amiga EXE header
			lea		(nextFx+m_ad)(pc),a1
			move.l	(a1),a0
			clr.l	(a1)				; clear nextRunAd so the next hunkCode could set the right pointer
			cmpi.l	#$3f3,(a0)+
			bne		relocError
			tst.l	(a0)+				; string must be empty
			bne		relocError
			move.l	(a0)+,d0			; hunk count
			beq		relocError
			cmpi.l	#MAX_HUNKS,d0
			bge		relocError
			move.w	d0,m_relHunkCount(a6)
			addq.w	#8,a0				; skip first and last hunk id
			
		; now build a LDOS dynamic alloc request (to alloc everything in one go)
			lea		m_hunkAds(a6),a1
			subq.w	#1,d0
.tLoop:		move.l	(a0)+,d1
			move.l	#$00ffffff,d2
			and.l	d1,d2
			lsl.l	#2,d2					; DWORD to bytes
			btst	#30,d1
			bne.s	.chip
			ori.l	#LDOS_MEM_ANY_RAM,d2
.chip:		move.l	d2,(a1)+
			dbf		d0,.tLoop
			move.l	#-2,(a1)+				; end marker
			
		; Now allocate everything in one go			
			move.l	a0,-(a7)
			lea		m_hunkAds(a6),a0
			bsr		batchAllocator
			move.l	(a7)+,a0
			
		; browse all hunks, move each one to its new memory zone, and backup relocation tables
			clr.w	m_relHunkId(a6)
.hunkLoop:	move.w	m_relHunkId(a6),d0
			cmp.w	m_relHunkCount(a6),d0
			beq.s	.theEnd

			lsl.w	#2,d0
			lea		m_hunkAds(a6,d0.w),a5
			pea		.hunkLoop(pc)

			move.l	(a0)+,d1				; chunk id
			andi.l	#$3fffffff,d1
			cmpi.l	#$3e9,d1
			beq		hunkCode
			cmpi.l	#$3ea,d1
			beq		hunkData
			cmpi.l	#$3eb,d1
			beq		hunkBss			
			bra		relocError

			
.theEnd:	
			lea		m_relocSizeof(a7),a7
			rts
			
		
hunkCode:	
hunkData:	
			move.l	(a0)+,d0
			lsl.l	#2,d0
			move.l	(a5),a1
			cmpi.l	#$3e9,d1
			bne.s	.noCode
			lea		(nextFx+m_ad)(pc),a2
			tst.l	(a2)
			bne.s	.noCode
			move.l	a1,(a2)			
.noCode:	
		; WARNING: here we should always copy to lower ad ( dst < src )
			bsr		fastMemcpy
			add.l	d0,a0

		; maybe reloc hunk here
			cmpi.l	#$3ec,(a0)
			bne.s	hunkExit
			addq.w	#4,a0

.offLoop:	move.l	(a0)+,d0					; offset count
			beq.s	hunkExit
			move.l	(a0)+,d1					; hunk number
			lsl.w	#2,d1
			move.l	m_hunkAds(a6,d1.w),d1		; hunk base
			subq.w	#1,d0
.pLoop:		move.l	(a0)+,d2
			add.l	d1,0(a1,d2.l)
			dbf		d0,.pLoop
			bra.s	.offLoop		
							
hunkExit:	cmpi.l	#$3f2,(a0)+
			bne		relocError
			addq	#1,m_relHunkId(a6)
			rts
			
hunkBss:	addq.w	#4,a0					; skip bss size
			bra.s	hunkExit

				
				rsreset
m_relHunkCount:		rs.w	1
m_relHunkId:		rs.w	1
m_hunkAds:			rs.l	MAX_HUNKS+1		
m_relocSizeof:		rs.w	1
		
; Reloc "LEOR" executable ( as68 )		
leonardReloc:

			; alloc the size
			lea		nextFx(pc),a2
			move.l	m_size(a2),d0	; size in bytes
			bsr		allocAnyMem

			lea		(nextFx+m_ad)(pc),a1
			move.l	(a1),a0				; src
			move.l	d0,(a1)				; patch new next run ad (dst)
			move.l	d0,a1
			move.l	m_size(a2),d0	; size in bytes
			bsr		fastMemcpy

			move.l	m_ad(a2),a0

			cmpi.w	#$6000,(a0)
			bne.s	.noLeonard
			cmpi.l	#'LEOR',4(a0)
			bne.s	.noLeonard

			lea		8(a0),a1
			moveq	#0,d1
			move.w	2(a0),d1
			addq.w	#2,d1			; header size
			add.l	a0,d1			; ad to add
			
.loop:		move.l	(a1)+,d0		; offset to patch
			bmi.s	.noLeonard
			add.l	d1,0(a0,d0.l)
			bra.s	.loop
				
.noLeonard:		rts


relocError:
			lea		.txt(pc),a0
			trap	#0
.txt:		dc.b	'RELOC Error',0
			even

			
; AMIGA Module relocation
; split data between music score data ( any ram ) and samples data (CHIP)
		IF USE_P61
		{
relocP61:
		; first, switch off module and free
			lea		bMusicPlay(pc),a0
			tst.w	(a0)
			beq.s	.noPlaying
			clr.w	(a0)
			bsr		vSync
			bsr		musicPlayer+4				; pr_end
.noPlaying:			
			moveq	#MEMLABEL_MUSIC,d0
			bsr		freeMemLabel

			move.b	#MEMLABEL_MUSIC,(SVAR_CURRENT_MEMLABEL).w

			move.l	(nextMusic+m_ad)(pc),a0
			moveq	#0,d0
			move.w	(a0),d0					; score size
			bsr 	allocAnyMemCopy

			lea		pModule(pc),a0
			move.l	d0,(a0)
			
			move.l	(nextMusic+m_ad)(pc),a0
			move.l	(nextMusic+m_size)(pc),d0		; mod size in bytes
			moveq	#0,d1
			move.w	(a0),d1					; score size
			sub.l	d1,d0					; sample size
			add.l	d1,a0					; samples start
			bsr 	allocChipMemCopy

			move.l	d0,a1
			move.l	pModule(pc),a0
			bsr		musicPlayer+0

			moveq	#0,d0
			move.w	(nextMusic+m_arg)(pc),d0
			beq.s	.noSetPos
			bsr		musicPlayer+20			; music set position
.noSetPos:
			lea		fadeOutStep(pc),a0
			clr.w	(a0)
			moveq	#64,d0
			bsr		musicPlayer+16			; music set volume			
			
			rts
		}
			
			
			
	IF	_DEBUG
	{
memmoveError:
			lea		.txt(pc),a0
			trap	#0
.txt:		dc.b	'memmove ERROR',0
			even
	}
	
relocCrcEnd:
