;---------------------------------------------------------
;
;	LDOS (Leonard Demo Operating System)
;	AMIGA version
;	Written by Leonard/Oxygene
;
;	Public function library	
;
;---------------------------------------------------------

; https://blastfromthepast.atlassian.net/wiki/spaces/REV18/pages/29229075/LDOS

LDOS_BASE				=	$4

LDOS_PRELOAD_NEXT_FX		=	0*4
LDOS_WAIT_VBLANK			=	1*4
LDOS_INSTALL_VCALLBACK		=	2*4
LDOS_MEMORY_BATCH_ALLOC		=	3*4
LDOS_INSTALL_COPPERL		=	4*4

; LZ4 fast depack
; in : a0.l : LZ4 packed data
;      a1.l : LZ4 depacked data buffer
; Note: you can use depack in place, a1 should be at least 64 bytes after a0 (yeah 64bytes is enough!!)
LDOS_FAST_DEPACK			=	5*4

LDOS_GET_FRAME_COUNTER		=	6*4
LDOS_ASSERT					=	7*4
LDOS_MUSIC_START			=	8*4
LDOS_MUSIC_GET_POSITION 	=	9*4
LDOS_MUSIC_STOP				=	10*4
LDOS_IS_DISK2_INSERTED		=	11*4

; Alloc persistent CHIP memory block between two FX
; in : d0.l: size of block
; out: d0.l: Addr of the block
LDOS_PERSISTENT_CHIP_ALLOC	=	12*4

; Retreive any previously persistent CHIP memory block between two FX
; in : none
; out: d0.l: Addr of block ( or NULL if no persistent block)
;	   d1.l: Size of block ( in bytes )
LDOS_PERSISTENT_CHIP_GET	=	13*4

; Free any previously persistent CHIP memory block
; in : none
; out: none
LDOS_PERSISTENT_CHIP_TRASH	=	14*4

; Internal use only, don't!
LDOS_SET_KERNEL_BPM			=	15*4
LDOS_FREE_MEMORY_AREA		=	16*4

LDOS_MUSIC_SET_POSITION_IN_SYNC 	=	17*4

LDOS_MUSIC_RELOC                    =	18*4 ; [CK]
LDOS_MUSIC_PRELOAD                  =	19*4 ; [CK]
LDOS_GETMEMBLOCKTABLE               =   20*4 ; [CK] d0 = chip, d1 = fast
LDOS_DATA_LOAD                      =   21*4 ; [CK] d0.l = index (alloc chip mem)
LDOS_FREE_MEM_DATA                  =   22*4 ; [CK] (will free mem of LDOS_DATA_LOAD )
LDOS_MUSIC_GET_TRIGGER              =   23*4 ; [CK] d0 = trigger value
LDOS_FREE_MEM_MUSIC                 =   24*4 ; [CK] (will free mem of Music )

LDOS_MEM_ANY_RAM		=	(1<<30)
LDOS_MEM_CLEAR			=	(1<<29)
