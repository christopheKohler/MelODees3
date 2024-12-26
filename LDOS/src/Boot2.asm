;---------------------------------------------------------
;
;	LDOS (Leonard Demo Operating System)
;	AMIGA version
;	Written by Leonard/Oxygene
;
;	Boot sector of second disk
;
;---------------------------------------------------------

bootStart:
	dc.b 'DOS',0
	dc.b 'DP.2'			; 4
	dc.l 880			; 8
