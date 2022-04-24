; ***************************************************************************
; ***************************************************************************
;
; teos.s
;
; A replacement Operating System for KRIKzz's Turbo Everdrive v2.
;
; Copyright John Brandwood 2019.
;
; Distributed under the Boost Software License, Version 1.0.
; (See accompanying file LICENSE_1_0.txt or copy at
;  http://www.boost.org/LICENSE_1_0.txt)
;
; ***************************************************************************
; ***************************************************************************

		;
		;
		;

REALHW		=	1
SUPPORT_MOUSE	=	1
HIRES		=	1

		;
		; Inlude definitions for PC Engine assembly-language coding.
		;

		include	"pceas.inc"
		include	"pcengine.inc"
		include	"ted2.inc"

PUTS		macro
		ldx	#< \1
		lda	#> \1
		jsr	tos_print_msg
		endm

		;
		;
		;


		list
		mlist

ASCII_HI	=	$01

		zp

view:		ds	2

                bss
                org	$22D0

		data

		code



; ***************************************************************************
; ***************************************************************************
;
;
;

WRAM_BANK	=	$05			; Writable in TED2. 
FRAG_BANK	=	$06			; Writable in TED2.

BRAM_BANK	=	$20			; 128KB allowed for MB128
SLOT_BANK	=	$30			; 128KB allowed for MB128



; ***************************************************************************
; ***************************************************************************
;
;
;

		include	"teos_bank0.s"
		include	"teos_bank1.s"
		include	"teos_bank2.s"
		include	"teos_bank3.s"

		if	REALHW
		else
		include	"fakesd.s"
		endif
