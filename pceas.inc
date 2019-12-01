; --------
; This block defines names for macro
; argument types (\?x).
;

ARG_NONE	.equ 0
ARG_REG		.equ 1
ARG_IMMED	.equ 2
ARG_ABS		.equ 3
ARG_ABSOLUTE	.equ 3
ARG_INDIRECT	.equ 4
ARG_STRING	.equ 5
ARG_LABEL	.equ 6

;
;
;

vreg		macro 
		lda	\1
		sta	<vdc_reg
.if (\?1 = ARG_IMMED)
		st0	\1
.else
		sta	video_reg
.endif
		endm

;
; STW - store a word-sized value at stated memory location
;

stw		macro
		lda	LOW_BYTE \1
		sta	LOW_BYTE \2
		lda	HIGH_BYTE \1
		sta	HIGH_BYTE \2
		endm

stb		macro
		lda	\1
		sta	\2
		endm

SPR_ALIGN	macro
		if	(* & 127)
		ds	128 - (* & 127)		; 128-byte align the data.
		endif
		endm

CHR_ALIGN	macro
		if	(* & 31)
		ds	32 - (* & 31)		; 32-byte align the data.
		endif
		endm

WORD_ALIGN	macro
		if	(* & 1)
		ds	2 - (* & 1)		; Word Align the data.
		endif
		endm

LONG_ALIGN	macro
		if	(* & 3)
		ds	4 - (* & 3)		; Long Align the data.
		endif
		endm

PAGE_ALIGN	macro
		if	(* & 255)
		ds	256 - (* & 255)		; Page Align the data.
		endif
		endm
