; ***************************************************************************
; ***************************************************************************
;
; aplib.s
;
; HuC6280 decompressor for data stored in Jorgen Ibsen's aPLib format.
;
; Copyright John Brandwood 2019.
;
; Distributed under the Boost Software License, Version 1.0.
; (See accompanying file LICENSE_1_0.txt or copy at
;  http://www.boost.org/LICENSE_1_0.txt)
;
; ***************************************************************************
; ***************************************************************************



; ***************************************************************************
; ***************************************************************************
;
; Decompression Options & Macros
;

		;
		; Assume that we're decompessing from a large multi-bank
		; compressed data file and that MPR3 (and MPR4) are used
		; as a window into that file?
		;

APL_FROM_MPR3	=	0

		;
		; Macro to increment the source pointer to the next page.
		;

		if	APL_FROM_MPR3
APL_INC_PAGE	macro
		bsr	.next_page
		endm
		else
APL_INC_PAGE	macro
		inc	<apl_srcptr + 1
		endm
		endif

		;
		; Macros to read a byte/bit from the compressed source data.
		;

APL_JMP_TII	=	1
APL_GET_BIT	macro
		asl	<apl_bitbuf
		bne	.skip\@
		bsr	.load_bit
.skip\@:
		endm
APL_GET_SRC	macro
		lda	[apl_srcptr],y
		iny
		bne	.skip\@
		APL_INC_PAGE
.skip\@:
		endm

APL_GET_SRCX	macro
		lda	[apl_srcptr]		; Can be used if you *really*
		inc	<apl_srcptr + 0		; don't want to map MPR4.
		bne	.skip\@
		APL_INC_PAGE
.skip\@:
		endm



; ***************************************************************************
; ***************************************************************************
;
; Data usage is pretty-much all of the temporary System Card locations.
;
; Note that the TII instruction runs into the bottom 2 bytes of the stack.
;

apl_bitbuf	=	__bp			; 1 byte.

apl_srcptr	=	__si			; 1 word.
apl_offset	=	__di			; 1 word.

apl_winptr	=	__ah			; Part of TII instruction.
apl_dstptr	=	__bh			; Part of TII instruction.
apl_length	=	__ch			; Part of TII instruction.



; ***************************************************************************
; ***************************************************************************
;
; apl_decompress - Decompress data stored in Jorgen Ibsen's aPLib format.
;
; Args: apl_srcptr = ptr to compessed data
; Args: apl_dstptr = ptr to output buffer
; Uses: __bp, __si, __di, __ax, __bx, __cx, __dx
;
; If compiled with APL_FROM_MPR3, then apl_srcptr should be within MPR3, i.e.
; in the range $6000..$7FFF, and both MPR3 & MPR4 should be set.
;
; As an optimization, the code to handle window offsets > 64768 bytes has
; been removed, since these don't occur with a 16-bit address range.
;
; As an optimization, the code to handle window offsets > 32000 bytes has
; been commented-out, since these don't occur in typical PC Engine usage.
;

apl_decompress: lda	#$73			; TII instruction.
		sta	<__al

		stz	<apl_length + 1

		tii	.tii_end, __dh, 3	; TII ends with JMP.

		lda	#$80			; Initialize an empty
		sta	<apl_bitbuf		; bit-buffer.

		cly				; Initialize source index.

		;
		; 0 bbbbbbbb - One byte from compressed data, i.e. a "literal".
		;

.literal:	APL_GET_SRC

.write_byte:	clx				; LWM=0.

		sta	[apl_dstptr]		; Write the byte directly to
		inc	<apl_dstptr + 0		; the output.
		bne	.next_tag
		inc	<apl_dstptr + 1

.next_tag:	asl	<apl_bitbuf		; 0 bbbbbbbb
		bcc	.literal
		bne	.skip1
		bsr	.load_bit
		bcc	.literal

.skip1:		asl	<apl_bitbuf		; 1 0 <offset> <length>
		bne	.skip2
		bsr	.load_bit
.skip2:		bcc	.code_pair

		asl	<apl_bitbuf		; 1 1 0 dddddddn
		bne	.skip3
		bsr	.load_bit
.skip3:		bcc	.copy_two_three

		; 1 1 1 dddd - Copy 1 byte within 15 bytes (or zero).

.copy_one:	lda	#$10
.nibble_loop:	asl	<apl_bitbuf
		bne	.skip4
		bsr	.load_bit
.skip4:		rol	a
		bcc	.nibble_loop
		beq	.write_byte		; Offset=0 means write zero.

		eor	#$FF
		adc	<apl_dstptr + 0
		sta	<apl_winptr + 0
		lda	#$FF
		adc	<apl_dstptr + 1
		sta	<apl_winptr + 1

		lda	[apl_winptr]
		bra	.write_byte

		;
		; Subroutines for byte & bit handling.
		;

		if	APL_FROM_MPR3
.next_page:	inc	<apl_srcptr + 1		; Increment source page, and
		bmi	.next_bank		; check if changed bank.
		rts

.next_bank:	pha				; Increment source bank view
		tma3				; within a large data file.
		inc	a
		tam3
		inc	a
		tam4
		lda	#$60
		sta	<apl_srcptr + 1
		pla
		rts
		endif

.load_bit:	pha				; Reload an empty bit-buffer
		APL_GET_SRC			; from the compressed source.
		rol	a
		sta	<apl_bitbuf
		pla
		rts

.get_gamma:	lda	#1			; Get a gamma-coded value.
;		stz	<apl_length + 1
.gamma_loop:	asl	<apl_bitbuf
		bne	.skip5
		bsr	.load_bit
.skip5:		rol	a
;		rol	<apl_length + 1
		asl	<apl_bitbuf
		bne	.skip6
		bsr	.load_bit
.skip6:		bcs	.gamma_loop
		rts

.match_done:	ldx	#1			; LWM=1.
		clc				; Update destination address.
		lda	<apl_length + 0
		adc	<apl_dstptr + 0
		sta	<apl_dstptr + 0
		bcc	.next_tag
		inc	<apl_dstptr + 1
		bra	.next_tag

		;
		; 1 1 0 dddddddn - Copy 2 or 3 within 128 bytes.
		;

.copy_two_three:APL_GET_SRC			; 1 1 0 dddddddn
		lsr	a
		beq	.finished		; offset 0 == EOF.

		sta	<apl_offset + 0		; Preserve offset.
		stz	<apl_offset + 1
		cla
		adc	#2
		sta	<apl_length + 0
		stz	<apl_length + 1
		bra	.do_match

.finished:	rts				; All decompressed!

		;
		; 1 0 <offset> <length> - gamma-coded LZSS pair.
		;

.code_pair:	bsr	.get_gamma		; Bits 8..15 of offset (min 2).

		cpx	#1			; CC if LWM==0, CS if LWM==1.
		sbc	#2			; -3 if LWM==0, -2 if LWM==1.
		bcs	.normal_pair		; CC if LWM==0 && offset==2.

		bsr	.get_gamma		; Get Length (lo-byte in A).
		bra	.do_match		; Use previous Offset.

.normal_pair:	sta	<apl_offset + 1		; Save bits 8..15 of offset.

		APL_GET_SRC
		sta	<apl_offset + 0		; Save bits 0...7 of offset.

		bsr	.get_gamma		; Get length (lo-byte in A).

		ldx	<apl_offset + 1		; If offset <	 256.
		beq	.lt256
;		cpx	#$7D			; If offset >= 32000, length += 2.
;		bcs	.match_plus2
		cpx	#$05			; If offset >=	1280, length += 1.
		bcs	.match_plus1
		bra	.do_match
.lt256:		ldx	<apl_offset + 0		; If offset <	 128, length += 2.
		bmi	.do_match

.match_plus2:	inc	a			; Increment length.
;		bne	.match_plus1
;		inc	<apl_length + 1

.match_plus1:	inc	a			; Increment length.
;		bne	.do_match
;		inc	<apl_length + 1

.do_match:	sta	<apl_length + 0

		sec				; Calc address of match.
		lda	<apl_dstptr + 0
		sbc	<apl_offset + 0
		sta	<apl_winptr + 0
		lda	<apl_dstptr + 1
		sbc	<apl_offset + 1
		sta	<apl_winptr + 1

		jmp	__ax

.tii_end:	jmp	.match_done
