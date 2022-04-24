; ***************************************************************************
; ***************************************************************************
;
; bram.s
;
; Functions for using the Backup RAM in a PCE CD system or the Tennokoe 2.
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
; bm_prepare_mpr - Prepare bank mapping for a BRAM operation.
;
; Uses: MPR2 = BRAM bank ($F7).
; Uses: MPR3 = Bank containing image of BRAM to load/store.
;
; N.B. Current bank mappings are saved.
;

bm_prepare_mpr:	plx				; Save the return address.
		ply
		php
		sei
		tma2				; Preserve MPR2 & MPR3.
		pha
		tma3
		pha
		csl				; BRAM needs lo-speed access.
		lda	#$F7			; Map in the BRAM and shadow.
		tam2
		lda	#$48			; Extended 3-byte unlock
		sta	IFU_BRAM_UNLOCK		; sequence for Tennokoe 2.
		lda	#$75
		sta	IFU_BRAM_UNLOCK
		lda	#$80
		sta	IFU_BRAM_UNLOCK
		phy				; Restore the return address.
		phx
		rts



; ***************************************************************************
; ***************************************************************************
;
; bm_restore_mpr - BRAM operation finished, restore previous bank mapping.
;

bm_restore_mpr:	plx				; Save the return address.
		ply
		lda	IFU_BRAM_LOCK		; Lock BRAM.
		lda	VDC_SR			; Skip any pending VDC irq.
		pla				; Restore MPR2 & MPR3.
		tam3
		pla
		tam2
		csh
		plp				; Restore original IRQ mask.
		phy				; Restore the return address.
		phx
		rts



; ***************************************************************************
; ***************************************************************************
;
; bm_exists - Confirm that BRAM exists on this console.
;

bm_exists:	jsr	bm_prepare_mpr

		lda	$47FF			; Confirm BRAM present.
		tax
		eor	#$FF
		sta	$47FF
		eor	$47FF
		stx	$47FF
		sta	<__al

		jsr	bm_restore_mpr		; Do NOT shorten this to a JMP!

		lda	<__al			; Returns Z if BRAM exists.
		rts



; ***************************************************************************
; ***************************************************************************
;
; bm_check_size - Check the size of the BRAM hardware (fixed at 2048 bytes).
;
; Returns: A = Size of BRAM as # of 256-byte pages (0 and Z flag if none).
;

		if	0

bm_check_size:	jsr	bm_prepare_mpr

		lda	#$FF			; Confirm "BRAM size".
		sta	<__ax + 0		; Check for 2KBytes maximum.
		lda	#$47
		sta	<__ax + 1

		cla
.find_loop:	sta	[__ax]
		cmp	[__ax]
		beq	.found
		dec	<__ax + 1
		bbs6	<__ax + 1, .find_loop	; Continue if addr >= $4000.

.found:		inc	<__ax + 1

.exit:		jsr	bm_restore_mpr		; Do NOT shorten this to a JMP!

		lda	<__ax + 1		; Subtract bank address to
		sec				; leave size in pages.
		sbc	#$40

		rts



; ***************************************************************************
; ***************************************************************************
;
; bm_format_bram - Format the BRAM.
;

bm_format_bram:	jsr	bm_check_size		; Confirm "BRAM size".
		beq	.no_bram		; Exit if none.

		jsr	bm_prepare_mpr

		lda	#$F7			; Map in the BRAM and shadow.
		tam3

		jsr	bm_format_mpr3

		jsr	bm_restore_mpr		; Do NOT shorten this to a JMP!

		lda	<__ax + 1		; Return BRAM size.
.no_bram:	rts

		endif



; ***************************************************************************
; ***************************************************************************
;
; bm_bram_to_bank - Copy BRAM image from actual BRAM hardware to RAM bank.
;

bm_bram_to_bank:jsr	bm_prepare_mpr

		tii	$4000, $6000, 2048

		if	REALHW
		else
		lda	#BANK(fake_bram2)
		tam2
		tii	fake_bram2, $6000, 2048
		endif

		jsr	bm_restore_mpr		; Do NOT shorten this to a JMP!
		rts



; ***************************************************************************
; ***************************************************************************
;
; bm_bank_to_bram - Copy BRAM image in RAM bank to actual BRAM hardware.
;

bm_bank_to_bram:jsr	bm_prepare_mpr

		tii	$6000, $4000, 2048

		jsr	bm_restore_mpr		; Do NOT shorten this to a JMP!
		rts



; ***************************************************************************
; ***************************************************************************
;
; bm_check_bram -
;
; Uses: __al = Return Code
;                $FF = Unformatted.
;                $00 = Formatted & OK.
;                $01 = Formatted, but has errors.
;
; Uses: __ah = BRAM Size ($00=none, $88=2KB, or else unexpected size).
; Uses: __bx = # of files in BRAM.
; Uses: __bp = Ptr to next free (in $6010..$67FF range).
;

bm_check_bram:	jsr	bm_prepare_mpr

		lda	#$F7			; Map the BRAM into MPR3 for
		tam3				; testing.

		jsr	bm_verify_mpr3		; Verify the integrity.

		jsr	bm_restore_mpr		; Do NOT shorten this to a JMP!

		rts



; ***************************************************************************
; ***************************************************************************
;
; bm_format_mpr3 - Format the BRAM image mapped into MPR3 ($6000..$67FF).
;

bm_format_mpr3:	tai	tos_zero, $6000, $0800

		tdd	bm_signature + 7, $6007, 8

		rts

bm_signature:	db	"HUBM"
		dw	$8800
		dw	$8010



; ***************************************************************************
; ***************************************************************************
;
; bm_verify_mpr3 - Verify the BRAM data mapped into MPR3 ($6000..$67FF).
;
; Uses: __al = Return Code
;                $FF = Unformatted.
;                $00 = Formatted & OK.
;                $01 = Formatted, but has errors.
;
; Uses: __ah = BRAM Size ($00=none, $88=2KB, or else unexpected size).
; Uses: __bx = # of files in BRAM.
; Uses: __bp = Ptr to next free (in $6010..$67FF range).
;

bm_verify_mpr3:	lda	#$FF
		sta	<__al			; $FF = Unformatted.

		stz	<__ah			; Return BRAM Size.
		stz	<__bl			; Return # of files (lo).
		stz	<__bh			; Return # of files (hi).

		ldx	#$FB			; Check signature & lo-byte
.test_header:	lda	bm_signature - $FB,x	; of size.
		cmp	$6000 - $FB,x
		bne	.test_present
		inx
		bne	.test_header

		stz	<__al			; $00 = Formatted.

		lda	$6005			; Check that BRAM Size is 2KB.
		sta	<__ah
		cmp	#$88
		bne	.free_wrong

		ldy	#<$6010			; First file in chain.
		lda	#>$6010

.file_loop:	sty	<__bp + 0		; Update file pointer.
		sta	<__bp + 1

.test_free:	cpy	$6006
		bne	.test_size
		eor	#$E0			; Xvert $6000 base to $8000.
		cmp	$6007
		beq	.found_end

.test_size:	ldy	#1			; End of file chain?
		lda	[__bp],y
		tay
		ora	[__bp]
		beq	.found_end

		lda	[__bp]
.too_small:	cmp	#$10			; Size < $0010 (min size)?
		bcs	.too_large
		cpy	#$00
		beq	.bram_corrupt
.too_large:	cpy	#$20			; Size > $1FFF (max size)?
		bcs	.bram_corrupt

		clc				; Calc address of next file.
		adc	<__bp + 0
		say
		adc	<__bp + 1
		cmp	#$68			; Is it beyond BRAM Size?
		bcs	.bram_corrupt

		inc	<__bl			; # of files found.
		bne	.file_loop
		inc	<__bh
		bra	.file_loop

.bram_corrupt:	smb1	<__al			; $01 = Formatted, but errors.

;		ldy	#1			; Repair damage.
;		cla
;		sta	[__bp],y
;		sta	[__bp]

.found_end:	ldy	<__bp + 0
		lda	<__bp + 1
		eor	#$E0			; Xvert $6000 base to $8000.

		cpy	$6006			; Check BRAM Free address.
		bne	.free_wrong
		cmp	$6007
		beq	.finished

.free_wrong:	smb1	<__al			; $01 = Formatted, but errors.

;		sty	$6006			; Repair damage.
;		sta	$6007

.finished:	lda	<__al
		rts

		;

.test_present:	stz	$67FF			; Confirm that BRAM is present.
		lda	$67FF
		bne	.finished

		lda	#$88			; Report that BRAM Size is 2KB.
		sta	<__ah
		bra	.finished



; ***************************************************************************
; ***************************************************************************
;
; bm_repair_mpr3 - Repair the BRAM data mapped into MPR3 ($6000..$67FF).
;

bm_repair_mpr3:	jsr	bm_verify_mpr3

		ldy	<__bp + 0		; Fix EOF address pointer.
		lda	<__bp + 1
		sty	.cleanup + 3
		sta	.cleanup + 4
		eor	#$E0			; Xvert $6000 base to $8000.
		sty	$6006			; Fix pointer to next free.
		sta	$6007

		sec				; Fix EOF marker and clear
		lda	$6004			; the free space.
		sbc	$6006
		sta	.cleanup + 5
		lda	$6005
		sbc	$6007
		sta	.cleanup + 6

.cleanup:	tai	tos_zero, $6000, $0800

		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_bram_csum - Calc the checkum of a file in a BRAM image.
;
; Args: __ax = Ptr to the file header in the BRAM image.
; Uses: __si = Temporary ptr.
; Uses: __bx = Checksum result.
;

		if	0

tos_bram_csum:	lda	<__ax + 0
		sta	<__si + 0
		lda	<__ax + 1
		sta	<__si + 1

		lda	[__si]
		sec
		sbc	#4
		tax
		ldy	#1
		lda	[__si],y
		sbc	#0
		inc	a
		sta	<__bl

		ldy	#4
		cla
		sta	<__bx + 1

.sum_loop:	clc
		adc	[__si],y
		bcc	.sum_skip
		inc	<__bx + 1
.sum_skip:	iny
		bne	.ptr_skip
		inc	<__si + 1
.ptr_skip:	dex
		bne	.sum_loop
		dec	<__bl
		bne	.sum_loop
		sta	<__bx + 0
		rts

		endif
