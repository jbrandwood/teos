TOS_OK		=	$00

TOS_BAD_HUCARD	=	$A1
TOS_WRONG_SIZE	=	$A2

		.bss

FILES_PER_PAGE	=	20
MAX_NUM_PAGES	=	99
MAX_NAME_LENGTH	=	64			; 120 if scrolling.

		; Current selection path, here in the OS itself so that
		; it is can persist in TED2 RAM while a cart is run.

tos_file_count:	ds	2
tos_show_page:	ds	1			; Only for onscreen display.
tos_num_pages:	ds	1
tos_num_files:	ds	1

tos_name_length:ds	FILES_PER_PAGE * 1
tos_1st_cluster:ds	FILES_PER_PAGE * 4
tos_file_length:ds	FILES_PER_PAGE * 4
tos_file_type:	ds	FILES_PER_PAGE * 1

		.code



; ****************************************************************************
; ****************************************************************************
;
; tos_decompress - Decompress data fron the aPLib file container.
;
; Args: Y - chunk offset into container file (4 or 8 for 1st 2 files).
;

tos_decompress:	clc
		lda	apl_font_data + 0,y
		adc	#<apl_font_data
		sta	<apl_srcptr + 0
		lda	apl_font_data + 1,y
		adc	#>apl_font_data
		and	#$1F
		ora	#$60
		sta	<apl_srcptr + 1

		lda	#BANK(apl_font_data) + 1
		tam3

		lda	#>$3000
		stz	<apl_dstptr + 0
		sta	<apl_dstptr + 1

		jsr	apl_decompress
		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_screen_mode - Change screen mode.
;
; Args X reg = Lo-byte of address of screen definition table.
; Args A reg = Hi-byte of address of screen definition table.
;

tos_screen_mode:pha
		lda	#$08			; BKG & SPR off, VSYNC IRQ on.
		sta	<vdc_crl
		jsr	wait_vsync
		pla

		jsr	init_vdc

		lda	#$88			; BKG on, VSYNC IRQ on.
		sta	<vdc_crl

		rts



; ****************************************************************************
; ****************************************************************************
;
; tos_scan_dir - Change directory and update UI info for the new directory.
;

tos_scan_dir:	lda	#$FF			; Find out how many selectable
		sta	<__ax + 0               ; entries there are in this
		sta	<__ax + 1               ; directory.
		jsr	tos_fastfwd_dir

		lda	<__ax + 0		; Xvert the return value into
		eor	#$FF			; a file count.
		sta	<__ax + 0
		sta	tos_file_count + 0
		lda	<__ax + 1
		eor	#$FF
		sta	<__ax + 1
		sta	tos_file_count + 1

		lda	#FILES_PER_PAGE		; Find out how many pages of
		sta	<__cl			; entries in this directory.
		jsr	tos_div16_7u

		lda	<__dl + 0		; Add a page for the remainder.
		beq	.skip1
		inc	<__ax + 0
		bne	.skip1
		inc	<__ax + 1

.skip1:		ldx	<__ax + 0		; Limit the # of pages shown.
		cpx	#MAX_NUM_PAGES + 1
		lda	<__ax + 1
		sbc	#0
		bcc	.skip2
		ldx	#MAX_NUM_PAGES		; Too many pages!
.skip2:		stx	tos_num_pages

		ldx	#TOS_OK
.exit:		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_show_files - Show the current list of files/directories on this page.
;

tos_show_files:	stz	tos_num_files		; # of files on this page.

		lda	#3			; First line on page.
		sta	tos_tty_ypos
		stz	tos_tty_xpos
		stz	tos_tty_xyok

.nxt_entry:	jsr	f32_nxt_entry		; Get nxt name in the directory.
		beq	.got_entry

.rest:		tai	.space, f32_long_name, 63 * 2

.rest_loop:	lda	tos_tty_ypos		; Blank out the rest of the
		cmp	#23			; BRAM files in the box.
		bcs	.finished

		lda	#' '
		jsr	tos_tty_write
		tia	f32_long_name, VDC_DL, 63 * 2
		stz	tos_tty_xpos
		inc	tos_tty_ypos
		stz	tos_tty_xyok
		bra	.rest_loop

.space:		dw	$0120

.finished:	rts

.got_entry:	lda	[__bp]			; End of Directory?
		beq	.rest
		cmp	#$E5			; Empty entry?
		beq	.nxt_entry
		cmp	#'.'			; Skip ".." and "." entries.
		beq	.nxt_entry

.show_entry:	ldy	#DIR_Attr		; What type of entry is this?
		lda	[__bp],y
		bit	#ATTR_System + ATTR_Hidden
		bne	.nxt_entry		; Skip SYSTEM or HIDDEN files.
		and	#ATTR_Type_Mask
		beq	.valid_type		; It is a file.
		cmp	#ATTR_Directory
		bne	.nxt_entry		; It is a directory.

.valid_type:	pha				; Preserve file type.
		ldx	tos_num_files		; # of files on this page.
		sta	tos_file_type,x
		txa
		asl	a
		asl	a
		tax
		phx
		ldy	#DIR_FileSize
.copy_length:	lda	[__bp],y
		sta	tos_file_length,x
		inx
		iny
		cpy	#DIR_FileSize + 4
		bne	.copy_length
		plx
		ldy	#DIR_FstClusLO
		bsr	.cluster_word
		ldy	#DIR_FstClusHI
		bsr	.cluster_word

		ldx	f32_name_length		; Clamp name length to the
		cpx	#MAX_NAME_LENGTH	; size that we can scroll.
		bcc	.name_ok
		ldx	#MAX_NAME_LENGTH	; Name too long,

.name_ok:	pla				; Restore file type.
		beq	.skip_slash

		lda	#'/'                    ; Add a slash to directory
		sta	f32_long_name,x         ; names.
		inx
		stx	f32_name_length

.skip_slash:	lda	#' '			; Pad the name out to the
.pad_string:	cpx	#64			; screen width so it wipes
		bcs	.show_name		; out the previous contents.
		sta	f32_long_name,x
		inx
		bra	.pad_string

.show_name:	stz	f32_long_name,x		; Terminate the string.

		stz	tos_tty_xpos
		stz	tos_tty_xyok

		lda	#' '
		jsr	tos_tty_write

		PUTS	f32_long_name		; Write the name to VRAM.

		stz	tos_tty_xpos
		inc	tos_tty_ypos
		stz	tos_tty_xyok

		ldx	tos_num_files		; Store the name length.
		lda	f32_name_length
		sta	tos_name_length,x
		inx				; # of files on this page.
		stx	tos_num_files
		cpx	#FILES_PER_PAGE
		beq	.page_full
		jmp	.nxt_entry

.page_full:	rts

		;

.cluster_word:	lda	[__bp],y
		sta	tos_1st_cluster,x
		inx
		iny
		lda	[__bp],y
		sta	tos_1st_cluster,x
		inx
		rts



; ****************************************************************************
; ****************************************************************************
;
; tos_fastfwd_dir - Goto (n)th selectable *short* entry in the current dir.
;
; The filename for the entry is not copied to f32_long_name.
;
; This is used to skip to the entry prior to the one that you want to read
; the long name from and display next.
;
; Args __ax = Entry # to go to 1..65536, corresponding to index # 0..65535.
; Uses __bp = Pointer to directory entry in cache.
;
; It can also be used to count the number of selectable entries in a dir ...
;
;		lda	#$FF
;		sta	<__ax + 0
;		sta	<__ax + 1
;		jsr	tos_fastfwd_dir
;		lda	<__ax + 0
;		eor	#$FF		
;		sta	tos_file_count + 0
;		lda	<__ax + 1
;		eor	#$FF
;		sta	tos_file_count + 1
;

tos_fastfwd_dir:jsr	f32_rewind_dir		; Goto 1st cluster in directory.
		beq	.get_entry
		rts				; Return the error code.

.nxt_entry:	clc				; Inc directory pointer.
		lda	<__bp + 0
		adc	#32
		sta	<__bp + 0
		lda	<__bp + 1
		adc	#0
		sta	<__bp + 1

		cmp	#>(f32_cache_buf + 512) ; Any entries left in cache?
		bne	.tst_entry

		jsr	f32_nxt_sector		; Increment the sector.
		bcc	.get_entry		; Still within the cluster?

		jsr	f32_nxt_cluster		; Increment the cluster.
		beq	.get_entry

		rts				; Return the error code.

		; Refresh the directory cache.

.get_entry:	jsr	f32_load_cache		; Load a directory sector.
		beq	.loaded

		rts				; Return the error code.

.loaded:	stz	<__bp + 0		; Reset directory pointer.
		lda	#>f32_cache_buf
		sta	<__bp + 1

		lda	<__ax + 0
		ora	<__ax + 1
		beq	.finished

		; Test the current directory entry.

.tst_entry:	lda	[__bp]			; End of Directory?
		beq	.finished
		cmp	#$E5			; Empty entry?
		beq	.nxt_entry
		cmp	#'.'			; Skip ".." and "." entries.
		beq	.nxt_entry

		ldy	#DIR_Attr		; Skip SYSTEM or HIDDEN files.
		lda	[__bp],y
		bit	#ATTR_System + ATTR_Hidden
		bne	.nxt_entry
		and	#ATTR_Long_Mask		; Part of a long name?
		cmp	#ATTR_Long_Name
		beq	.nxt_entry
		and	#ATTR_Type_Mask
		beq	.valid			; Is it a file?
		cmp	#ATTR_Directory
		bne	.nxt_entry		; Is it a directory?

.valid:		lda	<__al			; Decrement counter of files
		bne	.skip			; and directories found.
		dec	<__ah
.skip:		dec	<__al
		bne	.nxt_entry
		lda	<__ah
		bne	.nxt_entry

		; The counter just hit zero!

.finished:	ldx	#SDC_OK
		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_load_hucard - Load a HuCard image from an open file into TED memory.
;
; Uses: f32_file_length = Length of file.
; Uses: f32_file_map    = Data buffer containing the file map.
; Uses: f32_file_pos    = Current fragment within the file map.
;
; N.B. The hack to skip the 1st sector on files with a header is
;      nasty, and relies on internal details of fat32.s, yuk!!!!
;

		.bss

tos_card_size:	ds	1

		.code

tos_load_hucard:lda	f32_file_length + 3	; Reject file length >= 16MB.
		bne	.bad_hucard

		clc				; Xvert length to block cnt.
		lda	f32_file_length + 0
		adc	#$FF
		lda	f32_file_length + 1
		adc	#$01
		tax
		lda	f32_file_length + 2
		adc	#$00
		ror	a
		sta	<__ax + 1		; Hi-byte of # blocks in file.
		sta	tos_card_size		; Remember HuCard size.
		sax
		ror	a
		sta	<__ax + 0		; Lo-byte of # blocks in file.

		bit	#$0E			; Reject HuCard not a multiple
		bne	.bad_hucard		; of 8KB (+ 512 byte header).

.check_min:	cpx	#$00			; Is the file size < 8KB?
		bne	.check_max
		cmp	#$10
		bcc	.bad_hucard		; Reject HuCard size < 8KB.

.check_max:	sec				; Is the file size > 2.5MB?
		sbc	#$01
		txa
		sbc	#$14
		bcc	.check_header		; Reject HuCard size > 2.5MB.

.bad_hucard:	ldx	#TOS_BAD_HUCARD		; Return error code.
		jmp	.close

.check_header:	bbr0	<__ax + 0, .starting_bank

		lda	f32_sec2cls_cnt		; Sanity Check that the math
		cmp	#2			; below won't fail. KRIKzz
		bcc	.bad_hucard		; enforces a min of 8 anyway.

		dec	<__ax + 0		; *HACK* odd # blocks if header.
		inc	f32_file_pos + 0	; *HACK* addr of 1st fragment.
		dec	f32_file_pos + 4	; *HACK* size of 1st fragment.

.starting_bank: lda	#$40			; Start at HuCard bank $00.

		cpx	#$03			; If this a 384KB HuCard, then
		bne	.set_1st_bank		; start at the mirrored 256KB.

		lda	#$60			; Start at HuCard bank $20.

.set_1st_bank:	tam3

		stz	<sdc_data_addr + 0
		lda	#$60
		sta	<sdc_data_addr + 1

		lda	#$4F			; Map in TED2 512KB bank 4.
		sta	sdc_data_bank
		sta	TED_BASE_ADDR + TED_REG_MAP

		jsr	f32_file_read		; Load the file into memory.
		bne	.close

		lda	tos_card_size		; Did we load a 384KB HuCard?
		cmp	#$03
		bne	.success

		lda	#$4F			; Map in TED2 512KB bank 4.
		sta	TED_BASE_ADDR + TED_REG_MAP

		jsr	wait_vsync		; Wait for the screen.

		php				; Disable interrupts.
		sei
		clc				; Copy bank $60-$7F to $40-$5F.
		lda	#$60
.copy_bank:	tam3
		adc	#$E0			; Assumes CC, will set C.
		tam2
		tii	$6000,$4000,$2000
		adc	#$21-1			; Assumes CS, will clr C.
		bpl	.copy_bank

		lda	VDC_SR			; Clear any pending VDC irq.
		cla				; Restore TED hardware bank.
		tam2
		plp				; Restore interrupts.

.success:	ldx	#TOS_OK

.close:		jmp	f32_close_file		; Close the file & set N & Z.



; ***************************************************************************
; ***************************************************************************
;
; tos_fix_region - Search for and patch the TurboGrafx region test.
;
; Takes 219 cycles if the code is found, less if it is not.
;

tos_fix_region: lda	#$4F			; Map in TED2 512KB bank 4.
		sta	TED_BASE_ADDR + TED_REG_MAP
		lda	#$40			; Select bank 0 of the HuCard
		tam3				; image in TED2 memory.

		clc				; Try 6 bytes after the reset
		lda	#6			; vector.
		adc	$7FFE
		sta	<__bp + 0
		cla
		adc	$7FFF
		and	#$1F
		ora	#$60
		sta	<__bp + 1

		bsr	.find_start		; Test for the code sequence.

		lda	#<$6391			; Try $E391 in HuCard bank 0.
		sta	<__bp + 0
		lda	#>$6391
		sta	<__bp + 1

.find_start:	lda	[__bp]			; 1st byte of region code?
		cmp	#$AD
		bne	.finished
		cly
.test_loop:	iny				; Check for the rest of the
		lda	[__bp],y		; code sequence.
		cmp	.sequence - 1,y
		bne	.finished
		cpy	#$0A
		bne	.test_loop

.patch:		lda	#$80			; Patch in BRA instruction.
		ldy	#$05
		sta	[__bp],y
.finished:	rts

.sequence:	db	$00,$10,$29,$40,$F0,$0C,$A9,$90
		db	$53,$04



; ***************************************************************************
; ***************************************************************************
;
; tos_exec_hucard - Execute the HuCard that we've got loaded in memory.
;
;   Bit 0 : cfg_regs_oe_off : Bank 0 mapping (0=FPGA, 1=RAM)
;   Bit 1 : cfg_regs_we_off : FPGA Registers (0=access, 1=locked)
;   Bit 2 : cfg_usb_boot    : unknown (leave zero)
;   Bit 3 : cfg_skip_eep    : Normally set by KRIKzz : unknown (set to 1)
;   Bit 4 : cfg_ram_we_off  : RAM Read-Only? (0=writable, 1=read-only)
;   Bit 5 :		    : SF2 2MB mapper (0=disabled, 1=enabled) Only $1ff0-$1ff3.
;   Bit 6 :		    : unknown (leave zero)
;   Bit 7 :		    : unknown (leave zero)
;

tos_exec_hucard:
;		jsr	clear_screen

		lda	#$4F			; Map in TED2 512KB bank 4.
		sta	TED_BASE_ADDR + TED_REG_MAP
		lda	#$40			; Select bank 0 of the HuCard
		tam3                            ; image in TED2 memory.

		jsr	tos_fix_region		; Fix TurboGrafx to run on PCE.

		; Check for HuCards that need special handling.

		lda	#<.hucard_tbl		; Table of signatures of images
		sta	<__al			; that need special handling.
		lda	#>.hucard_tbl
		sta	<__ah

		ldx	#5-1			; # of signatures to search for.
		
.test_card:	ldy	#15-1

		lda	[__ax],y		; Does the HuCard size match?
		dey
		cmp	tos_card_size
		bne	.fail

		lda	[__ax],y		; Get ptr to card's test area.
		dey
		sta	<__bh
		lda	[__ax],y
		dey
		sta	<__bl

.test_loop:	lda	[__ax],y		; Compare signature with 12
		cmp	[__bx],y		; bytes of data from the card.
		bne	.fail
		dey
		bpl	.test_loop

.found:		txa				; Found a matching HuCard!
		asl	a
		tax
		jsr	.vector_jmp		; Call a routine to provide
		bra	.execute		; the special handling.

.fail:		clc				; Not a match, try the next
		lda	<__al			; signature.
		adc	#15
		sta	<__al
		bcc	.test_next
		inc	<__ah

.test_next:	dex
		bpl	.test_card

		; If it's not a special case HuCard, then run it as a ROM.

		ldy	#%00011011		; FPGA=off+locked, RAM=readonly.

.execute:	sei				; Turn the BKG and SPR off.
		stz	<vdc_crl
		lda	irq_cnt
		cli
.wait:		cmp	irq_cnt			; Wait for BKG and SPR off to
		beq	.wait			; take effect.

		sei				; Definitely disable interrupts!

		lda	#$4F			; Set 512KB mapping for HuCard.
		sta	TED_BASE_ADDR + TED_REG_MAP
		lda	#$40
		tam3

;		ldx	$6000 + TED_REG_CFG	; Remember byte at this addr.

		lda	#$F8			; Map in regular PCE RAM.
		tam1

		tii	.trampoline, $2200, (.trampoline_end - .trampoline)

		jmp	$2200			; Exec the trampoline in RAM.

		; This stub gets copied to PCE RAM and run from there.

.trampoline:	lda	#%00011000		; FPGA=on+writable, RAM=readonly.
		sta	TED_BASE_ADDR + TED_REG_CFG

		lda	#$04			; Set 512KB mapping for HuCard.
		sta	TED_BASE_ADDR + TED_REG_MAP

		; Set HuCard-specific config.

		sty	TED_BASE_ADDR + TED_REG_CFG

		; Repair RAM-writable HuCard.

;		stx	TED_BASE_ADDR + TED_REG_CFG

		cla				; Put Bank $00 in MPR7.
		tam7

		csl				; Simulate a reset.
		jmp	[$FFFE]

.trampoline_end:

		;

.vector_jmp:	jmp	[.vector_tbl,x]

.vector_tbl:	dw	.found_stft2		; #0
		dw	.found_romram		; #1
		dw	.found_tennokoe		; #2
		dw	tos_patch_usa30		; #3 - Call function to patch.
		dw	tos_patch_jpn30		; #4 - Call function to patch.

		; Tennokoe is a ROMRAM HuCard, and needs writable RAM.
		; Populous is a ROMRAM HuCard, and needs writable RAM.

.found_tennokoe:jsr	tos_patch_tbank		; Patch Tennokoe Bank HuCard.
		jsr	tos_load_tbank		; Load Tennokoe Bank SRAM file.
		inc	tos_tennokoe		; Signal that it needs saving.

.found_romram:	ldy	#%00001011		; FPGA=off+locked, RAM=writable.
		rts

		; Street Fighter II needs the TED2's SF2 mapper enabled.

.found_stft2:	ldy	#%00111011		; FPGA=off+locked, RAM=readonly,
		rts				; SF2=enabled.

.hucard_tbl:	; #4 - 256KB System Card 3.0 PCE @ Location $1FF4
		db	$68,$80,$36,$E7,$70,$E8,$B3,$E6,$A9,$E6,$F3,$E0
		dw	$7FF4
		db	$02			; 256KB size.

		; #3 - 256KB System Card 3.0 TGX @ Location $1FF4
		db	$68,$80,$4F,$E7,$89,$E8,$CC,$E6,$C2,$E6,$F3,$E0
		dw	$7FF4
		db	$02			; 256KB size.

		; #2 - 128KB Tennokoe Bank @ Location $1FD0
		db	$93,$56,$82,$CC,$90,$BA,$83,$6F,$83,$93,$83,$4E
		dw	$7FD0
		db	$01			; 128KB size.

		; #1 - 512KB Populous @ Location $1F25
		db	" POPULOUS   "
		dw	$7F25
		db	$04			; 512KB size.

		; #0 - 2.5MB Street Fighter II @ Location $1FF4
		db	$20,$48,$BC,$E1,$C1,$E1,$60,$3C,$BD,$E1,$00,$E0
		dw	$7FF4
		db	$14			; 2.5MB size.



; ****************************************************************************
; ****************************************************************************
;
; tos_set_hilite - Set the BAT for the tiles on the current line to hilite.
; tos_clr_hilite - Set the BAT for the tiles on the current line to normal.
;

tos_set_hilite:	stz	tos_hilite_idx

		bsr	selected_line
		ldx	#127
		lda	#$10
		ldy	#'>'
		sty	VDC_DL
		tsb	VDC_DH
.loop:		ldy	VDC_DL
		sty	VDC_DL
		tsb	VDC_DH
		dex
		bne	.loop
		rts

tos_clr_hilite:	bsr	selected_line
		ldx	#127
		lda	#$10
		ldy	#' '
		sty	VDC_DL
		tsb	VDC_DH
.loop:		ldy	VDC_DL
		sty	VDC_DL
		trb	VDC_DH
		dex
		bne	.loop
		rts

selected_line:	php
		sei
		ldx	tos_cur_depth
		lda	tos_cur_file,x
		inc	a
		inc	a
		inc	a
		lsr	a
		tax
		cla
		ror	a
		st0	#VDC_MARR
		sta	VDC_DL
		st0	#VDC_MAWR
		sta	VDC_DL
		st0	#VDC_MARR
		stx	VDC_DH
		st0	#VDC_MAWR
		stx	VDC_DH
		vreg	#VDC_VRR
		plp
		rts



; ****************************************************************************
; ****************************************************************************
;
; tos_pulse_color - Color cycle the palette index used for highlighted items.
;

tos_pulse_color:lda	tos_hilite_idx
		inc	a
		and	#$7F
		sta	tos_hilite_idx
		lsr	a
		lsr	a
		lsr	a
		tax
		lda	tos_hilite_tbl,x
		asl	a
		asl	a
		asl	a
		ora	tos_hilite_tbl,x
		asl	a
		asl	a
		asl	a
		ora	tos_hilite_tbl,x
		tax
		cla
		rol	a

		ldy	#$1E			; Hilite for 8x8 text.
		sty	VCE_CTA + 0
		stz	VCE_CTA + 1
		stx	VCE_CTW + 0
		sta	VCE_CTW + 1

		ldy	#$1A			; Hilite for 8x12 text.
		sty	VCE_CTA + 0
		stz	VCE_CTA + 1
		stx	VCE_CTW + 0
		sta	VCE_CTW + 1

		rts

		if	1
tos_hilite_tbl:	db	6,6,6,6,6,6,6,6
		db	6,6,6,5,4,3,4,5
		else

tos_hilite_tbl:	db	6,6,6,6,6,6,6,6
		db	6,5,4,3,2,3,4,5

tos_hilite_tbl:	db	7,7,7,7,7,7,7,7
		db	7,7,7,6,5,4,5,6

tos_hilite_tbl:	db	7,7,7,7,7,7,7,7
		db	7,7,7,7,7,6,5,6
		endif

		.bss

tos_hilite_idx:	ds	1

		.code



; ***************************************************************************
; ***************************************************************************
;
; tos_init_crc16 - Initialize the CRC16 lookup tables in RAM.
;
; By Paul Guertin. See http://6502.org/source/integers/crc.htm
;
; CCITT CRC-16 as used by ZMODEM/YMODEM/XMODEM/etc.
;
; Takes 60942 cycles to create the tables.
;
; name    polynomial  initial val
; CCITT         1021         FFFF
; XModem        1021         0000
;

crc16_tbl_lo:	=	$3700			; 256-bytes
crc16_tbl_hi:	=	$3800			; 256-bytes

tos_init_crc16: clx				; X counts from 0 to 255
.byte_loop:	cla				; Lo 8 bits of the CRC-16
		stx	<__al			; Hi 8 bits of the CRC-16
		ldy	#8			; Y counts bits in a byte

.bit_loop:	asl	a
		rol	<__al			; Shift CRC left
		bcc	.no_add			; Do nothing if no overflow
		eor	#$21			; else add CRC-16 polynomial $1021
		pha				; Save low byte
		lda	<__al			; Do high byte
		eor	#$10
		sta	<__al
		pla				; Restore low byte
.no_add:	dey
		bne	.bit_loop		; Do next bit

		sta	crc16_tbl_lo,x		; Save CRC into table, low byte
		lda	<__al			; then high byte
		sta	crc16_tbl_hi,x
		inx
		bne	.byte_loop		; Do next byte
		rts

		;

updcrc16:	eor	<__ax + 1		; Quick CRC computation with lookup tables
		tax
		lda	<__ax + 1
		eor	crc16_tbl_hi,x
		sta	<__ax + 1
		lda	crc16_tbl_lo,x
		sta	<__ax + 0
		rts

; Example: Computing the CRC-16 of 256 bytes of data in $1000-$10FF.

		jsr	tos_init_crc16
		ldy	#$FF
		sty	<__ax + 0
		sty	<__ax + 1
		iny
.loop:		lda	$1000,y
		jsr	updcrc16
		iny
		bne	.loop
		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_init_crc32 - Initialize the CRC32 lookup tables in RAM.
;
; By Paul Guertin. See http://6502.org/source/integers/crc.htm
;
; CRC-32 as used by ZIP/PNG/ZMODEM/etc.
;
; Takes 112654 cycles to create the tables.
;

crc32_tbl_b0	=	$3900			; 256-bytes
crc32_tbl_b1	=	$3A00			; 256-bytes
crc32_tbl_b2	=	$3B00			; 256-bytes
crc32_tbl_b3	=	$3C00			; 256-bytes

tos_init_crc32: clx				; X counts from 0 to 255
.byte_loop:	cla				; A contains the high byte of the CRC-32
		sta	<__ax + 2		; The other three bytes are in memory
		sta	<__ax + 1
		stx	<__ax + 0
		ldy	#8			; Y counts bits in a byte
.bit_loop:	lsr	a			; The CRC-32 algorithm is similar to CRC-16
		ror	<__ax + 2		; except that it is reversed (originally for
		ror	<__ax + 1		; hardware reasons). This is why we shift
		ror	<__ax + 0		; right instead of left here.
		bcc	.no_add			; Do nothing if no overflow
		eor	#$ED			; else add CRC-32 polynomial $EDB88320
		pha				; Save high byte while we do others
		lda	<__ax + 2
		eor	#$B8			; Most reference books give the CRC-32 poly
		sta	<__ax + 2		; as $04C11DB7. This is actually the same if
		lda	<__ax + 1		; you write it in binary and read it right-
		eor	#$83			; to-left instead of left-to-right. Doing it
		sta	<__ax + 1		; this way means we won't have to explicitly
		lda	<__ax + 0		; reverse things afterwards.
		eor	#$20
		sta	<__ax + 0
		pla				; Restore high byte.
.no_add:	dey
		bne	.bit_loop		; Do next bit
		sta	crc32_tbl_b3,x		; Save CRC into table, high to low bytes.
		lda	<__ax + 2
		sta	crc32_tbl_b2,x
		lda	<__ax + 1
		sta	crc32_tbl_b1,x
		lda	<__ax + 0
		sta	crc32_tbl_b0,x
		inx
		bne	.byte_loop		; Do next byte
		rts

updcrc32:	eor	<__ax + 0		; Quick CRC-32 computation with lookup tables
		tax
		lda	<__ax + 1
		eor	crc32_tbl_b0,x
		sta	<__ax + 0
		lda	<__ax + 2
		eor	crc32_tbl_b1,x
		sta	<__ax + 1
		lda	<__ax + 3
		eor	crc32_tbl_b2,x
		sta	<__ax + 2
		lda	crc32_tbl_b3,x
		sta	<__ax + 3
		rts

; Example: Computing the CRC-32 of 256 bytes of data in $1000-$10FF.

		jsr	tos_init_crc32
		ldy	#$FF
		sty	<__ax + 0
		sty	<__ax + 1
		sty	<__ax + 2
		sty	<__ax + 3
		iny
.loop:		lda	$1000,y
		jsr	updcrc32
		iny
		bne	.loop
		ldx	#3
.compl:		lda	<__ax,x
		eor	#$FF
		sta	<__ax,x
		dex
		bpl	.compl
		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_draw_box -
;
; Args: A    = Box type.
; Args: __al = Box width  (minimum 2).
; Args: __ah = Box height (minimum 2).
;

tos_draw_box:	sta	tos_tty_xyok		; Xvert box type to index.
		asl	a
		asl	a
		asl	a
		adc	tos_tty_xyok
		tay				; Index into .box_edge_tbl.

		stz	tos_tty_xyok

.box_top_line:	bsr	.box_line_lhs		; Draw top edge of box.

		ldx	<__ah			; Box height.
		dex
		dex
		dex
		bmi	.box_btm_line		; Skip middle if height < 3.

.box_mid_line:	phx
		bsr	.box_line_lhs		; Draw mid line of box.
		plx
		dex
		bmi	.box_btm_line
		dey				; Rewind .box_edge_tbl index
		dey				; for repeat of mid line.
		dey
		bra	.box_mid_line

.box_btm_line:					; Draw btm edge of box.

.box_line_lhs:	lda	tos_tty_xpos
		pha
		stz	tos_tty_xyok

		lda	.box_edge_tbl,y
		iny
		jsr	tos_tty_write

		lda	.box_edge_tbl,y
		iny
		phy

		ldy	<__al			; Box width.
		dey
		dey
		dey
		bmi	.box_line_rhs

		tax				; Center character zero?
		beq	.box_no_center

		jsr	tos_tty_write
.box_line_mid:	dey
		bmi	.box_line_rhs
		stx	VDC_DL
		sta	VDC_DH
		bra	.box_line_mid

.box_no_center:	tya
		sec
		adc	tos_tty_xpos
		sta	tos_tty_xpos
		stz	tos_tty_xyok

.box_line_rhs:	ply
		lda	.box_edge_tbl,y
		iny
		jsr	tos_tty_write

		pla
		sta	tos_tty_xpos
		inc	tos_tty_ypos
		stz	tos_tty_xyok
		rts

.box_edge_tbl:	db	$10,$11,$12		; 0 : Thick edge, no center.
		db	$13,$00,$14
		db	$15,$16,$17

		db	$20,$20,$20		; 1 : Blank Box.
		db	$20,$20,$20
		db	$20,$20,$20
