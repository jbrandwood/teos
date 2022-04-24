; ***************************************************************************
; ***************************************************************************
;
; menu_init.s
;
; TEOS Menu Screens
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
;
;

wait_for_key:	stz	joytrg
.wait:		lda	joytrg
		and	#JOY_B1 + JOY_B2
		beq	.wait
		rts



; ***************************************************************************
; ***************************************************************************
;
;
;

msg_wait_init:	db	"%>%p5"
		db	$0C
		db	" INITIALIZING...%<%p0"
		db	$0A,$0A,$0A,0

tos_menu_init:	ldy	#4			; Decompress the 1st font file.
		jsr	tos_decompress
		jsr	upload_font8x8

		ldy	#8			; Decompress the 2nd font file.
		jsr	tos_decompress
		jsr	upload_font8x16

		jsr	tos_init_crc16		; Create CRC16 lookup table.
		jsr	tos_init_crc32		; Create CRC32 lookup table.

		PUTS	msg_wait_init

;		jsr	show_mpr
;		inc	tos_tty_ypos

		php

		TED_CFG_FPGA

		; Save Tennokoe Bank SRAM?

		jsr	f32_mount_vol		; Init SDcard and mount drive.
		stx	$2F00

		cpx	#$00
		beq	.stage1_ok

		PUTS	fat32_nomount
.hang:		jmp	.hang

.stage1_ok:	PUTS	fat32_mounted

		; Save Tennokoe Bank SRAM?

		lda	tos_tennokoe		; Did we just run Tennokoe Bank?
		beq	.skip_tbank
		stz	tos_tennokoe
		jsr	tos_save_tbank		; Save the Tennokoe Bank SRAM.
.skip_tbank:	nop

		;

		if	0

		PUTS	msg_press_a_key

		jsr	wait_for_key

		jsr	mb1_read_info

		PUTS	msg_press_a_key

		jsr	wait_for_key

		endif

		;

		if	0

		PUTS	msg_wait_init

		lda	#BRAM_BANK		; Set up the directory bank.
		tam3

		lda	#<$6000
		ldx	#>$6000
		jsr	show_page

		PUTS	msg_press_a_key

		jsr	wait_for_key

		PUTS	msg_wait_init

		lda	#BRAM_BANK + 15		; Set up the directory bank.
		tam3

		lda	#<$7F80
		ldx	#>$7F80
		jsr	show_page

		PUTS	msg_press_a_key

		jsr	wait_for_key

		PUTS	msg_wait_init

		lda	#BRAM_BANK + 16		; Set up the directory bank.
		tam3

		lda	#<$6000
		ldx	#>$6000
		jsr	show_page

		PUTS	msg_press_a_key

		jsr	wait_for_key

		endif

		if	0

		lda	#BRAM_BANK
		tam3

		jsr	bm_bram_to_bank

		PUTS	msg_wait_init

		lda	#<$6000
		ldx	#>$6000
		jsr	show_page

		PUTS	msg_press_a_key

		jsr	wait_for_key

		endif

;		jsr	tos_save_test
;		beq	.test_ok
;
;		PUTS	test_failed
;.hang2:		jmp	.hang2
;
;.test_ok:	PUTS	test_saved
;
;		lda	#<sdc_block_cnt
;		ldx	#>sdc_block_cnt
;		jsr	show_8bytes

;		jsr	wait_for_key
;		jsr	tos_load_test

		; If emulating, then use the volume ID as the SDcard CID.

		if	REALHW
		else
		tii	BS_BootSig32, sdc_cid_value, 16
		endif

		;

;.stage2:	jsr     wait_for_key

		jmp	tos_hucard_menu

msg_press_a_key:db	$0A
		db	" Press%p1"
		db	28,29
		db	"%p0 to continue.",$0A,0

test_failed:	db	" Test write failed!",$0A,0
test_saved:	db	" Test write OK!",$0A,0

fat32_mounted:	db	" FAT32 partition mounted.",$0A,0
fat32_nomount:	db	" FAT32 partition mount failed!",$0A,0



; ***************************************************************************
; ***************************************************************************
;
;
;

show_mpr:	tma0
		sta	mpr + 0
		tma1
		sta	mpr + 1
		tma2
		sta	mpr + 2
		tma3
		sta	mpr + 3
		tma4
		sta	mpr + 4
		tma5
		sta	mpr + 5
		tma6
		sta	mpr + 6
		tma7
		sta	mpr + 7

		lda	#<mpr
		ldx	#>mpr
		jmp	show_8bytes

		.bss

mpr:		ds	8

		.code



; ***************************************************************************
; ***************************************************************************
;
; show_page - show a 16-bye hex dump
;

show_page:	ldy	#16
.loop:		phy
		bsr	show_8bytes
		clc
		adc	#$08
		bcc	.skip
		inx
.skip:		ply
		dey
		bne	.loop
		rts

show_8bytes:	php
		sei

		pha
		phx
		sta	<view + 0
		stx	<view + 1

		cly

.line_loop:	st0	#VDC_MAWR

		lda	tos_tty_ypos
		lsr	a
		tax
		cla
		ror	a
		sta	VDC_DL
		stx	VDC_DH

		st0	#VDC_VWR
		st1	#$20
		st2	#$01

		phy

		ldx	#1
.addr_loop:	lda	<view,x
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		ora	#$30
		cmp	#$3A
		bcc	.addr_lo
		adc	#6
.addr_lo:	sta	VDC_DL
		st2	#$01

		lda	<view,x
		and	#$0F
		ora	#$30
		cmp	#$3A
		bcc	.addr_hi
		adc	#6
.addr_hi:	sta	VDC_DL
		st2	#$01
		dex
		bpl	.addr_loop

		st1	#$3A
		st2	#$01

		st1	#$20
		st2	#$01

.byte_loop:	lda	[view],y
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		ora	#$30
		cmp	#$3A
		bcc	.skip_lo
		adc	#6
.skip_lo:	sta	VDC_DL
		st2	#$01

		lda	[view],y
		and	#$0F
		ora	#$30
		cmp	#$3A
		bcc	.skip_hi
		adc	#6
.skip_hi:	sta	VDC_DL
		st2	#$01

		iny
		tya
		bit	#$00		; $01 for 16-bits-per-block
		bne	.skip_space

		st1	#$20
		st2	#$01

.skip_space:	bit	#$07		; $0F for 16-bytes-per-line
		bne	.byte_loop

		ply

.char_loop:	lda	[view],y
		cmp	#$20
		bcc	.non_ascii
		cmp	#$7F
		bcc	.show_char
.non_ascii:	lda	#'.'
.show_char:	sta	VDC_DL
		st2	#$01

		iny
		tya
		bit	#$07		; $0F for 16-bytes-per-line
		bne	.char_loop

		inc	tos_tty_ypos
		cpy	#$08		; $10 for 16-bytes-per-line
		beq	.finished

		jmp	.line_loop

.finished:	lda	<vdc_reg
		sta	VDC_AR

		plx
		pla

		plp
		rts


; ***************************************************************************
; ***************************************************************************
;
; tos_tbank_file - Select the TENNOKOE.BIN file.
;
; Args: tos_bram_slot = Pointer to directory entry in cache.
; Uses: __bp = Pointer to directory entry in cache.
;
; Returns: X = F32_OK (and Z flag) or an error code.
;

tos_tbank_file: jsr	tos_chdir_tbed		; Select "/TBED/" directory.
		bne	.exit

		lda	#<.tennokoe_name	; Locate the named file in
		ldy	#>.tennokoe_name	; the current directory.
		sta	<__ax + 0
		sty	<__ax + 1
		jsr	f32_find_name
		bne	.exit

		jsr	f32_open_file		; Open file and create
		bne	.exit			; fragment map for I/O.

		ldx	#TOS_WRONG_SIZE		; Return error code.

		lda	f32_file_length + 1	; Check that the file is 8KB.
		cmp	#$20
		bne	.close
		lda	f32_file_length + 3
		ora	f32_file_length + 2
		ora	f32_file_length + 0
		bne	.close

		lda	#8192 / 512
		sta	<__ax + 0		; Lo-byte of # blocks in file.
		stz	<__ax + 1		; Hi-byte of # blocks in file.

		lda	#$0F			; Map in TED2 512KB bank 0.
		sta	TED_BASE_ADDR + TED_REG_MAP
		sta	sdc_data_bank

		stz	<sdc_data_addr + 0	; Load into $6000-$7FFF.
		lda	#$60
		sta	<sdc_data_addr + 1

		lda	#$40			; RAMROM HuCard bank $40.
		tam3

		ldx	#TOS_OK

.exit:		txa				; Set flags for result.
		rts

.close:		jmp	f32_close_file		; Close the file & set N & Z.

.tennokoe_name: db	"TENNOKOE.BIN",0



; ***************************************************************************
; ***************************************************************************
;
; tos_load_tbank - Load the TENNOKOE.BIN file into HuCard bank $40.
;

tos_load_tbank: jsr	tos_tbank_file		; Select "/TBED/TENNOKOE.BIN".
		bne	.failed

		PUTS	.loading

		jsr	f32_file_read		; Load the file into HuCard.

		ldy	#<.load_ok
		lda	#>.load_ok

		jsr	f32_close_file		; Close the file & set N & Z.
		beq	.finished

.failed:	ldy	#<.load_bad
		lda	#>.load_bad

.finished:	phx				; Preserve result code.
		sxy
		jsr	tos_print_msg
		plx				; Restore result code.

		lda	#$0F			; Map in TED2 512KB bank 0.
		sta	TED_BASE_ADDR + TED_REG_MAP
		lda	#$40			; RAMROM HuCard bank $40.
		tam3

		tma2				; Use the BRAM bank as storage
		pha				; to see if the user makes any
		lda	#BRAM_BANK		; changes to the Tennokoe Bank
		tam2				; SRAM contents.

		txa				; Was the SRAM loaded from
		beq	.copy			; SD card?

		tai	tos_empty, $6000, $2000 ; Clear SRAM.

.copy:		tii	$6000, $4000, $2000	; Copy SRAM to see changes.

		pla				; Restore TED2 bank.
		tam2

		lda	#$4F			; Set 512KB HuCard mapping.
		sta	TED_BASE_ADDR + TED_REG_MAP
		lda	#$40
		tam3

		phx
		PUTS	msg_press_a_key
		jsr	wait_for_key
		plx

		rts

.loading:	db	" Loading Tennokoe Bank SRAM from SD ...",$0A,$0A,0
.load_ok:	db	" SRAM loaded OK!",$0A,$0A
		db	" When you have finished, please remember to press",$0A
		db	" the RESET button on your Turbo EverDrive so that",$0A
		db	" your file chest changes are saved to the SD card!",$0A,$0A,0
.load_bad:	db	" Unable to read file!",$0A,0



; ***************************************************************************
; ***************************************************************************
;
; tos_save_tbank - Save the TENNOKOE.BIN file from HuCard bank $40.
;

tos_save_tbank: lda	#$0F			; Map in TED2 512KB bank 0.
		sta	TED_BASE_ADDR + TED_REG_MAP
		lda	#$40			; RAMROM HuCard bank $40.
		tam3

		tma2				; Map in the copy of the SRAM
		pha				; that we made before running
		lda	#BRAM_BANK		; the HuCard.
		tam2

		lda	#$40			; Did the Tennokoe Bank HuCard
		sta	.self_mod1 + 2		; user change the contents of
		lda	#$60			; the SRAM?
		sta	.self_mod2 + 2
		clx
.loop:
.self_mod1:	lda	$4000,x			; Compare the 8KB of SRAM.
.self_mod2:	cmp	$6000,x
		bne	.save_sram
		inx
		bne	.loop
		inc	.self_mod1 + 2
		inc	.self_mod2 + 2
		bpl	.loop

		pla				; Don't save the SRAM if it
		tam2				; is unchanged.
		txa				; Return TOS_OK.
		bra	.finished

.save_sram:	pla				; Save the Tennokoe Bank SRAM
		tam2				; to SD card.

.retry:		PUTS	msg_wait_init		; Inform the user.
		PUTS	.msg_saving

		jsr	tos_tbank_file		; Select "/TBED/TENNOKOE.BIN".
		bne	.failed

		if	0
		ldx	#F32_ERR_DSK_RD		; For testing the messages.
		else
		jsr	f32_file_write		; Save the file from HuCard.
		endif

		jsr	f32_close_file		; Close the file & set N & Z.
		beq	.success

		;

.failed:	PUTS	.msg_failed		; Warn the user.

.wait1_input:	stz	joytrg			; Wait for input.
.wait1_loop:	jsr	wait_vsync
		lda	joytrg
		beq	.wait1_loop

		bit	#JOY_RUN
		bne	.danger
		bit	#JOY_B1
		beq	.wait1_input

.want_retry:	PUTS	msg_wait_init
		bra	.retry

		;

.danger:	PUTS	.msg_danger

.wait2_input:	stz	joytrg
.wait2_loop:	jsr	wait_vsync
		lda	joytrg
		beq	.wait2_loop

		bit	#JOY_B1
		bne	.want_retry

		bit	#JOY_SEL
		beq	.wait2_input
		bra	.finished

		;

.success:	PUTS	.msg_save_ok
		PUTS	msg_press_a_key

.wait0_input:	stz	joytrg			; Wait for input.
.wait0_loop:	jsr	wait_vsync
		lda	joytrg
		beq	.wait0_loop

		bit	#JOY_B1
		beq	.wait0_input
		bra	.finished

.finished:	phx

		lda	#$4F			; Set 512KB mapping for HuCard.
		sta	TED_BASE_ADDR + TED_REG_MAP
		lda	#$40
		tam3

		plx

		rts

.msg_saving:	db	" Saving Tennokoe Bank SRAM to SD ...",$0A,$0A,0
.msg_save_ok:	db	" SRAM saved OK!",$0A,0

.msg_failed:	db	"%p5"
		db	"%y",24

;		db	"----------------------------------------"
		db	"  "
		db	28,29
		db	"%p6:Retry saving the SRAM to SD card%p5",$0A
		db	"    RUN%p6:Continue without saving SRAM",$0A

		db	"%p0"
		db	"%y",5
		db	"   FAILED TO SAVE SRAM TO SD CARD!",$0A,$0A
;		db	"----------------------------------------"
		db	" If you continue, then you will lose",$0A
		db	" any changes that you just made to",$0A
		db	" the contents of the Tennokoe Bank",$0A
		db	" file chests.",$0A,0

.msg_danger:	db	$0A,$0A
		db	" Are you really sure?",$0A,$0A
		db	" Please press %p1SEL%p0 to confirm that you",$0A
		db	" want to lose all of the changes that",$0A
		db	" you made to the Tennokoe Bank SRAM!",$0A
		db	"%y",25
		db	"%p5"
		db	"    SEL%p6:Continue without saving SRAM",$0A,0

;		db	" you made while in the Tennokoe Bank!",$0A
