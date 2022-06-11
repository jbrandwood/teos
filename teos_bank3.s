; ***************************************************************************
; ***************************************************************************
;
; BANK $03 - INTERRUPT HANDLERS AND UTILITY CODE
;
; Copyright John Brandwood 2019-2022.
;
; Distributed under the Boost Software License, Version 1.0.
; (See accompanying file LICENSE_1_0.txt or copy at
;  http://www.boost.org/LICENSE_1_0.txt)
;
; ***************************************************************************
; ***************************************************************************



		bank	3



; ***************************************************************************
; ***************************************************************************
;
; System Card Patch for the IPL boot-loader to check for game-specific fixes
; that are needed when running the System Card in writable RAM on the TED2.
;
; This is copied to bank 31 of the System Card, just below the data block
; that contains the boot-loader that the System Card compares to the boot
; loader on the CD in order to validate the disc.
;
; When check is performed, bank 31 is mapped into MPR6 (i.e. $C000-$DFFF).
;
; This is assembled at the same offset ($Dxxx-$DADF) in this bank so that
; we don't have to mess around with changing all of the memory addresses.
;

		;
		; Check if the IPL of the CD (loaded at $3000) matches
		; any of the games that need to be fixed.
		;

		org	$DA58

fix_bootload:	ldx	#(game_fix - game_list)

.loop0:		ldy	#21		; Compare IPL program name.
.loop1:		dex
		lda	$306A,y
		cmp	game_list,x
		bne	.wrong_game
		dey
		bpl	.loop1

		ldy	#9		; Compare IPL boot location.
.loop2:		dex
		lda	$3000,y
		cmp	game_list,x
		bne	.wrong_game
		dey
		bpl	.loop2

		txa			; Found a matching game!
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		tax
		jmp	[game_fix,x]	; Game-specific patch for boot loader.

.wrong_game:	txa			; Try the next game.
		and	#$E0
		tax
		bne	.loop0

		jmp	$2B26		; Goto the original code.

		;
		; Identifiers for a maximum of 8 Game-Specific Patches.
		;
		; 32-bytes per game ...
		; ... 10 bytes of IPL boot location. ($3000..$3009)
		; ... 22 bytes of IPL program name.  ($306A..$307F)
		;

game_list:	.db  $00, $00, $02, $03, $00, $30, $00, $3A, $00, $01
		.db  "EIYU_DENSETSU_2 921107"

game_fix:	.dw	fix_dslayer2

		;
		; Fix for Dragon Slayer II.
		;
		; The game is testing for the extended RAM on a Development
		; System and enabling some debugging code if it finds it.
		;
		; Having RAM in bank $4C fools the game into enabling that
		; code, which then crashes the game just after the intro.
		;

fix_dslayer2:	tii	.patch,$2CD1,(.done - .patch)
		jmp	$2B26		; Execute the boot loader in RAM.

.patch:		bra	.code		; This needs to be at $2CD1.
		nop
		rts			; This needs to be at $2CD4.

.code:		ldy	#15		; Start of the the check code.
.loop:		lda	$412F,y
		cmp	$2CD1+(.test - .patch),y
		bne	.exec		; Do not patch if different.
		dey
		bpl	.loop

		lda	#$80		; Patch "BNE $415F" instruction
		sta	$4137		; into	"BRA $415F".

.exec:		jmp	[$2007]		; Execute the game's boot program.

		; This is the game's boot code that is expected (@ $412F).

.test:		db	$B2, $10, $49, $FF, $92, $10, $D2, $10
		db	$D0, $26, $F7, $20, $AE, $11, $E0, $AD

.done:

		;
		; Make sure that the game-specific patches do not overrun
		; the original ROM contents.
		;

		ds	$DAE0-*		; Generate error if too long.

end_bootload:



; ***************************************************************************
; ***************************************************************************
;
; System Card Patch for ex_memopen so that the TED2 reports that external
; SCD RAM is present on the HuCard, which stops the code from enabling the
; built-in SCD RAM in a Duo/SuperCDROM, preventing the potentially damaging
; bus-fighting that would occur if both devices are active at the same time.
;
; This also includes a patch for ex_scrsiz that fixes the damage that the
; "Gate of Thunder" game does by writing garbage to $FFF5.
;
; Finally, there is also a patch to switch the TED2 into RAM mode when the
; System Card is booted.
;
; This code is position-independant, so we don't need to fixup any addresses.
;

		org	$FAE0

fix_memopen:	lda	#$68		; Return that 192KB of external SCD RAM
		ldx	#$03		; exists and thus avoid unlocking the
		clc			; internal SCD RAM chip.
		rts

fix_scrsiz:	pha			; Repair "Gate of Thunder" damage.
		lda	#$80
		sta	$FFF5
		pla
		sta	$0002		; Exec ex_scrsiz instr that was patched.
		rts

fix_unlock:	php			; Disable interrupts for the next bit.
		sei
		lda	#$A5		; Unlock TED2 registers.
		sta	$E005
		lda	#$04		; Select normal TED2 512KB banks.
		sta	$E007
		lda	#%00001011	; FPGA=off+locked, RAM=writable.
		sta	$E006
		tii	$FFB0,$E000,8	; Restore bytes damaged if the memory
		plp			; was already writable.
		bra	fix_memopen	; Initialization complete.

		; The original function is 48 bytes long.

		ds	fix_memopen + 48 - *

end_memopen:



; ***************************************************************************
; ***************************************************************************
;
; tos_patch_jpn30 - Patch JPN version of Super System Card.
; tos_patch_usa30 - Patch USA version of Super System Card.
;
; Functions to apply the two patches above to a System Card 3.0 image when
; it has been loaded by the TED2 and is ready to be executed.
;
; These functions also convert a previously pre-patched "TED2 v3.01" copy
; of the image into the latest "TED2 v3.02" version.
;

		; Locations for the Japanese HuCard.

SCRSIZ_JMP_JPN	=	$E267 ; System Card bank 0 ; Original vector.
DISPLAY_JSR_JPN	=	$C887 ; System Card bank 1 ; V3.01 patch addr.
DISPLAY_MSG_JPN	=	$C950 ; System Card bank 1 ; Original function.

MEMTEST_JSR_JPN	=	$C86B ; System Card bank 1
MESSAGE_STR_JPN	=	$C9D1 ; System Card bank 1

RUNBOOT_JPN	=	$E206 ; System Card bank 0
SCRSIZ_FIX_JPN	=	$E28E ; System Card bank 0
MEMOPEN_FNC_JPN	=	$FE92 ; System Card bank 0

		; Locations for the American HuCard.

SCRSIZ_JMP_USA	=	$E280 ; System Card bank 0 ; Original vector.
DISPLAY_JSR_USA	=	$C887 ; System Card bank 1 ; V3.01 patch addr.
DISPLAY_MSG_USA	=	$C943 ; System Card bank 1 ; Original function

MEMTEST_JSR_USA	=	$C86B ; System Card Bank 1
MESSAGE_STR_USA	=	$C9C4 ; System Card bank 1

RUNBOOT_USA	=	$E21F ; System Card bank 0
SCRSIZ_FIX_USA	=	$E2A7 ; System Card bank 0
MEMOPEN_FNC_USA	=	$FEAB ; System Card bank 0

		;
		; Patch the JPN version of the Super System Card 3.0
		;

tos_patch_jpn30:lda	#$41
		tam3

		; Restore original jsr to display SUPER message.
		tii	.oldted_jsr_jpn,(DISPLAY_JSR_JPN & $1FFF) + $6000,3

		; Apply new patch to System Card's test for SCD RAM.
		tii	.memtst_jsr_jpn,(MEMTEST_JSR_JPN & $1FFF) + $6000,3

		; Copy the on-screen visible identifier that the ROM is patched.
		tii	tos_ted2_mesg,(MESSAGE_STR_JPN & $1FFF) + $6000,9

		lda	#$40
		tam3

		; Restore original ex_scrsiz vector.
		tii	.oldted_vec_jpn,(ex_scrsiz & $1FFF) + $6000,3

		; Apply new patch to ex_scrsiz.
		tii	.scrsiz_jmp_jpn,(SCRSIZ_FIX_JPN & $1FFF) + $6000,3

		; Apply new patch to System Card's test for SCD RAM.
		tii	.bootld_jsr_jpn,(RUNBOOT_JPN & $1FFF) + $6000,3

		; Copy the new ex_memopen code.
		tii	fix_memopen,(MEMOPEN_FNC_JPN & $1FFF) + $6000,end_memopen - fix_memopen

		jmp	tos_patch_all30

		; Obsolete TED2 v3.01 patches.
.oldted_vec_jpn:jmp	SCRSIZ_JMP_JPN
.oldted_jsr_jpn:jsr	DISPLAY_MSG_JPN

		; Current TED2 v3.02 patches.
.scrsiz_jmp_jpn:jmp	(fix_scrsiz - fix_memopen) + MEMOPEN_FNC_JPN
.memtst_jsr_jpn:jsr	(fix_unlock - fix_memopen) + MEMOPEN_FNC_JPN
.bootld_jsr_jpn:jsr	fix_bootload

		;
		; Patch the USA version of the Super System Card 3.0
		;

tos_patch_usa30:lda	#$41
		tam3

		; Restore original jsr to display SUPER message.
		tii	.oldted_jsr_usa,(DISPLAY_JSR_USA & $1FFF) + $6000,3

		; Apply new patch to System Card's test for SCD RAM.
		tii	.memtst_jsr_usa,(MEMTEST_JSR_USA & $1FFF) + $6000,3

		; Copy the on-screen visible identifier that the ROM is patched.
		tii	tos_ted2_mesg,(MESSAGE_STR_USA & $1FFF) + $6000,9

		lda	#$40
		tam3

		; Restore original ex_scrsiz vector.
		tii	.oldted_vec_usa,(ex_scrsiz & $1FFF) + $6000,3

		; Apply new patch to ex_scrsiz.
		tii	.scrsiz_jmp_usa,(SCRSIZ_FIX_USA & $1FFF) + $6000,3

		; Apply new patch to System Card's test for SCD RAM.
		tii	.bootld_jsr_usa,(RUNBOOT_USA & $1FFF) + $6000,3

		; Copy the new ex_memopen code.
		tii	fix_memopen,(MEMOPEN_FNC_USA & $1FFF) + $6000,end_memopen - fix_memopen

		jmp	tos_patch_all30

		; Obsolete TED2 v3.01 patches.
.oldted_vec_usa:jmp	SCRSIZ_JMP_USA
.oldted_jsr_usa:jsr	DISPLAY_MSG_USA

		; Current TED2 v3.02 patches.
.scrsiz_jmp_usa:jmp	(fix_scrsiz - fix_memopen) + MEMOPEN_FNC_USA
.memtst_jsr_usa:jsr	(fix_unlock - fix_memopen) + MEMOPEN_FNC_USA
.bootld_jsr_usa:jsr	fix_bootload

		;
		; Patch the both versions of the Super System Card 3.0
		;

tos_patch_all30:lda	#$5F
		tam3

		; Copy the boot-loader patch into bank 31 of the System Card.
		tii	(fix_bootload & $1FFF) + $E000,(fix_bootload & $1FFF) + $6000,end_bootload - fix_bootload

		lda	#$40
		tam3

		; Copy the first 16 bytes of the ROM in case they are overwritten.
		tii	($E000 & $1FFF) + $6000,($FFB0 & $1FFF) + $6000,16

		; Copy the last 16 bytes of the ROM in case they are overwritten.
		tii	($FFF0 & $1FFF) + $6000,($FFC0 & $1FFF) + $6000,16

		; Copy the TED2 patched-ROM identifier.
		tii	.name,($FFD0 & $1FFF) + $6000,12

;		; Dump the patched image to SD card for testing.
;		jsr	tos_save_sys3

		; Return the TED2 hardware control flags.

		ldy	#%00001011		; FPGA=off+locked, RAM=writable.
		rts

		; Signature (same address as McGenjin card).
.name:		db	"TED2CARD"
		db	$00		; Capabilities byte 0.
		db	$00		; Capabilities byte 1.
		db	$00		; Capabilities byte 2.
		db	$00		; Capabilities byte 3.

tos_ted2_mesg:	db	"TED2 3.02"



; ***************************************************************************
; ***************************************************************************
;
; tos_save_sys3 - Save the patched System Card 3 to SD card for debugging.
;
; Args: tos_bram_slot = Pointer to directory entry in cache.
; Uses: __bp = Pointer to directory entry in cache.
;
; Returns: X = F32_OK (and Z flag) or an error code.
;

		if	0

tos_save_sys3:	PUTS	msg_wait_init

		jsr	tos_chdir_tbed		; Select "/TBED/" directory.
		bne	.exit

		lda	#<tos_sys3_name		; Locate "SYSCARD.BIN" file.
		ldy	#>tos_sys3_name
		sta	<__ax + 0
		sty	<__ax + 1
		jsr	f32_find_name
		bne	.exit

		jsr	f32_open_file		; Open the file.
		bne	.exit

		ldx	#TOS_WRONG_SIZE		; Return error code.

		lda	f32_file_length + 3	; Check that the file is 256KB.
		ora	f32_file_length + 1
		ora	f32_file_length + 0
		bne	.close
		lda	f32_file_length + 2
		cmp	#$04
		bcc	.close

		lda	#>(262144 / 512)
		stz	<__ax + 0		; Lo-byte of # blocks in file.
		sta	<__ax + 1		; Hi-byte of # blocks in file.

		lda	#$4F			; Map in TED2 512KB bank 4.
		sta	TED_BASE_ADDR + TED_REG_MAP
		sta	sdc_data_bank

		stz	<sdc_data_addr + 0	; Load into $6000-$7FFF.
		lda	#$60
		sta	<sdc_data_addr + 1

		lda	#$40			; HuCard bank $00.
		tam3

		jsr	f32_file_save		; Save the file from memory.

		lda	#$40			; HuCard bank $00.
		tam3

.close:		jsr	f32_close_file		; Close the file & set N & Z.

.exit:		rts

tos_sys3_name:	db	"SYSCARD.BIN",0

		endif



; ***************************************************************************
; ***************************************************************************
;
; tos_patch_tbank - Patch TenNoKoe Bank HuCard to unlock Hudson's Present!
;

tos_patch_tbank:lda	#$41			; Select bank 1 of the HuCard
		tam3				; image in TED2 memory.

		lda	$723E			; $01:D23E in HuCard.
		cmp	#$D0
		bne	.exit
		lda	$723F			; $01:D23F in HuCard.
		cmp	#$0B
		bne	.exit
		lda	#$89			; Xvert BNE to BIT.
		sta	$723E
.exit:		rts



; ***************************************************************************
; ***************************************************************************
;
; wait_vsync - Wait for the next VSYNC.
;

wait_vsync_dly:	bsr	wait_vsync
		dec	a
		bne	wait_vsync_dly
		rts

wait_vsync:	pha
		lda	irq_cnt
.loop:		cmp	irq_cnt
		beq	.loop
		pla
		rts



; ***************************************************************************
; ***************************************************************************
;
; wait_vsync_usb - Wait for the next VSYNC, while checking for USB commands.
;

wait_vsync_usb:	pha
		phx
		phy
		ldy	irq_cnt
.wait:	.if	REALHW
		TED_USB_RD_TEST
		bne	.got_usb
	.endif
		cpy	irq_cnt
		beq	.wait
		ply
		plx
		pla
		rts

.got_usb:	TED_USB_RD_BYTE
		cmp	#'*'
		beq	.got_star
.had_star:	ldx	#0			; Was this preceded by a '*'?
		beq	.ignore
		cmp	#'t'
		beq	.got_t
		cmp	#'g'
		beq	.got_g
.ignore:	stz	.had_star + 1		; Self-modifying code!
		bra	.wait

; turbo-usb2.exe is sending us a command!

.got_star:	sta	.had_star + 1		; Self-modifying code!
		bra	.wait

; turbo-usb2.exe wants to know if we're listening!

.got_t:		lda	#'k'
		TED_USB_WR_BYTE
		bra	.wait

; turbo-usb2.exe wants to send a HuCard ROM image!

.got_g:		if	HIRES
		ldx	#<video_mode_480	; Change to hi-res screen.
		lda	#>video_mode_480
		jsr	tos_screen_mode
		endif

		PUTS	.msg_usb_title		; Display USB message.
		PUTS	.msg_usb_upload		; The only function, for now!

		stz	tos_card_size		; Keep track of size loaded.

		lda	#$4F			; Map in TED2 512KB block 4.
		sta	TED_BASE_ADDR + TED_REG_MAP

		pha				; Preserve TED2 512KB block.

		lda	#$40			; Select bank 0 of the HuCard
		tam3				; image in TED2 memory.

.bank_loop:	clx
		lda	#$60			; Reset the desination page.
		sta	.save_page + 2

.byte_loop:	lda	#(1 << TED_FLG_USB_RD)	; Copy a byte from USB.
.wait_byte:	bit	TED_BASE_ADDR + TED_REG_STATE
		beq	.wait_byte
		lda	TED_BASE_ADDR + TED_REG_FIFO
.save_page:	sta	$6000,x
		inx
		bne	.byte_loop		; Same page?
		inc	.save_page + 2
		bpl	.byte_loop		; Same bank?

		PUTS	.msg_usb_dot		; Update progress bar.

		tma3				; Map in next PCE 8KB bank.
		inc	a
		bpl	.next_bank		; Have we just loaded 512KB?

.next_512kb:	pla				; Map in next TED 512KB block.
		clc
		adc	#$10
		bit	#$40			; Wrap from block 4 to block 0.
		beq	.wrap_512kb
		and	#$0F

.wrap_512kb:	pha				; Preserve TED2 512KB block.
		sta	TED_BASE_ADDR + TED_REG_MAP

		lda	#$40			; Reset destination PCE bank.
.next_bank:	tam3
		bit	#$0F
		bne	.wait_next
		inc	tos_card_size		; Increment # of 128KB chunks.

		bit	#$1F			; Half hi-res line.
;		bit	#$3F			; Full hi-res line.
		bne	.wait_next
		PUTS	.msg_usb_eol		; Update progress bar.

.wait_next:	TED_USB_RD_BYTE			; What next?
		cmp	#'+'			; Load another bank!
		beq	.bank_loop

		pla				; Discard TED 512KB block.

		TED_USB_RD_BYTE			; Is this a special HuCard?
;		cmp	#'p'			; It is Populous?
;		cmp	#'s'			; It is Street Fighter?
		sta	.msg_usb_type + 1

		tma3				; Inform the user, not that
		and	#$1F			; they'll have time to see
		beq	.skip_eol		; it before the HuCard runs!
		PUTS	.msg_usb_eol
.skip_eol:	PUTS	.msg_usb_done

		jmp	tos_exec_hucard		; Execute the HuCard.

.msg_usb_title:	db	"%>%p5%xl",0
		db	$0C
		db	" USB Communication"
		db	"%<%p0"
		db	$0A,$0A,$0A,0

.msg_usb_upload:db	" Starting download from computer!",$0A," ",0
.msg_usb_dot:	db	".",0
.msg_usb_eol:	db	$0A," ",0
.msg_usb_done:	db	"Completed download of type "
.msg_usb_type:	db	"'x' HuCard.",$0A,$0A,$0A,0



; ***************************************************************************
; ***************************************************************************
;
; read_joypads (full mouse support, but 6-button pad III..VI are ignored).
;
; This code distinguishes between a mouse and a 2-button or 6-button joypad,
; so that unsupported devices do not have to be unplugged from the TurboTap.
;
; Read four times to get both sets of 8-bit mouse delta coordinates.
;
; N.B. Takes approx 1/3 frame to detect mice the first time it is run.
;
; bit values for joypad 2-button bytes: (MSB = #7; LSB = #0)
; ----------------------------------------------------------
; bit 0 (ie $01) = I
; bit 1 (ie $02) = II
; bit 2 (ie $04) = SELECT
; bit 3 (ie $08) = RUN
; bit 4 (ie $10) = UP
; bit 5 (ie $20) = RIGHT
; bit 6 (ie $40) = DOWN
; bit 7 (ie $80) = LEFT
;
; bit values for joypad 6-button bytes: (MSB = #7; LSB = #0)
; ----------------------------------------------------------
; bit 0 (ie $01) = III
; bit 1 (ie $02) = IV
; bit 2 (ie $04) = V
; bit 3 (ie $08) = VI
; bit 4 (ie $10) = zero
; bit 5 (ie $20) = zero
; bit 6 (ie $40) = zero
; bit 7 (ie $80) = zero, but set to one for 6-button detect.
;

		if	SUPPORT_MOUSE

MAX_PADS	=	5			; 5 normally, 3 to save time.

SLOW_AUTORPT	=	30
FAST_AUTORPT	=	10

		bss

joyrpt		ds	MAX_PADS
mouse_x		ds	MAX_PADS
mouse_y		ds	MAX_PADS

		code

read_joypads:	tii	joynow,joyold,MAX_PADS	; Save the previous values.

		; Detect attached mice the first time this routine is called.

		lda	mouse_flags		; Has mouse detection happened?
		bpl	.calc_pressed

		ldy	#15			; Initialize repeat count.
		cla				; Initialize mouse detection.
.detect_loop:	phy
		pha
		bsr	.read_devices		; Read all devices as if mice.
		pla
		clx
.detect_port:	ldy	mouse_y,x		; A movement of zero means
		bne	.detect_next		; this port is a mouse.
		ora	tos_bit_mask,x
.detect_next:	inx				; Get the next pad from the
		cpx	#MAX_PADS		; multitap.
		bne	.detect_port
		ply				; Repeat the detection test.
		dey
		bne	.detect_loop

		sta	mouse_flags		; Report mouse detection.

		; See what has just been pressed, and check for soft-reset.

.calc_pressed:	bsr	.read_devices		; Read all devices normally.

		ldx	#MAX_PADS - 1

.pressed_loop:	lda	joynow,x		; Calc which buttons have just
		tay				; been pressed.
		eor	joyold,x		; Unlike the System Card, here
		and	joynow,x		; the "trg" is cumulative and
		ora	joytrg,x		; must be cleared when used.
		sta	joytrg,x

		cmp	#$04			; Detect the soft-reset combo,
		bne	.calc_next		; hold RUN then press SELECT.
		cpy	#$0C
		bne	.calc_next
		lda	tos_bit_mask,x		; Is soft-reset enabled on this
		bit	joyena			; port?
		bne	.soft_reset

		; Do auto-repeat processing on the d-pad.

.calc_next:	tya				; Auto-Repeat the UP and DOWN
		ldy	#SLOW_AUTORPT		; while they are held.
		and	#JOY_U + JOY_D
		beq	.set_delay
		dec	joyrpt,x
		bne	.no_repeat
		ora	joytrg,x
		sta	joytrg,x
		ldy	#FAST_AUTORPT
.set_delay:	tya
		sta	joyrpt,x

.no_repeat:	dex				; Check the next pad from the
		bpl	.pressed_loop		; multitap.
		rts				; All done, phew!

.soft_reset:	lda	#$80			; Disable the BIOS PSG driver.
		sta	<$E7
		jmp	[$2284]			; Jump to the soft-reset hook.

		; Read all of the devices attached to the TurboTap.

.read_devices:	ldy	#3			; Repeat this loop 4 times.

.read_turbotap: lda	.mouse_vectors,y	; Self-modify the branch for
		sta	.branch_mod + 1		; this pass.

		clx				; Start at port 1.
		lda	#$01			; CLR lo, SEL hi for d-pad.
		sta	IO_PORT
		lda	#$03			; CLR hi, SEL hi, reset tap.
		sta	IO_PORT

.read_port:	lda	#$01			; CLR lo, SEL hi for d-pad.
		sta	IO_PORT

		lda	tos_bit_mask,x		; Is there a mouse attached?
.device_mod:	and	#%10011111		; Self-Modifying code!!!
.branch_mod:	bne	.mouse_y_lo		; Self-Modifying code!!!

		cpy	#2			; Joypads only need to be read
		bcc	.skip_port		; twice, skip the other reads.

.read_pad:	lda	IO_PORT			; Read direction-pad bits.
		stz	IO_PORT			; CLR lo, SEL lo for buttons.
		asl	a			; Wait 1.25us (9 cycles).
		asl	a
		asl	a
		asl	a
		beq	.next_port		; 6-btn pad if UDLR all held.

.read_2button:	sta	.button_mod + 1		; Get buttons of 2-btn pad.
		lda	IO_PORT
		and	#$0F
.button_mod:	ora	#$00			; Self-Modifying code!!!
		eor	#$FF
		sta	joynow,x

.skip_port:	stz	IO_PORT			; CLR lo, SEL lo for buttons.

.next_port:	inx				; Get the next pad from the
		cpx	#MAX_PADS		; multitap.
		bne	.read_port

		dey				; Do the next complete pass.
		bpl	.read_turbotap		; Have we finished 4 passes?
		rts				; Now that everything is read.

		; Mouse processing, split into four passes.

.mouse_x_hi:	lda	#28			; 179 cycle delay after CLR lo
.wait_loop:	dec	a			; on port to allow the mouse
		bne	.wait_loop		; to buffer and reset counters.

		lda	IO_PORT			; Read direction-pad bits.
		stz	IO_PORT			; CLR lo, SEL lo for buttons.
		asl	a			; Wait 1.25us (9 cycles).
		asl	a
		asl	a
		asl	a
		sta	mouse_x,x		; Save port's X-hi nibble.

		lda	IO_PORT			; Get mouse buttons.
		and	#$0F
		eor	#$0F
		sta	joynow,x
		bra	.next_port

.mouse_x_lo:	lda	IO_PORT			; Read direction-pad bits.
		and	#$0F			; Wait 1.25us (9 cycles).
		ora	mouse_x,x		; Add port's X-hi nibble.
		sta	mouse_x,x
		bra	.skip_port

.mouse_y_hi:	lda	IO_PORT			; Read direction-pad bits.
		asl	a			; Wait 1.25us (9 cycles).
		asl	a
		asl	a
		asl	a
		sta	mouse_y,x		; Save port's Y-hi nibble.
		bra	.skip_port

.mouse_y_lo:	lda	IO_PORT			; Read direction-pad bits.
		and	#$0F			; Wait 1.25us (9 cycles).
		ora	mouse_y,x		; Add port's Y-hi nibble.
		sta	mouse_y,x
		bra	.skip_port

.mouse_vectors:	db	(.mouse_y_lo - .branch_mod) - 2 ; Pass 4
		db	(.mouse_y_hi - .branch_mod) - 2 ; Pass 3
		db	(.mouse_x_lo - .branch_mod) - 2 ; Pass 2
		db	(.mouse_x_hi - .branch_mod) - 2 ; Pass 1

mouse_flags	=	.device_mod + 1

		endif



; ***************************************************************************
; ***************************************************************************
;
; read_joypads (full 6-button pad support, but mouse movement is incomplete).
;
; This code distinguishes between a mouse and a 2-button or 6-button joypad,
; so that unsupported devices do not have to be unplugged from the TurboTap.
;
; Read two times to get both sets of button data for 6-button joypad.
;
; N.B. Takes approx 1/3 frame to detect mice the first time it is run.
;
; bit values for joypad 2-button bytes: (MSB = #7; LSB = #0)
; ----------------------------------------------------------
; bit 0 (ie $01) = I
; bit 1 (ie $02) = II
; bit 2 (ie $04) = SELECT
; bit 3 (ie $08) = RUN
; bit 4 (ie $10) = UP
; bit 5 (ie $20) = RIGHT
; bit 6 (ie $40) = DOWN
; bit 7 (ie $80) = LEFT
;
; bit values for joypad 6-button bytes: (MSB = #7; LSB = #0)
; ----------------------------------------------------------
; bit 0 (ie $01) = III
; bit 1 (ie $02) = IV
; bit 2 (ie $04) = V
; bit 3 (ie $08) = VI
; bit 4 (ie $10) = zero
; bit 5 (ie $20) = zero
; bit 6 (ie $40) = zero
; bit 7 (ie $80) = zero, but set to one for 6-button detect.
;

		if	SUPPORT_MOUSE
		else

MAX_PADS	=	5			; 5 normally, 3 to save time.

SLOW_AUTORPT	=	30
FAST_AUTORPT	=	10

		bss

joyrpt		ds	MAX_PADS
joy6now		ds	MAX_PADS
joy6trg		ds	MAX_PADS
joy6old		ds	MAX_PADS
mouse_x		ds	MAX_PADS
mouse_y		ds	MAX_PADS

		code

read_joypads:	tii	joynow,joyold,MAX_PADS	; Save the previous values.
		tii	joy6now,joy6old,MAX_PADS

;		stz	joy6now + 0		; Clear the 6-btn bits in case
;		stz	joy6now + 1		; the joypad was unplugged.
;		stz	joy6now + 2
;		stz	joy6now + 3
;		stz	joy6now + 4

		; Detect attached mice the first time this routine is called.

		lda	mouse_flags		; Has mouse detection happened?
		bpl	.calc_pressed

		ldy	#23			; Initialize repeat count.
		cla				; Initialize mouse detection.
.detect_loop:	phy
		pha
		bsr	.read_devices		; Read all devices as if mice.
		pla
		clx
.detect_port:	ldy	mouse_x,x		; A movement of zero means
		bne	.detect_next		; this port is a mouse.
		ora	tos_bit_mask,x
.detect_next:	inx				; Get the next pad from the
		cpx	#MAX_PADS		; multitap.
		bne	.detect_port
		ply				; Repeat the detection test.
		dey
		bne	.detect_loop

		sta	mouse_flags		; Report mouse detection.

		; See what has just been pressed, and check for soft-reset.

.calc_pressed:	bsr	.read_devices		; Read all devices normally.

		ldx	#MAX_PADS - 1

.pressed_loop:	lda	joy6now,x		; Calc which buttons have just
		eor	joy6old,x		; been pressed.
		and	joy6now,x		; Unlike the System Card, here
		ora	joy6trg,x		; the "trg" is cumulative and
		sta	joy6trg,x		; must be cleared when used.

		lda	joynow,x
		tay
		eor	joyold,x
		and	joynow,x
		ora	joytrg,x
		sta	joytrg,x

		cmp	#$04			; Detect the soft-reset combo,
		bne	.calc_next		; hold RUN then press SELECT.
		cpy	#$0C
		bne	.calc_next
		lda	tos_bit_mask,x		; Is soft-reset enabled on this
		bit	joyena			; port?
		bne	.soft_reset

		; Do auto-repeat processing on the d-pad.

.calc_next:	tya				; Auto-Repeat the UP and DOWN
		ldy	#SLOW_AUTORPT		; while they are held.
		and	#JOY_U + JOY_D
		beq	.set_delay
		dec	joyrpt,x
		bne	.no_repeat
		ora	joytrg,x
		sta	joytrg,x
		ldy	#FAST_AUTORPT
.set_delay:	tya
		sta	joyrpt,x

.no_repeat:	dex				; Check the next pad from the
		bpl	.pressed_loop		; multitap.
		rts				; All done, phew!

.soft_reset:	lda	#$80			; Disable the BIOS PSG driver.
		sta	<$E7
		jmp	[$2284]			; Jump to the soft-reset hook.

		; Read all of the devices attached to the TurboTap.

.read_devices:	ldy	#1			; Repeat this loop 2 times.

.read_turbotap: lda	.mouse_vectors,y	; Self-modify the branch for
		sta	.branch_mod + 1		; this pass.

		clx				; Start at port 1.
		lda	#$01			; CLR lo, SEL hi for d-pad.
		sta	IO_PORT
		lda	#$03			; CLR hi, SEL hi, reset tap.
		sta	IO_PORT

.read_port:	lda	#$01			; CLR lo, SEL hi for d-pad.
		sta	IO_PORT

		lda	tos_bit_mask,x		; Is there a mouse attached?
.device_mod:	and	#%10011111		; Self-Modifying code!!!
.branch_mod:	bne	.mouse_x_lo		; Self-Modifying code!!!

.read_pad:	lda	IO_PORT			; Read direction-pad bits.
		stz	IO_PORT			; CLR lo, SEL lo for buttons.
		asl	a			; Wait 1.25us (9 cycles).
		asl	a
		asl	a
		asl	a
		beq	.read_6button		; 6-btn pad if UDLR all held.

.read_2button:	sta	.button_mod + 1		; Get buttons of 2-btn pad.
		lda	IO_PORT
		and	#$0F
.button_mod:	ora	#$00			; Self-Modifying code!!!
		eor	#$FF
		sta	joynow,x

.skip_port:	stz	IO_PORT			; CLR lo, SEL lo for buttons.

.next_port:	inx				; Get the next pad from the
		cpx	#MAX_PADS		; multitap.
		bne	.read_port

		dey				; Do the next complete pass.
		bpl	.read_turbotap		; Have we finished 2 passes?
		rts				; Now that everything is read.

.read_6button:	lda	IO_PORT			; Get buttons of 6-btn pad.
		and	#$0F
		eor	#$8F			; Set bit-7 to show that a
		sta	joy6now,x		; 6-button pad is present.
		bra	.skip_port

		; Mouse processing, normally four passes, here just two.

.mouse_x_hi:	lda	#28			; 179 cycle delay after CLR lo
.wait_loop:	dec	a			; on port to allow the mouse
		bne	.wait_loop		; to buffer and reset counters.

		lda	IO_PORT			; Read direction-pad bits.
		stz	IO_PORT			; CLR lo, SEL lo for buttons.
		asl	a			; Wait 1.25us (9 cycles).
		asl	a
		asl	a
		asl	a
		sta	mouse_x,x		; Save port's X-hi nibble.

		lda	IO_PORT			; Get mouse buttons.
		and	#$0F
		eor	#$0F
		sta	joynow,x
		bra	.next_port

.mouse_x_lo:	lda	IO_PORT			; Read direction-pad bits.
		and	#$0F			; Wait 1.25us (9 cycles).
		ora	mouse_x,x		; Add port's X-hi nibble.
		sta	mouse_x,x
		bra	.skip_port

.mouse_vectors:	db	(.mouse_x_lo - .branch_mod) - 2 ; Pass 2
		db	(.mouse_x_hi - .branch_mod) - 2 ; Pass 1

mouse_flags	=	.device_mod + 1

		endif



; ***************************************************************************
; ***************************************************************************
;
; rom_irq1_stub - System Card compatible interrupt handler stub.
;
; Note that it takes 8 cycles to respond to an IRQ.
;

jmp_irq1_hook:	jmp	[irq1_hook]

rom_irq1_stub:	bbs1	<irq_vec, jmp_irq1_hook

		pha
		phx
		phy

		lda	VDC_SR
		sta	<vdc_sr

		; VSYNC

.test_vsync:	bbr5	<vdc_sr, .test_hsync

		st0	#$05
		lda	<vdc_crl
		sta	VDC_DL
;		lda	<vdc_crh		; Do NOT do this, it f**ks up the
;		sta	VDC_DH			; VRAM increment.

		inc	irq_cnt

		bbs5	<irq_vec, .no_rom_vsync

		st0	#$07
		tii	bg_x1, VDC_DL, $0002
		st0	#$08
		tii	bg_y1, VDC_DL, $0002

;		jsr	ex_colorcmd		; In the System Card!!!

		jsr	tos_pulse_color

		inc	rndseed

;		lda	<psg_driver_mode
;		cmp	#$01
;		bne	.skip_psg
;		jsr	psg_driver		; In the System Card!!!

.skip_psg:	jsr	read_joypads

.no_rom_vsync:	bbr4	<irq_vec, .test_hsync
		bsr	jmp_vsync_hook

		; HSYNC

.test_hsync:	bbr2	<vdc_sr, .exit

		bbs7	<irq_vec, .no_rom_hsync
		bsr	.delay
		bsr	.delay
		bsr	.delay			; Extra delay so it works properly!

		st0	#$07
		tii	bg_x2, VDC_DL, $0002
		st0	#$08
		tii	bg_y2, VDC_DL, $0002

.no_rom_hsync:	bbr6	<irq_vec, .exit
		bsr	jmp_hsync_hook

		; End of IRQ1

.exit:		lda	<vdc_reg
		sta	VDC_AR
		ply
		plx
		pla

		rti

.delay:		rts

jmp_vsync_hook: jmp	[vsync_hook]
jmp_hsync_hook: jmp	[hsync_hook]



; ***************************************************************************
; ***************************************************************************
;
; rom_irq2_stub - System Card compatible interrupt handler stub.
;
; Note that it takes 8 cycles to respond to an IRQ.
;

jmp_irq2_hook:	jmp	[irq2_hook]

rom_irq2_stub:	bbs0	<irq_vec, jmp_irq2_hook
.hang:		bra	.hang



; ***************************************************************************
; ***************************************************************************
;
; rom_timer_stub - System Card compatible interrupt handler stub.
;
; Note that it takes 8 cycles to respond to an IRQ.
;

jmp_timer_hook: jmp	[timer_hook]

rom_timer_stub: bbs2	<irq_vec, jmp_timer_hook
.hang:		bra	.hang



; ***************************************************************************
; ***************************************************************************
;
; rom_nmi_stub - System Card compatible interrupt handler stub.
;
; Note that it takes 8 cycles to respond to an IRQ.
;

rom_nmi_stub:	rti



; ***************************************************************************
; ***************************************************************************
;
; RESET VECTORS (when running as the TED OS)
;

		bank	3
		org	$FFE8

tos_bit_mask:	db	$01,$02,$04,$08,$10,$20,$40,$80

		dw	$0301			; TEOS Version.
tos_empty:	dw	$FFFF			; Useful constant for TAI.
tos_zero:	dw	$0000			; Useful constant for TAI.
		dw	rom_irq2_stub		; a.k.a. IRQ2
		dw	rom_irq1_stub		; a.k.a. IRQ1
		dw	rom_timer_stub
		dw	rom_nmi_stub
		dw	teos_reset		; RESET
