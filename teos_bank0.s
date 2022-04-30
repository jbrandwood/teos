; ***************************************************************************
; ***************************************************************************
;
; BANK $00 - CARTRIDGE STARTUP & CORE FUNCTIONALITY
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
; While running as the TED OS, the bank mapping is ...
;
; MPR0 = bank $FF : PCE hardware
; MPR1 = bank $F8 : ZP & Stack in RAM
; MPR2 = bank $00 : TED2 hardware
; MPR3 = bank $xx : TED2 RAM
; MPR4 = bank $01 : Set by the TED boot loader.
; MPR5 = bank $02 : Set by the TED boot loader.
; MPR6 = bank $03 : Set by the TED boot loader.
; MPR7 = bank $04 : Set by the TED boot loader.
;
;
; When initially run as a cartridge, the bank mapping starts as ...
;
; MPR0 = bank $FF : PCE hardware
; MPR1 = bank $F8 : ZP & Stack in RAM
; MPR2 = bank $00 : TED2 hardware
; MPR3 = bank $xx : TED2 RAM
; MPR4 = bank $00
; MPR5 = bank $01
; MPR6 = bank $02 : Boot code
; MPR7 = bank $03 : IRQ vectors in RAM
;
;
; When run as a cartridge, the code first copies itself into the same bank
; locations that TED2 boots the OS, and then it restarts itself.
;
; ***************************************************************************
; ***************************************************************************
;
; !!! WARNING !!!
;
; When run as a cartridge, this whole bank disappears while the TED2
; hardware is mapped in and being accessed.
;
; !!! DO NOT PUT CRITICAL OR INTERRUPT-HANDLING CODE IN THIS BANK !!!
;
; ***************************************************************************
; ***************************************************************************

		.bank	0
		.org	$8000

		; Startup point if run by the Turbo Everdrive bootloader.

teos_reset:	bra	teos_initialize		; Start @ $8000 for TED2 boot.

		nop				; Leave space so that writing
		nop				; the TED2's unlock and config
		nop				; registers doesn't corrupt
		nop				; any important code.
		nop
		nop
		nop
		nop

		; Initialize data banks for both environments.

teos_initialize:sei				; Disable interrupts (soft-reset).
		csh				; Set high-speed mode.
		cld
		ldx	#$FF			; Initialize stack pointer.
		txs
		txa				; MPR0 = bank $FF : PCE hardware
		tam0				; MPR1 = bank $F8 : ZP & Stack in RAM
		lda	#WRAM_BANK		; MPR2 = bank $00 : TED2 hardware
		tam1				; MPR3 = bank $43 : TED2 bootloader2
		cla				;
		tam2				; MPR4..MPR7 : DO NOT CHANGE!

                lda     #$A5			; Enable TED2 hardware.
                sta     TED_BASE_ADDR + TED_REG_KEY1
		stz	TED_BASE_ADDR + TED_REG_CFG
		lda	#$0F
		sta	TED_BASE_ADDR + TED_REG_MAP

		lda	ted_assem_date + 1	; Is TED2 hardware info setup?
		bne	.init_pce		; (Because year is NEVER zero.)

		lda	#$43			; The stage 1 FPGA boot loader
		tam3				; reads the 16KB stage 2 boot
		tii	$6100,ted_assem_date,6	; loader from eeprom to banks
		lda	#$44			; $43 & $44.
		tam3				; We read the hardware info
		tii	$7FF0,ted_boot2_ver,2	; from the stage 2 image.

.init_pce:	jsr	init_vce		; Blank the screen.

		tai	tos_zero,$2000,$2000	; Clear RAM.

		ldx	#<video_mode_320	; Enable display.
		lda	#>video_mode_320
		jsr	init_vdc

		jsr	clear_vram		; Clear VRAM.

		stw	#$0000,VCE_CTA
		stw	#cpc464_palette,__ax
		ldx	#8
		jsr	copy_palettes

;		lda	#$CC			; Enable SPR, BKG, VSYNC IRQ, RCR IRQ.
;		lda	#$C8			; Enable SPR, BKG, VSYNC IRQ.
;		lda	#$8C			; Enable BKG, VSYNC IRQ, RCR IRQ.
		lda	#$88			; Enable BKG, VSYNC IRQ.
		sta	<vdc_crl

		lda	VDC_SR			; Clear any pending VDC irq.
		cli
		jsr	wait_vsync		; Wait for the screen to show.

		jmp	tos_menu_init		; Start up the menu system.



; ***************************************************************************
; ***************************************************************************
;
; init_vce - Clear all of the VCE's palette entries.
;

init_vce:	php
		sei
		stz	VCE_CTA+0
		stz	VCE_CTA+1
		ldy	#$02
		clx
.loop:		stz	VCE_CTW+0
		stz	VCE_CTW+1
		dex
		bne	.loop
		dey
		bne	.loop
		plp
		rts



; ***************************************************************************
; ***************************************************************************
;
; TED2's System Information Block
;
; Building these into the OS space makes them persistent across a reset or
; the running of a HuCard cartridge image.
;

		; Current SDC volume and file selection, stored here in an OS
		; bank so that it survives in TED2 RAM while a cart is run.

tos_cur_cardid:	ds	16			; Current FAT32 volume ID.
tos_cur_folder:	ds	4			; Current directory cluster.
tos_cur_depth:	ds	1			; Depth in directory tree.
tos_cur_page:	ds	32+1			; For each level in the tree.
tos_cur_file:	ds	32+1			; For each level in the tree.
tos_tennokoe:	ds	1			; Last HuCard was Ten No Koe?

		ds	$8106-*			; Pad out dead space.

		; Krikzz's original OS stores these values at $8106 when it
		; runs. TEOS keeps them at the same location so that we can
		; read them from there if TEOS is run as a HuCard image ...
		; either from Krikzz's OS, or from another version of TEOS.

		.org	$8106

ted_assem_date:	ds	2			; Assembly date.
ted_assem_time:	ds	2			; Assembly time.
ted_serial_num:	ds	2			; Serial Number.
ted_boot2_ver:	ds	2			; Boot2 Version.



; ***************************************************************************
; ***************************************************************************
;
; init_vdc - Initialize the VDC's video mode from a table.
;

init_vdc:	stx	<__ax + 0
		sta	<__ax + 1

		php
		sei
		lda	rndseed			; Preserve rndseed.
		pha				; Overwritten by DCR hi.

		lda	[__ax]
		sta	VCE_CR
		ldy	#1

.loop:		lda	[__ax],y
		beq	.done
		sta	VDC_AR
		iny
		tax
		lda	.shadow_lo - 5,x
		sta	<__bx + 0
		lda	.shadow_hi - 5,x
		sta	<__bx + 1

		lda	[__ax],y
		iny
		sta	VDC_DL
		sta	[__bx]
		inc	<__bx

		lda	[__ax],y
		iny
		sta	VDC_DH
		sta	[__bx]
		bra	.loop

.done:		pla				; Restore rndseed.
		sta	rndseed

		plp
		rts

.shadow_lo:	;db	<__cx			; 0  = VDC_MAWR
		;db	<__cx			; 1  = VDC_MARR
		;db	<__cx			; 2  = VDC_VWR
		;db	<__cx			; 3
		;db	<__cx			; 4
		db	<vdc_crl		; 5  = VDC_CR
		db	<__cx			; 6  = VDC_RCR
		db	<bg_x1			; 7  = VDC_BXR
		db	<bg_y1			; 8  = VDC_BYR
		db	<vdc_mwr		; 9  = VDC_MWR
		db	<__cx			; 10 = VDC_HSR
		db	<__cx			; 11 = VDC_HDR
		db	<__cx			; 12 = VDC_VPR
		db	<__cx			; 13 = VDC_VDW
		db	<__cx			; 14 = VDC_VCR
		db	<vdc_dcr		; 15 = VDC_DCR
		db	<__cx			; 16 = VDC_SOUR
		db	<__cx			; 17 = VDC_DESR
		db	<__cx			; 18 = VDC_LENR
		db	<satb_addr		; 19 = VDC_DVSSR

.shadow_hi:	;db	>__cx			; 0  = VDC_MAWR
		;db	>__cx			; 1  = VDC_MARR
		;db	>__cx			; 2  = VDC_VWR
		;db	>__cx			; 3
		;db	>__cx			; 4
		db	>vdc_crl		; 5  = VDC_CR
		db	>__cx			; 6  = VDC_RCR
		db	>bg_x1			; 7  = VDC_BXR
		db	>bg_y1			; 8  = VDC_BYR
		db	>vdc_mwr		; 9  = VDC_MWR
		db	>__cx			; 10 = VDC_HSR
		db	>__cx			; 11 = VDC_HDR
		db	>__cx			; 12 = VDC_VPR
		db	>__cx			; 13 = VDC_VDW
		db	>__cx			; 14 = VDC_VCR
		db	>vdc_dcr		; 15 = VDC_DCR
		db	>__cx			; 16 = VDC_SOUR
		db	>__cx			; 17 = VDC_DESR
		db	>__cx			; 18 = VDC_LENR
		db	>satb_addr		; 19 = VDC_DVSSR

		; Initialization for a HuCard (256-wide with RCR for sync).

video_mode_nul:	db	VCE_CR_5MHz		; VCE Clock

		db	VDC_CR			; Control Register
		dw	$0004			;   Enable RCR interrupt
		db	VDC_RCR			; Raster Counter Register
		dw	$0040			;   RCR irq on screen line 0
		db	VDC_BXR			; Background X-Scroll Register
		dw	$0000
		db	VDC_BYR			; Background Y-Scroll Register
		dw	$0000
		db	VDC_MWR			; Memory-access Width Register
		dw	VDC_MWR_32x32 + VDC_MWR_1CYCLE
		db	VDC_HSR			; Horizontal Sync Register
		dw	VDC_HSR_256
		db	VDC_HDR			; Horizontal Display Register
		dw	VDC_HDR_256
		db	VDC_VPR			; Vertical Sync Register
		dw	VDC_VPR_224		;   Do not change!
		db	VDC_VDW			; Vertical Display Register
		dw	VDC_VDW_224		;   Do not change!
		db	VDC_VCR			; Vertical Display END position Register
		dw	VDC_VCR_224		;   Do not change!
		db	VDC_DCR			; DMA Control Register
		dw	$0000
		db	VDC_DVSSR		; SATB address $0000
		dw	$0000
		db	0

		; A 320-wide screen, widened further to give it a border.

video_mode_320:	db	VCE_CR_7MHz + 4		; VCE Clock + Artifact Reduction

		db	VDC_CR			; Control Register (enable VSYNC IRQ)
		dw	$0008
		db	VDC_RCR			; Raster Counter Register
		dw	$0000
		db	VDC_BXR			; Background X-Scroll Register
		dw	$FFF4
		db	VDC_BYR			; Background Y-Scroll Register
		dw	$0001
		db	VDC_MWR			; Memory-access Width Register
		dw	VDC_MWR_128x32 + VDC_MWR_1CYCLE
		db	VDC_HSR			; Horizontal Sync Register
		dw	VDC_HSR_344
		db	VDC_HDR			; Horizontal Display Register
		dw	VDC_HDR_344
		db	VDC_VPR			; Vertical Sync Register
		dw	VDC_VPR_208
		db	VDC_VDW			; Vertical Display Register
		dw	VDC_VDW_208
		db	VDC_VCR			; Vertical Display END position Register
		dw	VDC_VCR_208
		db	VDC_DCR			; DMA Control Register
		dw	$0010
		db	VDC_DVSSR		; SATB address $1000
		dw	$0010
		db	0

		; A 480-wide screen, widened further to give it a border.

video_mode_480:	db	VCE_CR_10MHz + 4	; VCE Clock + Artifact Reduction

		db	VDC_CR			; Control Register (enable VSYNC IRQ)
		dw	$0008
		db	VDC_RCR			; Raster Counter Register
		dw	$0000
		db	VDC_BXR			; Background X-Scroll Register
		dw	$FFE8
		db	VDC_BYR			; Background Y-Scroll Register
		dw	$0001
		db	VDC_MWR			; Memory-access Width Register
		dw	VDC_MWR_128x32 + VDC_MWR_2CYCLE
		db	VDC_HSR			; Horizontal Sync Register
		dw	VDC_HSR_512
		db	VDC_HDR			; Horizontal Display Register
		dw	VDC_HDR_512
		db	VDC_VPR			; Vertical Sync Register
		dw	VDC_VPR_208
		db	VDC_VDW			; Vertical Display Register
		dw	VDC_VDW_208
		db	VDC_VCR			; Vertical Display END position Register
		dw	VDC_VCR_208
		db	VDC_DCR			; DMA Control Register
		dw	$0010
		db	VDC_DVSSR		; SATB address $1000
		dw	$0010
		db	0



; ***************************************************************************
; ***************************************************************************
;
; clear_vram - Clear all of VRAM.
;

clear_vram:	bsr	clear_screen		; Clear the BAT  ($0E00).

		ldx	#$1C + 1		; Clear the rest ($7200).
		ldy	#$80
		st1	#$00
.clr_loop:
		st2	#$00
		st2	#$00
		st2	#$00
		st2	#$00
		dey
		bne	.clr_loop
		dex
		bne	.clr_loop

		rts



; ***************************************************************************
; ***************************************************************************
;
; clear_screen - Clear the BAT.
;

clear_screen:	vreg	#VDC_CR
		st2	#%00000000

		vreg	#VDC_MAWR
		stz	VDC_DL
		stz	VDC_DH
		vreg	#VDC_VWR

		if	1

		ldy	#$80
		st1	#$20
.top_loop:	st2	#$41
		st2	#$41
		dey
		bne	.top_loop

		ldx	#$05+1
;		ldy	#$C0
		ldy	#$80
		st1	#$20
.mid_loop:	st2	#$01
		st2	#$01
		dey
		bne	.mid_loop
		dex
		bne	.mid_loop

;		ldy	#$C0
		ldy	#$00
		st1	#$20
.btm_loop:	st2	#$41
		st2	#$41
		dey
		bne	.btm_loop

		else

		ldx	#$10/2
		cly
		st1	#$20
.bat_loop:	st2	#$01
		st2	#$01
		dey
		bne	.bat_loop
		dex
		bne	.bat_loop

		endif

		rts



; ***************************************************************************
; ***************************************************************************
;
;

copy_palettes:	bsr	copy_palette
		dex
		bne	copy_palettes
		rts

copy_palette:	cly
.loop:		lda	[__ax],y
		iny
		sta	VCE_CTW+0
		lda	[__ax],y
		iny
		sta	VCE_CTW+1
		cpy	#32
		bne	.loop
		tya
		clc
		adc	<__al
		sta	<__al
		cla
		adc	<__ah
		sta	<__ah
		rts



; ***************************************************************************
; ***************************************************************************
;
; 0 = trans
; 1 = shadow
; 2 = font

tmp_shadow_buf	equ	$2100			; Interleaved 16 + 1 lines.
tmp_normal_buf	equ	$2101			; Interleaved 16 lines.

upload_font8x8:	ldx	#$11			; Upload solid version to
		lda	#$FF			; VRAM $1100-$17FF.
		bsr	.upload

		ldx	#$19			; Upload transparent version
		lda	#$00			; to VRAM $1900-$1FFF. 

.upload:	pha				; Preserve bit 2 & 3 fill.

		vreg	#VDC_MAWR		; Set VRAM destination.
		stz	VDC_DL
		stx	VDC_DH

		lda	#>$3000			; Set source font.
		sta	<__ax + 1
		stz	<__ax + 0

		vreg	#VDC_VWR
;		lda	#96			; ASCII =  96 characters.
		lda	#112			; ASCII =  96 characters + 16.
		sta	<__bl

		cly

.tile_loop:	clx				; Create a drop-shadowed version
		stz	tmp_shadow_buf,x	; of the glyph.

		.if	0
.line_loop:	lda	[__ax],y		; Drop-shadow on the LHS.
		sta	tmp_normal_buf,x	; Font data is RHS justified.
		asl	a
		.else
.line_loop:	lda	[__ax],y		; Drop-shadow on the RHS.
		sta	tmp_normal_buf,x	; Font data is LHS justified.
		lsr	a
		.endif

		ora	[__ax],y
		sta	tmp_shadow_buf+2,x
		ora	tmp_shadow_buf,x
		eor	tmp_normal_buf,x
		sta	tmp_shadow_buf,x

		iny
		bne	.next_line
		inc	<__ah
.next_line:	inx
		inx
		cpx	#2*8
		bne	.line_loop

.copy_tile:	tia	tmp_shadow_buf, VDC_DL, 16

		pla				; Restore bit 2 & 3 byte.
		ldx	#8
		sta	VDC_DL			; Solid uses colors 12-14.
.plane23_loop:	sta	VDC_DH			; Transparent uses 0-2.
		dex
		bne	.plane23_loop
		pha				; Preserve bit 2 & 3 byte.

		dec	<__bl
		bne	.tile_loop

		pla

.exit:		rts



; ***************************************************************************
; ***************************************************************************
;
; 0 = trans
; 1 = shadow
; 2 = font

		if	0

upload_font8x12:lda	#>$3000			; Set source font.
		sta	<__ax + 1
		stz	<__ax + 0

		stz	__bl			; ANK =  256 characters.

		vreg	#VDC_MAWR		; VRAM destination $1800.
		stz	VDC_DL
		lda	#$20
		sta	VDC_DH

		vreg	#VDC_VWR

		cly

		stz	tmp_shadow_buf
		tii	tmp_shadow_buf, tmp_shadow_buf+1, 31

.tile_loop:	ldx	#2*2			; Create a drop-shadowed version
		stz	tmp_shadow_buf+0,x	; of the glyph.

.line_loop:	lda	[__ax],y		; Drop-shadow on the RHS.
		sta	tmp_normal_buf+0,x	; Font data is LHS justified.
		lsr	a
		ora	[__ax],y
		sta	tmp_shadow_buf+2,x
		ora	tmp_shadow_buf+0,x
		eor	tmp_normal_buf+0,x
		sta	tmp_shadow_buf+0,x

		iny
		bne	.next_line
		inc	<__ah
.next_line:	inx
		inx
		cpx	#2*(2+12)
		bne	.line_loop

		tia	tmp_shadow_buf + $00, VDC_DL, 16
		bsr	.fill_plane23
		tia	tmp_shadow_buf + $10, VDC_DL, 16
		bsr	.fill_plane23

.next_tile:	dec	<__bl
		bne	.tile_loop

.exit:		rts

.fill_plane23:	ldx	#8
		st1	#$00			; Solid background,
.plane23_loop:	st2	#$FF			; uses colors 8-11.
		dex
		bne	.plane23_loop
		rts

		endif



; ***************************************************************************
; ***************************************************************************
;
; 0 = trans
; 1 = shadow
; 2 = font

upload_font8x16:lda	#>$3000			; Set source font.
		sta	<__ax + 1
		stz	<__ax + 0

		stz	__bl			; ANK =  256 characters.

		vreg	#VDC_MAWR		; VRAM destination $1800.
		stz	VDC_DL
		lda	#$20
		sta	VDC_DH

		vreg	#VDC_VWR

		cly

;		stz	tmp_shadow_buf
;		tii	tmp_shadow_buf, tmp_shadow_buf+1, 31

.tile_loop:	ldx	#2*0			; Create a drop-shadowed version
		stz	tmp_shadow_buf+0,x	; of the glyph.

.line_loop:	lda	[__ax],y		; Drop-shadow on the RHS.
		sta	tmp_normal_buf+0,x	; Font data is LHS justified.
		lsr	a
		ora	[__ax],y
		sta	tmp_shadow_buf+2,x
		ora	tmp_shadow_buf+0,x
		eor	tmp_normal_buf+0,x
		sta	tmp_shadow_buf+0,x

		iny
		bne	.next_line
		inc	<__ah
.next_line:	inx
		inx
		cpx	#2*(0+16)
		bne	.line_loop

		tia	tmp_shadow_buf + $00, VDC_DL, 16
		bsr	.fill_plane23
		tia	tmp_shadow_buf + $10, VDC_DL, 16
		bsr	.fill_plane23

.next_tile:	dec	<__bl
		bne	.tile_loop

.exit:		rts

.fill_plane23:	ldx	#8
		st1	#$00			; Solid background,
.plane23_loop:	st2	#$FF			; uses colors 8-11.
		dex
		bne	.plane23_loop
		rts




; ***************************************************************************
; ***************************************************************************
;
; 0 = blue background
; 1 = black shadow
; 2 = white font

cpc464_palette:	; 0 - Yellow on Blue, selectable.

		dw	$0000,$0001,$01B2,$01B2,$0000,$0000,$0000,$0000
		dw	$0001,$0000,$01B2,$01B2,$0002,$0000,$01B2,$01B2

		; 1 - White on Blue, selected (and pulsing).

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0001,$0000,$01B6,$01FF,$0002,$0000,$01B6,$01FF

		; 2 - Cyan on Blue, information

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0002,$0000,$01B2,$01B2,$0002,$0000,$0196,$0196

		; 3 - Cyan on Dark Blue, BRAM title.

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
;		dw	$0001,$0000,$01B2,$01B2,$0001,$0000,$0196,$0196
		dw	$0002,$0000,$0196,$0196,$0001,$0000,$0196,$0196

		; 4 - Yellow on Dark Grey, header.

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0049,$0000,$01B6,$01B6,$0049,$0000,$01B2,$01B2

		; 5- White on Dark Grey, headline.

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0049,$0000,$01B6,$01B6,$0049,$0000,$01B6,$01FF

		; 6 - Cyan on Dark Grey, help.

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0049,$0000,$01B6,$01B6,$0049,$0000,$0196,$0196



; ***************************************************************************
; ***************************************************************************
;
; Same colors, but different background to make the MB128 menu stand out.
;
; 0 = red background
; 1 = black shadow
; 2 = white font

red464_palette:	; 0 - Yellow on Blue, selectable.

		dw	$0000,$0001,$01B2,$01B2,$0000,$0000,$0000,$0000
		dw	$0008,$0000,$01B2,$01B2,$0010,$0000,$01B2,$01B2

		; 1 - White on Blue, selected (and pulsing).

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0008,$0000,$01B6,$01FF,$0010,$0000,$01B6,$01FF

		; 2 - Cyan on Blue, information

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0010,$0000,$01B2,$01B2,$0010,$0000,$0196,$0196

		; 3 - Cyan on Dark Blue, BRAM title.

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0010,$0000,$0196,$0196,$0008,$0000,$0196,$0196

		; 4 - Yellow on Dark Grey, header.

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0049,$0000,$01B6,$01B6,$0049,$0000,$01B2,$01B2

		; 5- White on Dark Grey, headline.

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0049,$0000,$01B6,$01B6,$0049,$0000,$01B6,$01FF

		; 6 - Cyan on Dark Grey, help.

		dw	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dw	$0049,$0000,$01B6,$01B6,$0049,$0000,$0196,$0196





; 110 11 0 000


; cyan
; 0145
; 0186
;
; 1 1001 0110


;110 110 010
;
;1 0110 1001



; 1 1111 1111 = $1FF
; 1 1011 0110 = $1B6
; 1 0110 1101 = $16D
; 1 0010 0100 = $124
; 0 1101 1011 = $0DB
; 0 1001 0010 = $092
; 0 0100 1001 = $049

; 1 0000 0100

; 1 0100 0101

; 1 0110 1111 = $16D



; ***************************************************************************
; ***************************************************************************
;
; tos_mul8u - Simple 16-bit x 8-bit multiply.
;
; Args: __ax = Multiplier / Result
; Args: __bl = Multiplier
;

tos_mul8u:	cla				; Clear Result.
 		clx
 		lsr 	<__bl			; Shift and test multiplier.
 		bcc 	.loop
.add: 		clc				; Add __ax to the Result.
 		adc 	<__al
		sax
		adc	<__ah
		sax
.loop:		asl	<__al			; __ax = __ax * 2
		rol	<__ah
 		lsr 	<__bl			; Shift and test multiplier.
 		bcs 	.add
 		bne 	.loop
		sta	<__al			; Save Result.
		stx	<__ah
		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_div16_7u  - Simple 16-bit /  7-bit divide.
; tos_div32_7u  - Simple 32-bit /  7-bit divide.
;
; Args: __ax = Dividend / Quotient
; Args: __bx = Dividend / Quotient (if 32-bit)
; Args: __cl = Dividor
; Uses: __dl = Remainder
;

tos_div16_7u:	ldx	#16
		cla				; Clear Remainder.
		asl	<__ax + 0		; Rotate Dividend, MSB -> C.
		rol	<__ax + 1
.loop:		rol	a			; Rotate C into Remainder.
		cmp	<__cl			; Test Divisor.
		bcc	.less			; CC if Divisor > Remainder.
		sbc	<__cl			; Subtract Divisor.
.less:		rol	<__ax + 0		; Quotient bit -> Dividend LSB.
		rol	<__ax + 1		; Rotate Dividend, MSB -> C.
		dex
		bne	.loop
		sta	<__dl			; Save the remainder.
		rts

		if	0
tos_div32_7u:	ldx	#32
		cla				; Clear Remainder.
		asl	<__ax + 0		; Rotate Dividend, MSB -> C.
		rol	<__ax + 1
		rol	<__ax + 2
		rol	<__ax + 3
.loop:		rol	a			; Rotate C into Remainder.
		cmp	<__cl			; Test Divisor.
		bcc	.skip			; CC if Divisor > Remainder.
		sbc	<__cl			; Subtract Divisor.
.skip:		rol	<__ax + 0		; Quotient bit -> Dividend LSB.
		rol	<__ax + 1		; Rotate Dividend, MSB -> C.
		rol	<__ax + 2
		rol	<__ax + 3
		dex
		bne	.loop
		sta	<__dl			; Save the remainder.
		rts
		endif



; ***************************************************************************
; ***************************************************************************
;
; tos_print_msg - A simple formatted-print routine for text output.
;
; Uses: __si =
; Uses: __di =
; Uses: __ax, __bx, __cx, __dx
;
; This is NOT "printf", so don't expect it to behave like it.
;
; But it provides a lot of similar functionality, with a different syntax.
;
; The escape sequences are ...
;
;   % [- | 0] [<width>] [h | l] u
;   % [- | 0] [<width>] [h | l] X
;   % [<width>] c
;   % s
;
;   % tx <byte>
;   % ty <byte>
;   % tt <word tile #>
;   % tp <hex>
;   % tb <byte w> <byte h>
;   % tr <addr>
;   % }                          push
;   % {                          pop
;   % <                          8-hi font
;   % >                          16-hi font
;

tos_print_msg:	stx	<__si + 0
		sta	<__si + 1

		tsx				; Preserve stack because
		stx	tos_tty_stack		; it can get unbalanced!

		stz	tos_tty_xyok		; Make sure VRAM addr is set!

.set_vram_delta:jsr	tos_tty_delta

.next_src:	jsr	.read_src
		cmp	#0
		beq	.finished
		cmp	#'%'
		beq	.escape
		cmp	#$0A
		beq	.got_lf
		cmp	#$0C
		beq	.got_ff
		cmp	#$0D
		beq	.got_cr

.got_chr:	jsr	tos_tty_write
		bra	.next_src

.got_lf:	lda	tos_tty_ypos
		inc	a
		and	#$1F
		sta	tos_tty_ypos
.got_cr:	lda	tos_tty_xlhs
		sta	tos_tty_xpos
		stz	tos_tty_xyok
		bra	.next_src

.got_ff:	jsr	clear_screen
		lda	tos_tty_xlhs
		sta	tos_tty_xpos
		lda	tos_tty_ytop
		sta	tos_tty_ypos
		stz	tos_tty_xyok
		bra	.set_vram_delta

.finished:	ldx	tos_tty_stack		; Restore stack position.
		txs

		rts

		; A "%" escape sequence.

.escape:	stz	tos_tty_outmin		; Default width.
		stz	tos_tty_outzer		; Pad with zero, not space.
		stz	tos_tty_outlhs		; Left justify, not right.

		; Read the format flag.

.read_flag:	jsr	.read_src		; Is there a flag?

.flag_minus:	cmp	#'-'			; Left-justify?
		bne	.flag_zero
		inc	tos_tty_outlhs
		bra	.read_width

.flag_zero:	cmp	#'0'			; Pad with zero?
		bne	.width_digit
		sta	tos_tty_outzer
;		bra	.read_width

		; Read the output width.

.read_width:	jsr	.read_src		; Is there a width?

.width_digit:	cmp	#'9'+1
		bcs	.read_length
		cmp	#'0'
		bcc	.read_length

		sbc	#'0'			; Read a decimal value for
		asl	tos_tty_outmin		; the width (0..255).
		adc	tos_tty_outmin
		asl	tos_tty_outmin
		asl	tos_tty_outmin
		adc	tos_tty_outmin
		sta	tos_tty_outmin
		bra	.read_width

		; Read the parameter length.

.read_length:	ldy	#2			; None = 2-byte parameter.

.length_half:	cmp	#'h'
		bne	.length_long
		ldy	#1			; Half = 1-byte parameter.
		bra	.read_specifier

.length_long:	cmp	#'l'
		bne	.specifier_pct
		ldy	#4			; Long = 4-byte parameter.
;		bra	.read_specifier

		; Read the output format specifier.

.read_specifier:jsr	.read_src

.specifier_pct:	cmp	#'%'			; Just print a '%'.
		bne	.specifier_u
		jmp	.got_chr

		;
		; Unsigned decimal integer output.
		;

.specifier_u:	cmp	#'u'
		bne	.specifier_X

		stz	<__ah
;		stz	<__bl
;		stz	<__bh

		clx
.decimal_val:	jsr	.read_param		; Read the desired # of bytes
		sta	<__ax,x			; into the dividend.
		inx
		dey
		bne	.decimal_val

		cla				; Push the output terminator.
		pha
		tsx				; Remember terminator position.
		stx	.decimal_self + 1

		lda	#10			; Divide by 10 and push the
		sta	<__cl			; remainder onto the stack.

.decimal_div16:	jsr	tos_div16_7u
		lda	<__dl
		clc
		adc	#'0'			; Always leaves C clr.
		pha
		lda	<__al			; Repeat while non-zero.
		ora	<__ah
		bne	.decimal_div16

		tsx				; Calc how many chrs are on
		txa				; the stack.
		eor	#$FF
		inc	a
		clc
.decimal_self:	adc	#$FF			; Always leaves C set.
		sbc	tos_tty_outmin		; Is there a minimum length?
		bcs	.decimal_min

		ldy	tos_tty_outlhs		; Is this left-justified?
		bne	.decimal_min

		ldy	tos_tty_outzer		; '0' or zero.
		bne	.decimal_pad_l
		ldy	#' '			; '0' or ' '.
.decimal_pad_l:	phy
		inc	a
		bne	.decimal_pad_l

.decimal_min:	sta	tos_tty_outmin		; Negative if pad RHS.

.decimal_pop:	pla				; Pop the digits and output.
		beq	.decimal_rhs
		jsr	tos_tty_write
		bra	.decimal_pop

.decimal_rhs:	ldy	tos_tty_outmin		; Negative if pad RHS.
		bpl	.decimal_done
.decimal_pad_r:	lda	#' '
		jsr	tos_tty_write
		iny
		bne	.decimal_pad_r

.decimal_done:	jmp	.next_src

		;
		; Hex integer output.
		;

.specifier_X:	cmp	#'X'
		bne	.specifier_c

		lda	tos_tty_stack		; Preserve current stack save.
		pha
		cla				; Push the output terminator.
		pha
		tsx				; Remember terminator position.
		stx	tos_tty_stack

.hex_val:	jsr	.read_param		; Read the desired # of bytes
		tax				; and push hex onto the stack.
		and	#$0F
		bsr	.hex_nibble
		pha
		txa
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		bsr	.hex_nibble
		pha
		dey
		bne	.hex_val

.hex_pop:	pla				; Pop the digits and output.
		beq	.hex_tail
		jsr	tos_tty_write
		bra	.hex_pop

.hex_tail:	pla				; Restore current stack save.
		sta	tos_tty_stack
		jmp	.next_src

.hex_nibble:	and	#$0F
		ora	#$30
		cmp	#$3A
		bcc	.hex_nibble_ok
		adc	#6
.hex_nibble_ok:	rts

		;
		; Chr (len is #chrs in array)
		;

.specifier_c:	cmp	#'c'
		bne	.specifier_s

		ldy	tos_tty_outmin		; Get the output length, with
		bne	.char_loop		; a minumum of 1.
		iny

.char_loop:	jsr	.read_param
		jsr	tos_tty_write
		dey
		bne	.char_loop
		jmp	.next_src

		;
		; String
		;

.specifier_s:	cmp	#'s'
		bne	.specifier_x

.str_loop:	jsr	.read_param
		cmp	#0
		beq	.str_done
		jsr	tos_tty_write
		bra	.str_loop
.str_done:	jmp	.next_src

		;
		; Set Cursor X coordinate.
		;

.specifier_x:	cmp	#'x'
		bne	.specifier_y
		jsr	.read_src
		cmp	#$40
		bcc	.set_xpos
		ora	#$20
		cmp	#'l'
.x_hang:	bne	.x_hang
		jsr	.read_src
		sta	tos_tty_xlhs
		bra	.x_done
.set_xpos:	sta	tos_tty_xpos
		stz	tos_tty_xyok
.x_done:	jmp	.next_src

		;
		; Set Cursor Y coordinate.
		;

.specifier_y:	cmp	#'y'
		bne	.specifier_r
		jsr	.read_src
		cmp	#$20
		bcc	.set_ypos
		ora	#$20
		cmp	#'t'
.y_hang:	bne	.y_hang
		jsr	.read_src
		sta	tos_tty_ytop
		bra	.y_done
.set_ypos:	sta	tos_tty_ypos
		stz	tos_tty_xyok
.y_done:	jmp	.next_src

		;
		; Change Memory Read Ptr
		;

.specifier_r:	cmp	#'r'
		bne	.escape7
		jsr	.read_src
		sta	<__di + 0
		jsr	.read_src
		sta	<__di + 1
		jmp	.next_src

		;
		; Push Memory Read Ptr
		;

.escape7:	cmp	#'}'
		bne	.escape8
		lda	<__di + 1
		pha
		lda	<__di + 0
		pha
		jmp	.next_src

		;
		; Pop Memory Read Ptr
		;

.escape8:	cmp	#'{'
		bne	.escape9
		pla
		sta	<__di + 0
		pla
		sta	<__di + 1
		jmp	.next_src

		;
		; Change Palette
		;

.escape9:	cmp	#'p'
		bne	.escape10
		lda	#$F0
		trb	tos_tty_tile + 1
		jsr	.read_hex
		asl	a
		asl	a
		asl	a
		asl	a
		tsb	tos_tty_tile + 1
		jmp	.next_src

		;
		; Change VRAM Base Tile + Palette
		;

.escape10:	cmp	#'t'
		bne	.escape11
		jsr	.read_src
		sta	tos_tty_tile + 0
		jsr	.read_src
		sta	tos_tty_tile + 1
		jmp	.next_src

		;
		; Set small (8-high) text.
		;

.escape11:	cmp	#'<'
		bne	.escape12
		stz	tos_tty_16hi
		stz	tos_tty_tile + 0	; Font base = VRAM $1000.
		lda	tos_tty_tile + 1
		and	#$F0
		ora	#$01
		sta	tos_tty_tile + 1
		jmp	.set_vram_delta

		;
		; Set large (16-high) text.
		;

.escape12:	cmp	#'>'
		bne	.escape13
		inc	tos_tty_16hi
		stz	tos_tty_tile + 0	; Font base = VRAM $2000.
		lda	tos_tty_tile + 1
		and	#$F0
		ora	#$02
		sta	tos_tty_tile + 1
		jmp	.set_vram_delta

		;
		; Draw a box.
		;

.escape13:	cmp	#'b'
		bne	.escape14
		jsr	.read_src		; Box width.
		sta	<__al
		jsr	.read_src		; Box height.
		sta	<__ah
		jsr	.read_src		; Box type.
		jsr	tos_draw_box
		jmp	.next_src

		; Illegal Value!!!

.escape14:	jmp	.finished
		bra	.escape14

		;

.read_hex:	jsr	.read_src
		cmp	#'9'+1
		bcs	.hex_gt_9
		sbc	#'0'-1
		bcc	.hex_bad
		rts

.hex_gt_9:	and	#$1F
		beq	.hex_bad
		cmp	#7
		bcs	.hex_bad
		adc	#9
		rts

.hex_bad:	bra	.hex_bad

		;

.read_src:	lda	[__si]
		inc	<__si + 0
		bne	.read_src_done
		inc	<__si + 1
.read_src_done:	rts

.read_param:	lda	[__di]
		inc	<__di + 0
		bne	.param_exit
		inc	<__di + 1
.param_exit:	rts

.write_nibble:	and	#$0F
		ora	#$30
		cmp	#$3A
		bcc	tos_tty_write
		adc	#6

tos_tty_write:	ldx	tos_tty_xyok
		bne	.write_8x16

.write_pos:	pha
		inc	tos_tty_xyok
		lda	tos_tty_ypos
		lsr	a
		tax
		cla
		ror	a
		ora	tos_tty_xpos
		php
		sei
		st0	#VDC_MARR
		sta	VDC_DL
		stx	VDC_DH
		st0	#VDC_MAWR
		sta	VDC_DL
		stx	VDC_DH
		st0	#VDC_VWR
		lda	#VDC_VWR
		sta	<vdc_reg
		plp
		pla

.write_8x16:	ldx	tos_tty_16hi
		clx
		beq	.write_8x8
		asl	a
		bcc	.write_top
		inx
.write_top:	bsr	.write_now
.write_btm:	inx
		stx	VDC_DL
		sta	VDC_DH
		stz	tos_tty_xyok
		rts

.write_8x8:	cmp	#$80			; Replace japanese characters
		bcc	.write_now		; when using in the 8x8 font.
		lda	#$7F

.write_now:	clc
		adc	tos_tty_tile + 0
		sta	VDC_DL
		sax
		adc	tos_tty_tile + 1
		sta	VDC_DH
		inc	tos_tty_xpos
		rts

tos_tty_delta:	lda	#VDC_CR
		sta	<vdc_reg
		st0	#VDC_CR
		lda	tos_tty_16hi		; Display 8-high or 16-high?
		beq	.set_delta		;  0 == increment by 1.
		lda	#%00011000		; NZ == increment by 128.
.set_delta:	sta	VDC_DH
		rts

		bss

tos_tty_stack:	ds	1
tos_tty_16hi:	ds	1
tos_tty_xyok:	ds	1
tos_tty_xlhs:	ds	1
tos_tty_ytop:	ds	1
tos_tty_xpos:	ds	1
tos_tty_ypos:	ds	1
tos_tty_tile:	ds	2

tos_tty_outmin:	ds	1			; Output width specifier.
tos_tty_outzer:	ds	1			; Pad with zero, not space.
tos_tty_outlhs:	ds	1                       ; Left justify, not right.

		code



; ***************************************************************************
; ***************************************************************************
;
; tos_set_pen - Set the pen color for tos_print_msg.
;

tos_set_pen:	pha
		lda	#$F0
		trb	tos_tty_tile + 1
		pla
		asl	a
		asl	a
		asl	a
		asl	a
		tsb	tos_tty_tile + 1
		rts



; ***************************************************************************
; ***************************************************************************
;
; Include library code.
;

		include	"sd.s"
		include	"fat32.s"
		include	"bram.s"
		include	"aplib.s"

apl_font_data:	incbin	"fonts.apl"



; ***************************************************************************
; ***************************************************************************
;
; RESET VECTORS (when booted as a cartridge, before starting as the TED OS)
;

;		ds	$9F80 - *

		org	$FF80

		;
		; If TEOS has been run as a cartridge image, then relocate it
		; into the same banks that the TED2 bootloader would run it.
		;

relocate_cart:	lda	#$01			; Set TED2 bootloader bank map.
		tam4				;
		inc	a			; MPR4 = bank $01.
		tam5                            ; MPR5 = bank $02.
		inc	a                       ; MPR6 = bank $03.
		tam6                            ; MPR7 = bank $04.
		inc	a
		tam7

                lda     #$A5			; TED2 takes over bank 0.
                sta     TED_BASE_ADDR + TED_REG_KEY1
		stz	TED_BASE_ADDR + TED_REG_CFG

		lda	#$4F			; Map TED bank 7 to low 512KB.
		sta	TED_BASE_ADDR + TED_REG_MAP

		if	REALHW
		lda	#$43			; MPR3 = cartridge bank $03.
		else
		lda	#$03			; MPR3 = cartridge bank $03.
		endif

		tam3
		tii	$6000,$E000,$2000	; Copy cart bank 3 to TED OS.
		dec	a
		tam3
		tii	$6000,$C000,$2000	; Copy cart bank 2 to TED OS.
		dec	a
		tam3
		tii	$6000,$A000,$2000	; Copy cart bank 1 to TED OS.

		dec	a			; Keep hardware information
		tam3				; from previous OS version.
		tii	$8106,(ted_assem_date & $1FFF) + $6000,8

		tii	$6000,$8000,$2000	; Copy cart bank 0 to TED OS.

		jmp	[$FFFE]			; Restart as TED OS.

cart_dummy_vec:	rti				; Just in case!

		ds	$FFD0-*			; Sanity Check.

		;
		; This string at this location tells both TEOS and Mednafen
		; that this HuCard ROM is TED2-aware, and to run the HuCard
		; with 1MB RAM enabled.
		;

		org	$FFD0

		db	"TED2CARD"
		dw	0			; Reserved word.
		dw	0			; Reserved word.

cart_reset:	sei
		csh				; Set high-speed mode.
		cld
		lda	#$FF			; MPR0 = bank $FF : PCE hardware
		tam0                            ; MPR1 = bank $F8 : PCE RAM
		tax				; MPR2 = bank $00 : TED2 hardware
		txs
		lda	#$F8
		tam1
		cla
		tam2

		tii	relocate_cart, $2200, $FFD0 - relocate_cart
		jmp	$2200

		ds	$FFF6-*			; Sanity Check.

		;
		;
		;

		org	$FFF6

		dw	cart_dummy_vec		; a.k.a. IRQ2
		dw	cart_dummy_vec		; a.k.a. IRQ1
		dw	cart_dummy_vec
		dw	cart_dummy_vec
		dw	cart_reset		; RESET
