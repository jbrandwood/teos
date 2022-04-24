		.include  "pce.inc"
		.include  "macro.inc"
		.include  "ted2.inc"
		.include  "sd.inc"

		.export	  _text
		.export	  _ed_test

		.exportzp _view
		.exportzp _line
		.export	  _show_page
		.exportzp _string
		.export	  _show_string

								.importzp	_ed_data_ptr


_al = $20f8

member = $04

		.zeropage

		.feature  bracket_as_indirect

_string:	.word	0
_view:		.word	0
_line:		.byte	0

		.data

		.segment "SDCODE"

_text:		.asciiz "Hello world!"

_read_sect_0:		.asciiz	"read_sector_0"
_read_sect_1:		.asciiz	"read_sector_1"

	.macro	PUTS	addr
		ldx	#<addr
		lda	#>addr
		jsr	_show_string
	.endmacro

;
;
;

_ed_test:	cld

		stz	_line
		jsr	_clear_screen

		php

		TED_UNLOCK
		TED2_SET_CFG_1MB_RAM

		jsr	_clear_ram

;
;
;

@skip_sf2:	TED_SET_CFG_FPGA

		lda	#$6c ; 767KB
		tam	#$40 ; $C000-$DFFF

		jsr	_disk_init

		stx	$c004
		stw	#$c000, _view
		jsr	_show_page
		stw	#$d000, _view
		jsr	_show_page
		stw	#$d1f0, _view
		jsr	_show_page

		PUTS		_read_sect_0

		lda	#$01
		sta	<_ed_sect_cnt
		stw	#$0000, _ed_dsk_addr+0
		stw	#$0000, _ed_dsk_addr+2
		stw	#$d000, _ed_data_ptr

		jsr	_disk_read_sector

		stx	$c006
		stw	#$c000, _view
		jsr	_show_page
		stw	#$d1be, _view
		jsr	_show_page
		stw	#$d1f0, _view
		jsr	_show_page

		stw	#$d200, _view
		jsr	_show_page
		stw	#$d3f0, _view
		jsr	_show_page

		PUTS		_read_sect_1

		lda	#$03
		sta	<_ed_sect_cnt
;		 stw	 #$0800, _ed_dsk_addr+0
		stw	#$0a3a, _ed_dsk_addr+0
		stw	#$0000, _ed_dsk_addr+2
		stw	#$d000, _ed_data_ptr

		jsr	_disk_read_sector

		stx	$c006
		stw	#$c000, _view
		jsr	_show_page
		stw	#$d000, _view
		jsr	_show_page
		stw	#$d1f0, _view
		jsr	_show_page

		stw	#$d200, _view
		jsr	_show_page
		stw	#$d3f0, _view
		jsr	_show_page

		stw	#$d400, _view
		jsr	_show_page
		stw	#$d5f0, _view
		jsr	_show_page

@hang2:		bra	@hang2

;		jsr	_disk_read_sector

		stx	$c006
		stw	#$c000, _view
		jsr	_show_page

		stw	#$d000, _view
		jsr	_show_page

		lda	#$01
		sta	<_ed_sect_cnt
		stw	#$0000, _ed_dsk_addr+0
		stw	#$0000, _ed_dsk_addr+2
		stw	#$d000, _ed_data_ptr

		jsr	_disk_read_sector

		stw	#$d000, _view
		jsr	_show_page

		lda	#$00 ; 767KB
		tam	#$40 ; $C000-$DFFF

		TED2_SET_CFG_1MB_RAM

@hang:		bra	@hang

		jmp	[$fffe]

		jmp	[$fffe,x]

		plp
		rts

;
;
;

_clear_ram:	lda	#$08
		clx

		ldy	#$ff
		ldy	#$00
		ldy	#$88

@clear_loop:	tam	#$40 ; $C000-$DFFF
		sta	$C000
		stx	$C001
		sta	$C002
		stx	$C003
		sty	$C004
		tii	$C004,$C005,$1ffb
		inc	a
		cmp	#$80
		bne	@clear_loop
		rts		

;
;
;

_clear_screen:	php
		sei
		st0	#VDC_MAWR
		st1	#$00
		st2	#$00
		st0	#VDC_VWR
		ldx	#$10 ; $0000-$1FFF
		cly
		st1	#$00
@word_loop:	st2	#$00
		dey
		bne	@word_loop
		dex
		bne	@word_loop
		plp
		rts
		
;
;
;

_show_page:	cld
		cly

@line_loop:	st0	#VDC_MAWR

		lda	_line
		lsr	a
		tax
		cla
		ror	a
		sta	VDC_DATA_LO
		stx	VDC_DATA_HI

		st0	#VDC_VWR

		phy

@byte_loop:	lda	[_view],y
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		ora	#$30
		cmp	#$3a
		bcc	@skip_lo
		adc	#6
@skip_lo:	sta	VDC_DATA_LO
		st2	#$12

		lda	[_view],y
		and	#$0f
		ora	#$30
		cmp	#$3a
		bcc	@skip_hi
		adc	#6
@skip_hi:	sta	VDC_DATA_LO
		st2	#$12

		iny
		tya
		bit	#$01
		bne	@skip_space

		st1	#$20
		st2	#$12

@skip_space:	bit	#$0f
		bne	@byte_loop

		ply

@char_loop:	lda	[_view],y
		cmp	#$20
		bcc	@non_ascii
		cmp	#$7f
		bcc	@show_char
@non_ascii:	lda	#'.'
@show_char:	sta	VDC_DATA_LO
		st2	#$12

		iny
		tya
		bit	#$0f
		bne	@char_loop

		inc	_line
		cpy	#$40
		cpy	#$10
		bne	@line_loop

;		inc	_line

		rts


;
;
;

_show_string:	stx	_string
		sta	_string+1

		st0	#VDC_MAWR

		lda	_line
		lsr	a
		tax
		cla
		ror	a
		sta	VDC_DATA_LO
		stx	VDC_DATA_HI

		st0	#VDC_VWR

		cly

@char_loop:	lda	[_string],y
		beq	@finished
		sta	VDC_DATA_LO
		st2	#$12
		iny
		bra	@char_loop

@finished:	inc	_line

		rts
