; ***************************************************************************
; ***************************************************************************
;
; menu_mb128.s
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
; tos_bram_menu - TENNOKOE DS menu.
;

		;
		;
		;

msg_m128_init:	db	$0C
		db	"%>%p5%xl",0
		db	"%x",0
		db	"%y",0
		db	" MBASE128 SD: Initializing ... "
		db	"%<",$0A,$0A,$0A,0

msg_m128_none:	db	" Memory Base 128 not detected!",$0A,0



tbl_m128_title:	dw	msg_copy_mbram
		dw	msg_copy_mslot

cls_copy_mbram:	db	$0C
msg_copy_mbram:	db	"%>%p5%xl",0
		db	"%x",0
		db	"%y",0
		db	" MBASE128 SD: Copy Memory Base to SD    "
		db	"%<%p0",$0A,$0A,$0A,0

cls_copy_mslot:	db	$0C
msg_copy_mslot:	db	"%>%p5%xl",0
		db	"%x",0
		db	"%y",0
		db	" MBASE128 SD: Copy SD to Memory Base    "
		db	"%<%p0",$0A,$0A,$0A,0

		;
		;
		;

msg_m128_lhs:	db	"%<%xl",0

		db	"%y",2
		db	"%x",1
		db	"%b",18,19,0

		db	"%p3"
		db	"%y",3
		db	"%x",2
		db	" Name       Size"

		db	"%x",1
		db	"%y",21
		db	"%p0"
		db	"Console "
		db	"%p2"
		db	"%r"
		dw	tos_bram_files + 0
		db	"Files: %3hu",$0A
		db	"%r"
		dw	tos_bram_free + 0
		db	"%x",9
		db	"%y",22
		db	"Free: %4u"
		db	"%xl",2
		db	"%x",2
		db	"%y",4
		db	"%p0%>",0

		;
		;
		;

msg_m128_rhs:	db	"%<%xl",0

		db	"%y",2
		db	"%x",21
		db	"%b",18,19,0

		db	"%p3"
		db	"%y",3
		db	"%x",22
		db	" Name       Size"

		db	"%x",21
		db	"%y",21
		db	"%p0"
		db	"%r"
		dw	tos_bram_slot
		db	"Slot #%hu "
		db	"%p2"
		db	"%r"
		dw	tos_bram_files + 2
		db	"Files: %3hu",$0A
		db	"%r"
		dw	tos_bram_free + 2
		db	"%x",29
		db	"%y",22
		db	"Free: %4u"
		db	"%xl",22
		db	"%x",22
		db	"%y",4
		db	"%p0%>",0

		;
		;
		;

tbl_mbram_help:	dw	hlp_copy_mbram		; Help if box is selected.
		dw	hlp_copy_mslot

hlp_copy_mbram:	db	"%<%p5%xl",0
		db	"%x",0
		db	"%y",24
		db	"  SEL%p6:Chg Menu%p5  <>%p6:Chg Slot%p5  \\|%p6:Scroll %p5",$0A
		db	"    "
		db	30,31
		db	"%p6:Chg Mode%p5"
		db	" "
		db	28,29
		db	"%p6:Copy MB128 to Slot %p5",$0A
		db	"%y",23
		db	"%p0"

		db	"%xl",19
		db	"%x",19
		db	"%y",3
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A
		db	">>",$0A

		db	0

hlp_copy_mslot:	db	"%<%p5%xl",0
		db	"%x",0
		db	"%y",24
		db	"  SEL%p6:Chg Menu%p5  <>%p6:Chg Slot%p5  \\|%p6:Scroll %p5",$0A
		db	"    "
		db	30,31
		db	"%p6:Chg Mode%p5"
		db	" "
		db	28,29
		db	"%p6:Copy Slot to MB128 %p5",$0A
		db	"%y",23
		db	"%p0"

		db	"%xl",19
		db	"%x",19
		db	"%y",3
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A
		db	"<<",$0A

		db	0

		if	0

msg_small_index:db	"%-2hu",0
msg_large_index:db	"%02hu",0

msg_bram_larrow:db	$1C,0
msg_bram_rarrow:db	$1D,0

msg_bram_space:	db	" ",0

		if	0

msg_bram_name:	db	"%10c",0

msg_bram_size:	db	"%r"
		dw	tos_temp_length
		db	"%4u",$0A,$0A,0

		else

msg_bram_name:	db	"%10c %r"
		dw	tos_temp_length
		db	"%4u",$0A,$0A,0

		endif

msg_bram_spaces:db	"        "
		db	"        ",$0A,$0A,0

msg_blank_rhs:	db	"%<%xl",0

		db	"%y",2
		db	"%x",19
		db	"%p0"
		db	"%b",20,21,1

		db	"%p0%>",0

		endif

		;
		;
		;

		bss

tos_m128_slot:	ds	1
tos_m128_mode:	ds	1
tos_m128_focus:	ds	1			; Which side is focused?

tos_m128_files:	ds	2*2			; # of files on LHS/RHS.
tos_m128_least:	ds	2*2			; Minimum LHS/RHS file choice.
tos_m128_chosen:ds	2*2			; Current LHS/RHS file choice.
tos_m128_free:	ds	2*2			; BRAM unused on LHS/RHS.

tos_m128_first:	ds	1
tos_m128_choice:ds	1
tos_m128_index:	ds	1
tos_m128_hilite:ds	1

		code

		;
		;
		;

tos_m128_menu:	stz	tos_m128_mode
		stz	tos_m128_slot

		PUTS	msg_m128_init

		jsr	mb1_detect
		beq	tos_m128_menu2

		PUTS	msg_m128_none

.wait_input:	stz	joytrg
.wait_loop:	jsr	wait_vsync
		lda	joytrg
		beq	.wait_loop

		bit	#JOY_SEL
		beq	.wait_input
		jmp	tos_info_menu

		;
		;
		;

tos_m128_menu2:	jsr	clear_screen


;		PUTS	msg_bram_title

		PUTS	mb1_msg_init

		; Verify contents of BRAM_BANK.


		; Load up the MB128 directory.

		PUTS	mb1_msg_rdd_now

		lda	#BRAM_BANK
		sta	mb1_base_bank

		jsr	mb1_load_dir		; Load the directory.
		beq	.ok1

;		jmp	.finished





.ok1:

		PUTS	mb1_msg_rdd_ok



		tam3

		jsr	bm_bram_to_bank

		jsr	bm_verify_mpr3		; Verify the BRAM image.

		ldx	#0			; Save BRAM info.
		jsr	tos_m128_info

		; Verify contents of SLOT_BANK.

.new_slot:	stz	tos_hilite_idx		; Reset hilite pulsing.

		lda	#SLOT_BANK
		tam3

		if	REALHW

		jsr	tos_load_slot		; Load the file data.
		beq	.got_slot

		jsr	bm_format_mpr3		; Blank the slot.

		else

		tii	fake_bram1,$6000,$800

		endif

.got_slot:	jsr	bm_verify_mpr3		; Verify the BRAM image.
;		bmi	.unformatted

		ldx	#2			; Save BRAM info.
		jsr	tos_m128_info

		;

.new_mode:
		ldx	tos_m128_mode		; Lookup which side has focus.
		lda	.tbl_focus_side,x
		sta	tos_m128_focus
		lda	.tbl_min_choice,x	; Lookup minimum selection.
		sta	tos_m128_least + 0	; Set minimum selection.
		sta	tos_m128_least + 2
		sta	tos_m128_chosen + 0	; Set current selection.
		sta	tos_m128_chosen + 2

		cpx	#3			; Delete File mode?
		beq	.show_title

		ldx	#2
.calc_minimum:	lda	#8			; Start at the last file
		cmp	tos_m128_files,x	; shown on the screen so
		bcc	.set_minimum		; scrolling is immediate.
		lda	tos_m128_files,x
.set_minimum:	sta	tos_m128_least,x	; Set minimum selection.
		sta	tos_m128_chosen,x	; Set current selection.
		dex
		dex
		bpl	.calc_minimum

.show_title:	lda	tos_m128_mode		; Lookup which title to display.
		asl	a
		tay
		ldx	tbl_bram_title + 0,y
		lda	tbl_bram_title + 1,y
		jsr	tos_print_msg

		; Redraw the menu screen to reflect changes.

.show_help:	ldx	tos_m128_mode		; Lookup which help to display.
		ldy	.tbl_focus_side,x	; Is there a file selected on
		lda	tos_m128_chosen,y	; the side that has focus?
		sax
;		beq	.lookup_help
;		clc				; If so, use 2nd set of help
;		adc	#4			; messages.
.lookup_help:	asl	a
		tay
		ldx	tbl_bram_help + 0,y
		lda	tbl_bram_help + 1,y
		jsr	tos_print_msg

		; Display contents of BRAM_BANK.

.redraw_menu:

.show_bram:	if	1

		ldx	tos_m128_mode
		cpx	#3
		bne	.bram_box_color

		stz	tos_hilite_idx		; Reset hilite pulsing.

.bram_box_color:lda	.tbl_box_lhs,x
		jsr	tos_set_pen

		else

		lda	#1			; Always hilite the box in
		cmp	tos_m128_mode		; "Swap" mode.
		beq	.bram_box_color

		cla				; Otherwise, only hilite the
		ldx	tos_m128_chosen + 0	; box if no file is selected.
		bne	.bram_box_color
		ldx	tos_m128_mode
		lda	.tbl_box_lhs,x
.bram_box_color:jsr	tos_set_pen

		endif

		PUTS	msg_bram_lhs

		lda	#BRAM_BANK
		tam3

		ldx	#0
		jsr	tos_m128_show

		; Display contents of SLOT_BANK.

		lda	tos_m128_mode
		cmp	#3
		bne	.show_slot

		PUTS	msg_blank_rhs
		bra	.wait_input

.show_slot:
		if	1

		ldx	tos_m128_mode
		lda	.tbl_box_rhs,x
		jsr	tos_set_pen

		else

		lda	#1			; Always hilite the box in
		cmp	tos_m128_mode		; "Swap" mode.
		beq	.slot_box_color

		cla				; Otherwise, only hilite the
		ldx	tos_m128_chosen + 2	; box if no file is selected.
		bne	.slot_box_color
		ldx	tos_m128_mode
		lda	.tbl_box_rhs,x
.slot_box_color:jsr	tos_set_pen

		endif

		inc	tos_m128_slot
		PUTS	msg_bram_rhs
		dec	tos_m128_slot

		lda	#SLOT_BANK
		tam3

		ldx	#2
		jsr	tos_m128_show

		;

.wait_input:	stz	joytrg
.wait_loop:	jsr	wait_vsync
		lda	joytrg
		beq	.wait_loop

		bit	#JOY_SEL
		beq	.same_menu
		jmp	tos_info_menu

.same_menu:	bit	#JOY_L
		bne	.prev_slot
		bit	#JOY_R
		bne	.next_slot
		bit	#JOY_U
		bne	.prev_file
		bit	#JOY_D
		bne	.next_file
		bit	#JOY_B1
		bne	.select

.back_only:	bit	#JOY_B2
		beq	.wait_input
;		jmp	.chg_mode

		; Change Mode.

		lda	tos_m128_mode
		inc	a
		and	#3
		sta	tos_m128_mode
		jmp	.new_mode

		; Change Save Slot.

.prev_slot:	lda	tos_m128_slot
		dec	a
		and	#7
		sta	tos_m128_slot
		jmp	.new_slot

.next_slot:	lda	tos_m128_slot
		inc	a
		and	#7
		sta	tos_m128_slot
		jmp	.new_slot

		; Change Selected File.

.prev_file:	clx
		ldy	#2
.prev_loop:	lda	tos_m128_chosen,x	; Already at minimum selection?
		cmp	tos_m128_least,x
		bne	.check_other
;		lda	tos_m128_files,x	; Wrap around to end selection.
		bra	.set_prev_file
.check_other:	cmp	tos_m128_chosen,y	; Is the other side on a higher
		bcc	.set_prev_file		; selection?
		dec	a
.set_prev_file:	pha
		sxy
		txa
		bne	.prev_loop
.changed_choice:pla
		sta	tos_m128_chosen + 2
		pla
		sta	tos_m128_chosen + 0
		jmp	.redraw_menu

.next_file:	clx
		ldy	#2
.next_loop:	lda	tos_m128_chosen,x	; Already at maximum selection?
		cmp	tos_m128_files,x
		bne	.inc_next_file
		lda	tos_m128_chosen,y	; Is the other side on maximum?
		cmp	tos_m128_files,y
;		lda	tos_m128_least,x	; Wrap around to 1st selection.
;		bcs	.set_next_file
		lda	tos_m128_chosen,x	; Else stay on current choice.
		dec	a
.inc_next_file:	inc	a
.set_next_file:	pha
		sxy
		txa
		bne	.next_loop
		bra	.changed_choice

.select:	lda	tos_m128_mode
		asl	a
		tax
		jsr	.vector
		jmp	tos_m128_menu2

.vector:	jmp	[.tbl_funcs,x]

.tbl_funcs:	dw	func_copy_mbram
		dw	func_copy_mslot

.tbl_box_lhs:	db	1,1,0,0			; Palette for box per mode.
.tbl_box_rhs:	db	0,1,1,0			; Palette for box per mode.
.tbl_focus_side:db	0,4,2,0			; Focused side per mode.
.tbl_min_choice:db	0,0,0,1			; Minimum choice per mode.



; ***************************************************************************
; ***************************************************************************
;
; func_copy_bram -
;

func_copy_mbram:PUTS	cls_copy_mbram

		jsr	wait_for_key

		rts

func_copy_mslot:PUTS	cls_copy_mslot

		lda	#SLOT_BANK
		tam3

		jsr	bm_bank_to_bram

		jsr	wait_for_key

		rts

		;
		;
		;



; ***************************************************************************
; ***************************************************************************
;
; tos_m128_info - Save the info after a verify for later display.
;
; N.B. There can be a maximum of 126 files in the 2KB of BRAM.
;

tos_m128_info:	lda	<__bl
		sta	tos_m128_files + 0,x
		stz	tos_m128_files + 1,x

		sec
		lda	#<($6800 - $02)
		sbc	<__bp + 0
		tay
		lda	#>($6800 - $02)
		sbc	<__bp + 1
		bcs	.skip
		cla
		cly
.skip:		sta	tos_m128_free + 1,x
		tya
		sta	tos_m128_free + 0,x

		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_m128_show - Display the BRAM files in the current bank.
;
; N.B. There can be a maximum of 126 files in the 2KB of BRAM.
;

		if	0

tos_m128_show:	lda	tos_m128_chosen,x	; Decide which file to display
		sta	tos_m128_choice		; at the top of the box.
		bne	.non_zero
		inc	a
.non_zero:	sta	tos_m128_first
		clc
		adc	#7
		eor	#$FF
		sec
		adc	tos_m128_files,x
		bcs	.test_focus
		adc	tos_m128_choice
		bcs	.rewind_first
		lda	#1
.rewind_first:	sta	tos_m128_first

.first_to_show:

		else

tos_m128_show:	lda	tos_m128_chosen,x	; Decide which file to display
		sta	tos_m128_choice		; at the top of the box.
		sec
		sbc	#7
		bcs	.first_to_show
		lda	#1
.first_to_show:	sta	tos_m128_first

		endif

		if	0

.test_focus:	stz	tos_m128_hilite		; Set hilite color to normal.

		cpx	tos_m128_focus		; Is this side of the menu display
		bne	.first_in_bram		; in "focus"?

		inc	tos_m128_hilite		; Set hilite color to pulsing.

;		stz	tos_m128_choice		; Do not hilite any file.

		else

.test_focus:	lda	#1
		sta	tos_m128_hilite		; Set hilite color to pulsing.

		lda	tos_m128_mode		; Only hilite a file in Delete.
		cmp	#$3
		beq	.first_in_bram

		stz	tos_m128_hilite		; Set hilite color to normal.
		stz	tos_m128_choice		; Do not hilite any file.

		endif

.first_in_bram:	ldy	#<$6010
		lda	#>$6010

		stz	tos_m128_index

.file_loop:	inc	tos_m128_index		; Increment file index.

		sty	<__bp + 0		; Update file pointer.
		sta	<__bp + 1

		ldy	#1			; End of file chain?
		lda	[__bp],y
		sta	tos_temp_length + 1	; Size to display (hi-byte).
		tay
		ora	[__bp]
		bne	.not_eof

.bram_corrupt:	nop

		jmp	.rest			; EOF, remaining slots blank.

.not_eof:	lda	[__bp]
		sta	tos_temp_length + 0	; Size to display (lo-byte).
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

		ldx	tos_m128_index		; Skip off-screen files.
		cpx	tos_m128_first
		bcc	.file_loop

		phy				; Preserve next file pointer.
		pha

		cla				; Select the palette to use
		ldx	tos_m128_index		; for this file entry.
		cpx	tos_m128_choice
		bne	.file_color
		lda	tos_m128_hilite
.file_color:	jsr	tos_set_pen

		ldx	#<msg_small_index	; Display bottom 2 digits
		ldy	#>msg_small_index

		lda	tos_m128_index
		cmp	#100
		bcc	.show_index
		sbc	#100

		ldx	#<msg_large_index
		ldy	#>msg_large_index

.show_index:	sta	tos_temp_index		; Truncated to 2 digits.

		lda	#<tos_temp_index	; Display the file index.
		sta	<__di + 0
		lda	#>tos_temp_index
		sta	<__di + 1

;		tya
;		jsr	tos_print_msg

		ldx	#<msg_bram_space	; Display the LHS file cursor.
		ldy	#>msg_bram_space

		lda	tos_m128_index
		cmp	tos_m128_choice
		bne	.show_lhs_arrow

		ldx	#<msg_bram_larrow
		ldy	#>msg_bram_larrow

.show_lhs_arrow:tya
		jsr	tos_print_msg

.show_name:	lda	#$06			; Display the file name & size.
		clc
		adc	<__bp + 0
		sta	<__di + 0
		cla
		adc	<__bp + 1
		sta	<__di + 1

		PUTS	msg_bram_name

		if	0

		ldx	#<msg_bram_space	; Display the RHS file cursor.
		ldy	#>msg_bram_space

		lda	tos_m128_index
		cmp	tos_m128_choice
		bne	.show_rhs_arrow

		ldx	#<msg_bram_rarrow
		ldy	#>msg_bram_rarrow

.show_rhs_arrow:tya
		jsr	tos_print_msg

		PUTS	msg_bram_size

		endif

		pla				; Restore next file pointer.
		ply

		ldx	tos_tty_ypos		; Display the rest of the
		cpx	#20                     ; BRAM files in the box.
		bcs	.done

		jmp	.file_loop

.rest:		lda	tos_tty_ypos		; Blank out the rest of the
		cmp	#20			; BRAM files in the box.
		bcs	.done

		PUTS	msg_bram_spaces
		bra	.rest

.done:		rts
