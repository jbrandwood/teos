; ***************************************************************************
; ***************************************************************************
;
; menu_mb128.s
;
; TEOS Menu Screens
;
; Copyright John Brandwood 2019-2022.
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
; tos_m128_menu - Text messages for the Memory Base SD menu.
;

msg_m128_init:	db	"%>%p5%xl",0
		db	$0C
		db	" MEMORY BASE SD: Initializing ... "
		db	"%<",$0A,$0A,$0A,0

msg_m128_none:	db	" Memory Base 128 not detected!",$0A,0

		;
		;
		;

tbl_m128_title:	dw	msg_m128_save
		dw	msg_m128_load

cls_m128_save:	db	$0C
msg_m128_save:	db	"%>%p5%xl",0
		db	"%x",0
		db	"%y",0
		db	" MEMORY BASE SD: Copy MB128 to SD "
		db	"%<%p0",$0A,$0A,$0A,0

cls_m128_load:	db	$0C
msg_m128_load:	db	"%>%p5%xl",0
		db	"%x",0
		db	"%y",0
		db	" MEMORY BASE SD: Copy SD to MB128 "
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
;		db	"Console "
		db	"MB128   "
		db	"%p2"
		db	"%r"
		dw	tos_m128_files + 0
		db	"Files: %3hu",$0A
		db	"%r"
		dw	tos_m128_free + 0
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
		dw	tos_m128_slot
		db	"Slot #%hu "
		db	"%p2"
		db	"%r"
		dw	tos_m128_files + 2
		db	"Files: %3hu",$0A
		db	"%r"
		dw	tos_m128_free + 2
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

tbl_m128_help:	dw	hlp_copy_mb128		; Help if box is selected.
		dw	hlp_copy_mslot

hlp_copy_mb128:	db	"%<%p5%xl",0
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

		;

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

		;
		;
		;

msg_m128_name:	db	"%8c   %r"
		dw	tos_temp_length
		db	"%4u",$0A,$0A,0

		; USE COPIES FROM BRAM MENU

		if	0

msg_small_index:db	"%-2hu",0
msg_large_index:db	"%02hu",0

msg_bram_larrow:db	$1C,0
msg_bram_rarrow:db	$1D,0

msg_bram_space:	db	" ",0

msg_bram_name:	db	"%10c %r"
		dw	tos_temp_length
		db	"%4u",$0A,$0A,0

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
tos_m128_free:	ds	2*2			; Blocks unused on LHS/RHS.

tos_m128_first:	ds	1
tos_m128_choice:ds	1
tos_m128_index:	ds	1
tos_m128_hilite:ds	1

		code



; ***************************************************************************
; ***************************************************************************
;
; tos_m128_menu - Memory Base SD menu.
;

tos_m128_menu:	stz	tos_m128_mode
		stz	tos_m128_slot

		if	REALHW

		PUTS	msg_m128_init

		jsr	mb1_detect		; Skip if there is no MB128.
		beq	tos_m128_menu2

		endif

		jmp	tos_hucard_menu

tos_m128_menu2:	jsr	clear_screen

;		PUTS	msg_bram_title
;		PUTS	mb1_msg_init

		; Verify contents of BRAM_BANK.

		; Load up the MB128 directory.

;		PUTS	mb1_msg_rdd_now

		lda	#BRAM_BANK
		sta	mb1_base_bank

		jsr	mb1_load_dir		; Load the directory.
		beq	.m128_info

.bad_m128:	lda	#BRAM_BANK		; Format the MB128 image.
		jsr	mb1_new_image

.m128_info:	ldx	#0			; Save MB128 info.
		jsr	tos_m128_info

		; Verify contents of SLOT_BANK.

.new_slot:	stz	tos_hilite_idx		; Reset hilite pulsing.

		lda	#SLOT_BANK		; Load the file data.
		jsr	tos_load_m128
		bne	.bad_slot

.got_slot:	jsr	mb1_check_dir		; Verify the MB128 image.
		beq	.slot_info		; Is it OK?

.bad_slot:	lda	#SLOT_BANK		; Format the MB128 image.
		jsr	mb1_new_image

.slot_info:	ldx	#2			; Save SLOT info.
		jsr	tos_m128_info

.if 0
		lda	#BRAM_BANK
		tam3

		lda	#<mb1_directory
		ldx	#>mb1_directory
		jsr	show_page

		jmp	!-
.endif

;

.new_mode:	ldx	tos_m128_mode		; Lookup which side has focus.
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
		ldx	tbl_m128_title + 0,y
		lda	tbl_m128_title + 1,y
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
		ldx	tbl_m128_help + 0,y
		lda	tbl_m128_help + 1,y
		jsr	tos_print_msg

		; Display contents of BRAM_BANK.

.show_m128:	ldx	tos_m128_mode
		cpx	#3
		bne	.m128_box_color

		stz	tos_hilite_idx		; Reset hilite pulsing.

.m128_box_color:lda	.tbl_box_lhs,x
		jsr	tos_set_pen

		PUTS	msg_m128_lhs

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

.show_slot:	ldx	tos_m128_mode
		lda	.tbl_box_rhs,x
		jsr	tos_set_pen

		inc	tos_m128_slot
		PUTS	msg_m128_rhs
		dec	tos_m128_slot

		lda	#SLOT_BANK
		tam3

		ldx	#2
		jsr	tos_m128_show

		;

.wait_input:	stz	joytrg
.wait_loop:	jsr	wait_vsync_usb
		lda	joytrg
		beq	.wait_loop

		bit	#JOY_SEL
		beq	.same_menu
		jmp	tos_hucard_menu

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
;		and	#3
		and	#1
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
		jmp	.show_m128

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

.tbl_funcs:	dw	func_m128_save
		dw	func_m128_load

.tbl_box_lhs:	db	1,1,0,0			; Palette for box per mode.
.tbl_box_rhs:	db	0,1,1,0			; Palette for box per mode.
.tbl_focus_side:db	0,4,2,0			; Focused side per mode.
.tbl_min_choice:db	0,0,0,1			; Minimum choice per mode.



; ***************************************************************************
; ***************************************************************************
;
; tos_m128_info - Save the info after a verify for later display.
;
; N.B. There can be a maximum of 63 files in the MB128.
;

tos_m128_info:	lda	mb1_file_count
		sta	tos_m128_files + 0,x
		stz	tos_m128_files + 1,x

.m128_info:	sec
		cla
		sbc	mb1_directory + MB1_HEAD_USED + 0
		sta	tos_m128_free + 0,x
		lda	#1
		sbc	mb1_directory + MB1_HEAD_USED + 1
		sta	tos_m128_free + 1,x

		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_m128_show - Display the BRAM files in the current bank.
;
; N.B. There can be a maximum of 63 files in the MB128.
;

tos_m128_show:	lda	tos_m128_chosen,x	; Decide which file to display
		sta	tos_m128_choice		; at the top of the box.
		sec
		sbc	#7
		bcs	.first_to_show
		lda	#1
.first_to_show:	sta	tos_m128_first

.test_focus:	lda	#1
		sta	tos_m128_hilite		; Set hilite color to pulsing.

		lda	tos_m128_mode		; Only hilite a file in Delete.
		cmp	#$3
		beq	.first_in_m128

		stz	tos_m128_hilite		; Set hilite color to normal.
		stz	tos_m128_choice		; Do not hilite any file.

.first_in_m128:	ldy	#<mb1_directory + 16	; Start at the first file.
		lda	#>mb1_directory + 16

		stz	tos_m128_index

.file_loop:	inc	tos_m128_index		; Increment file index.

		cpy	#>mb1_directory + 1024	; Is it beyond directory?
		bcs	.rest			; EOD, remaining slots blank.

		sty	<__bp + 0		; Update file pointer.
		sta	<__bp + 1

.not_eof:	lda	[__bp]			; Sector# or zero at end.
		beq	.rest

		ldy	#MB1_FILE_SIZE
		lda	[__bp],y
		sta	tos_temp_length + 0	; Size to display (lo-byte).
		stz	tos_temp_length + 1	; Size to display (hi-byte).

		lda	#$10			; Calc address of next file.
		clc
		adc	<__bp + 0
		say
		cla
		adc	<__bp + 1

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

		if	0

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

		endif

		ldx	#<msg_bram_space	; Display the LHS file cursor.
		ldy	#>msg_bram_space

		lda	tos_m128_index
		cmp	tos_m128_choice
		bne	.show_lhs_arrow

		ldx	#<msg_bram_larrow
		ldy	#>msg_bram_larrow

.show_lhs_arrow:tya
		jsr	tos_print_msg

.show_name:	lda	#MB1_FILE_NAME		; Display the file name & size.
		clc
		adc	<__bp + 0
		sta	<__di + 0
		cla
		adc	<__bp + 1
		sta	<__di + 1

		PUTS	msg_m128_name

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



; ***************************************************************************
; ***************************************************************************
;
; func_m128_save - Copy MB128 to SD card slot.
;

func_m128_save:	PUTS	cls_m128_save		; Confirm the operation.
		inc	tos_m128_slot		; Show slot from '1' not '0'.
		PUTS	.msg_warning
		dec	tos_m128_slot

.wait_input:	stz	joytrg			; Wait for input.
.wait_loop:	jsr	wait_vsync_usb
		lda	joytrg
		beq	.wait_loop
		bit	#JOY_RUN
		bne	.confirmed
		bit	#JOY_B2
		beq	.wait_input
		rts

.confirmed:	PUTS	cls_m128_save		; Inform the user.

		PUTS	.msg_load

		lda	#BRAM_BANK		; Load the MB128 image.
		jsr	mb1_load_image
		bne	.failed

		PUTS	.msg_save

		lda	#BRAM_BANK		; Save image to the SD card.
		jsr	tos_save_m128
		bne	.failed

		PUTS	msg_m128_done
		bra	.result

.failed:	PUTS	msg_m128_fail

.result:	PUTS	msg_press_a_key
		jsr	wait_for_key

.finished:	rts

.msg_warning:	db	"%r"
		dw	tos_m128_slot
		db	"%y",11
		db	"  Please press %p1RUN%p0 to confirm that you",$0A
;		db	"  want to copy the MB128 to SD Slot %02hu!",$0A
		db	"  want to copy the MB128 to SD Slot #%hu!",$0A
		db	"%y",24
		db	"%x",12
		db	"%p5RUN%p6:Confirm copy",$0A
		db	"%x",13
		db	"%p5",30,31,"%p6:Cancel copy",$0A
		db	"%p0%y",14,0

.msg_load:	db	" Loading all MB128 files.",$0A,$0A,0
.msg_save:	db	" Saving files to SD card.",$0A,$0A,0



; ***************************************************************************
; ***************************************************************************
;
; func_m128_load - Copy SD card slot to MB128.
;

func_m128_load:	PUTS	cls_m128_load
		inc	tos_m128_slot		; Show slot from '1' not '0'.
		PUTS	.msg_warning
		dec	tos_m128_slot

.wait_input:	stz	joytrg			; Wait for input.
.wait_loop:	jsr	wait_vsync_usb
		lda	joytrg
		beq	.wait_loop
		bit	#JOY_RUN
		bne	.confirmed
		bit	#JOY_B2
		beq	.wait_input
		rts

.confirmed:	PUTS	cls_m128_load		; Inform the user.

		PUTS	.msg_save

		lda	#SLOT_BANK		; Save the MB128 image.
		jsr	mb1_save_image
		bne	.failed

		PUTS	msg_m128_done
		bra	.result

.failed:	PUTS	msg_m128_fail

.result:	PUTS	msg_press_a_key
		jsr	wait_for_key

.finished:	rts

.msg_warning:	db	"%r"
		dw	tos_m128_slot
		db	"%y",11
		db	"  Please press %p1RUN%p0 to confirm that you",$0A
;		db	"  want to copy SD Slot %02hu to the MB128!",$0A
		db	"  want to copy SD Slot #%hu to the MB128!",$0A
		db	"%y",24
		db	"%x",12
		db	"%p5RUN%p6:Confirm copy",$0A
		db	"%x",13
		db	"%p5",30,31,"%p6:Cancel copy",$0A
		db	"%p0%y",14,0

.msg_save:	db	" Saving files to MB128.",$0A,$0A,0

msg_m128_fail:	db	" MB128 transfer failed!",$0A,$0A,0
msg_m128_done:	db	" MB128 transfer completed!",$0A,$0A,0