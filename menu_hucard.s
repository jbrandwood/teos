; ***************************************************************************
; ***************************************************************************
;
; menu_hucard.s
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

msg_main_title:	db	$0C
		db	"%>%p5"
		db	" RUN HuCARD"
		db	"%<%p0"
		db	$0A,$0A,$0A,0

msg_main_help:	db	"%p5"
		db	"%y",24

		if	HIRES
		db	"%x",9			; 480 wide
		endif

;		db	"----------------------------------------"
		db	" SEL%p6:Chg Menu%p5  <>%p6:Chg Page%p5  \\|%p6:Chg File%p5",$0A

		if	HIRES
		db	"%x",9			; 480 wide
		endif

		db	"   "
		db	30,31
;		db	"%p6:Parent%p5  I%p6:Select%p5",$0A

		db	"%p6:Parent%p5 "
		db	28,29
		db	"%p6:Select File or Folder%p5",$0A,0

msg_main_pageno:db	"%p5"
		db	"%y",1

		if	HIRES
		db	"%x",26+18		; 480 wide
		else
		db	"%x",26			; 320 wide
		endif

		db	"%s"
		db	"%r"
		dw	tos_show_page
		db	"%hu of "
		db	"%r"
		dw	tos_num_pages
		db	"%hu"
		db	"%p0"
		db	$0A,$0A,0

str_page:	db	"  Page ",0

msg_empty_dir:	db	"%p2 This directory is empty!%p0",0

		;
		;
		;

tos_hucard_menu:
		if	HIRES
		ldx	#<video_mode_480	; Change to hi-res screen.
		lda	#>video_mode_480
		jsr	tos_screen_mode
		endif

		PUTS	msg_main_title
		PUTS	msg_main_help

		jsr	f32_clear_cache		; Don't assume that it's valid!

		ldx	#15			; Is this the same SDcard as
.cmp_sdcard:	lda	sdc_cid_value,x		; the last time?
		cmp	tos_cur_cardid,x
		bne	.new_sdcard
		dex
		bpl	.cmp_sdcard

.old_sdcard:	tii	tos_cur_folder, f32_dir_cluster, 4
		bra	.new_directory

.new_sdcard:	tii	sdc_cid_value, tos_cur_cardid, 16

		stz	tos_cur_depth
		tai	tos_zero, tos_cur_page, (tos_cur_file - tos_cur_page) * 2

		jsr	f32_select_root		; New card resets to root dir.

.new_directory:	tii	f32_dir_cluster, tos_cur_folder, 4

		jsr	tos_scan_dir		; Find selectable entries.

.new_page:	lda	#FILES_PER_PAGE		; Calc file index of the 1st
		sta	<__bl			; entry on this page.
		ldx	tos_cur_depth
		lda	tos_cur_page,x
		sta	<__al
		stz	<__ah
		jsr	tos_mul8u

		jsr	tos_fastfwd_dir		; Skip the directory forward.

		ldx	tos_cur_depth		; Get the current page # to
		lda	tos_cur_page,x		; show onscreen.
		ldy	tos_num_pages
		beq	.skip0		
		inc	a
.skip0:		sta	tos_show_page		; Move where "Page" is shown
		cmp	#10                     ; depending upon if either
		lda	#<str_page              ; value is single or double
		ldx	#>str_page              ; digits.
		bcc	.skip1
		inc	a
		bne	.skip1
		inx
.skip1:		cpy	#10
		bcc	.skip2
		inc	a
		bne	.skip2
		inx
.skip2:		sta	<__di + 0
		stx	<__di + 1

;		PUTS	msg_main_title
;		PUTS	msg_main_help
		PUTS	msg_main_pageno

		ldx	tos_num_pages		; Any files in this directory?
		beq	.empty_dir

		jsr	tos_show_files		; Display the page of files.

		ldx	tos_cur_depth		; Make sure that the selection
		lda	tos_cur_file,x		; is available on this page.
		cmp	tos_num_files
		bcc	.selection_ok
		lda	tos_num_files
		dec	a
		sta	tos_cur_file,x
.selection_ok:	jsr	tos_set_hilite

.new_file:	bra	.wait_input

.empty_dir:	PUTS	msg_empty_dir

.wait_input:	stz	joytrg
.wait_loop:	jsr	wait_vsync
		lda	joytrg
		beq	.wait_loop

		bit	#JOY_SEL
		beq	.same_menu

		if	HIRES
		ldx	#<video_mode_320	; Change to medium-res screen.
		lda	#>video_mode_320
		jsr	tos_screen_mode
		endif

		jmp	tos_bram_menu

.same_menu:	ldx	tos_num_pages
		beq	.back_only

		bit	#JOY_L
		bne	.prev_page
		bit	#JOY_R
		bne	.next_page
		bit	#JOY_U
		bne	.prev_file
		bit	#JOY_D
		bne	.next_file
		bit	#JOY_B1
		bne	.select

.back_only:	bit	#JOY_B2
		beq	.wait_input
		jmp	.parent

		; Change Page in current directory.

.prev_page:	ldx	tos_cur_depth
		lda	tos_cur_page,x
		bne	.dec_page
		lda	tos_num_pages
.dec_page:	dec	a
.chk_page:	cmp	tos_cur_page,x
		beq	.wait_input
		sta	tos_cur_page,x
		bsr	.clr_child_pos
		jmp	.new_page

.next_page:	ldx	tos_cur_depth
		lda	tos_cur_page,x
		inc	a
		cmp	tos_num_pages
		bcc	.chk_page
		cla
		bra	.chk_page

		; Change File in current directory.

.prev_file:	ldx	tos_cur_depth
		lda	tos_cur_file,x
		bne	.dec_file
		lda	tos_num_files
.dec_file:	dec	a
.chk_file:	cmp	tos_cur_file,x
		beq	.wait_input
		pha
		jsr	tos_clr_hilite
		ldx	tos_cur_depth
		pla
		sta	tos_cur_file,x
		jsr	tos_set_hilite
		bsr	.clr_child_pos
		jmp	.new_file

.next_file:	ldx	tos_cur_depth
		lda	tos_cur_file,x
		inc	a
		cmp	tos_num_files
		bcc	.chk_file
		cla
		bra	.chk_file

		;

.clr_child_pos: ldx	tos_cur_depth		; Reset position in child dir.
		stz	tos_cur_page + 1,x	; Done here so that reentering
		stz	tos_cur_file + 1,x	; the child dir keeps position.
		rts

.invalid:	jmp	.wait_input

		;

.select:	ldx	tos_cur_depth
		lda	tos_cur_file,x
		tay
		asl	a
		asl	a
		tax
		lda	tos_file_type,y
		beq	.file

.directory:	ldy	tos_cur_depth		; Limit directory depth to 32.
		iny
		cpy	#32
		beq	.invalid
		sty	tos_cur_depth

		cly
.dir_loop:	lda	tos_1st_cluster,x
		sta	f32_dir_cluster,y
		inx
		iny
		cpy	#4
		bne	.dir_loop

		jsr	f32_dir_chosen
		beq	.changed

		stz	tos_cur_depth
		jsr	f32_select_root

.changed:	jmp	.new_directory

		;

.parent:	stw	#dotdot,__ax		; Find the ".." directory entry.
		jsr	f32_find_name
		bne	.invalid

		jsr	f32_change_dir
		bne	.invalid

		dec	tos_cur_depth
		jmp	.new_directory

		;

.file:		cly				; Copy file's initial cluster
.file_loop:	lda	tos_1st_cluster,x	; and total length.
		sta	f32_fil_cluster,y
		lda	tos_file_length,x
		sta	f32_file_length,y
		inx
		iny
		cpy	#4
		bne	.file_loop

		jsr	f32_file_chosen		; Open the file using only the
		bne	.load_fail		; cluster/length, not a name.

		PUTS	msg_main_title

		jsr	tos_load_hucard		; Load the current file as
		beq	.load_good		; a HuCard image.

.load_fail:	PUTS	file_failed
		bra	.hang2

.load_good:	jmp	tos_exec_hucard

.hang:
.hang2:		bra	.hang2


file_loaded:	db	" File loaded!",$0A,0

file_failed:	db	" File error!",$0A,0

dotdot:		db	"..",0

;
;
;

.hang:		jsr	wait_vsync
		bra	.hang
