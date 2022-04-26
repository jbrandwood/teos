; ***************************************************************************
; ***************************************************************************
;
; mb128.s
;
; Functions for using an "MB128" or "Save Kun" backup-memory device.
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
; The MB128 is built around an MSM6389 1Mbit (128KByte) serial memory chip.
;
; That memory chip is accessed by the PC Engine one bit at a time, through
; the joypad port.
;
; The MB128 normally allows the joypad port's signals to directly pass
; through to any attached joypad/mouse/multitap devices.
;
; When it detects a specific "wakeup" sequence on the joypad port, it takes
; control of the port and locks-out the other devices.
;
; You then send the MB128 a command, and when the command is finished, the
; MB128 releases the joypad port, and waits for the next "wakeup" sequence.
;
;
; The MB128 only responds to two commands, READ BITS and WRITE BITS.
;
; READ BITS
;
;  1 <addr> <size> ... MB128 then sends <data> ... PCE sends 3 zero-bits.
;
; WRITE BITS
;
;  0 <addr> <size> ... MB128 then receives <data> ... PCE sends 5 zero-bits.
;
; <addr> is a 10-bit value, in 128-byte increments within the 1Mbit range.
;
;	 The code normally uses an 8-bit value of nominal 512-byte "sectors".
;
; <size> is a 20-bit value, as the # of bits to transfer.
;
;	 The code normally uses an 8-bit value of nominal 512-byte "sectors".
;
; <addr>, <size>, and <data> are all little-endian values, sent and received
; low-bit first.
;
; The final bits of each command that the PCE sends, seem designed to flush
; out the MB128's internal shift-registers, and to prepare the MB128's
; sequence-detector for the next "wakeup" sequence.
;
; ***************************************************************************
; ***************************************************************************

; Compilation options.

MB1_TRY_UNJAM	=	0			; Try to unjam a stuck MB128.
MB1_UNJAM_SIZE	=	4			; Max # of sectors to flush.

MB1_NUM_RETRIES	=	3			; Retry count for operations.

; Error Codes.

MB1_OK		=	$00
MB1_ERR_INIT	=	$A0
MB1_ERR_CSUM	=	$A1
MB1_ERR_INVALID	=	$A2
MB1_ERR_READ	=	$A3
MB1_ERR_WRITE	=	$A4

; 1st entry in MB128's 1024-byte directory.
;
; N.B. Some early MB128 games did not count the 2 directory sectors in the
;      total number of sectors used, while later MB128 games do.
;      But some later games only check the lo-byte of MB1_HEAD_USED, which
;      makes them think that $0000 is actually $0100 (i.e. full), and then
;      they think that a freshly-formatted MB128 is actually full.
;
;      These library rountines count the 2 directory sectors in the total,
;      but they do not trust MB1_HEAD_USED, and always recalculate it.

MB1_HEAD_CSUM	=	$00			; ( 2) Checksum of last 510 bytes.
MB1_HEAD_USED	=	$02			; ( 2) # of sectors used.
MB1_HEAD_IDENT	=	$04			; (12) Identification Signature.

; Other 63 entries in MB128's 1024-byte directory.
;
; N.B. The file's checksum is only of the bytes used, not of the sectors used.

MB1_FILE_ADDR	=	$00			; ( 1) Sector # (or 0 if final).
MB1_FILE_SIZE	=	$01			; ( 1) # of sectors used.
MB1_FILE_TAIL	=	$02			; ( 2) # of bytes in last sector.
MB1_FILE_CSUM	=	$04			; ( 2) Checksum of file data.
MB1_FILE_UNKN	=	$06			; ( 2) Unknown, maybe company ID.
MB1_FILE_NAME	=	$08			; ( 8) Filename.

;

		bss

mb1_detected:	ds	1			; NZ if an MB128 has been found.
mb1_retry_left:	ds	1
mb1_base_bank:	ds	1			; BRAM_BANK or SLOT_BANK.
mb1_sector_num:	ds	1
mb1_file_count	=	mb1_sector_num		; Used at different times!

mb1_directory	=	$6000			; 1024-bytes.

		code






; ****************************************************************************
; ****************************************************************************
;


mb1_read_info:
;		bra	.ready

		if	0
		lda	#BRAM_BANK		; Set up the directory bank.
		sta	mb1_base_bank		; Set the image base bank.
		jsr	mb1_load_dir		; Load the directory.
		beq	.ok0
		bne	.fail
		endif

		lda	#BRAM_BANK		; Set up the directory bank.
		jsr	mb1_load_image
		beq	.ok0
		jmp	.finished

.fail:		phx
		PUTS	.failed

		PUTS	msg_press_a_key
		jsr	wait_for_key

		plx
		rts

.ok0:		PUTS	.all_loaded

		if	0

		lda	#1
		sta	tos_m128_slot
		lda	#BRAM_BANK		; Set up the directory bank.
		jsr	tos_save_m128
		bne	.fail

.ok1:		PUTS	.write_ok

		endif

		PUTS	msg_press_a_key
		jsr	wait_for_key


		


		if	0

		; Load up the directory.

		jsr	mb1_load_dir		; Load the directory.
		beq	.ok1
		jmp	.finished

.ok1:		PUTS	mb1_msg_got_dir

		; Load up everything else.

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		stz	<__al
		lda	#$64
		sta	<__ah

		lda	#$02			; Sector address.
		ldx	#$FE			; Sector count.
		jsr	mb1_read_data
		beq	.ok2
		jmp	.finished

.fail:		phx
		PUTS	.failed
		plx
		rts

.ok2:		PUTS	.all_loaded


		; Save the directory.

		jsr	mb1_save_dir		; Save the directory.
		beq	.ok9
		jmp	.finished

.ok9:		PUTS	.dir_saved

		;
		;
		;

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		lda	#<$6400
		sta	<__al
		lda	#>$6400
		sta	<__ah

		lda	#<$0800
		ldx	#>$0800
		cly
		jsr	mb1_csum_memory

		lda	<__al
		cmp	$6014
		bne	.fail

		lda	<__ah
		cmp	$6015
		bne	.fail

		PUTS	.checksum_ok

		;
		;
		;

		lda	#<$6C00
		sta	<__al
		lda	#>$6C00
		sta	<__ah

		lda	#<$0800
		ldx	#>$0800
		cly
		jsr	mb1_csum_memory

		lda	<__al
		cmp	$6024
		bne	.fail

		lda	<__ah
		cmp	$6025
		bne	.fail

		PUTS	.checksum_ok

		;
		;
		;

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		lda	#<$6400
		sta	<__al
		lda	#>$6400
		sta	<__ah

		lda	#$02			; Sector address.
		ldx	#$04			; Sector count.
		jsr	mb1_check_data

		beq	.ok3
		jmp	.finished

.ok3:		PUTS	.chk_1st

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		lda	#<$6C00
		sta	<__al
		lda	#>$6C00
		sta	<__ah

		lda	#$06			; Sector address.
		ldx	#$04			; Sector count.
		jsr	mb1_check_data

		beq	.ok4
		jmp	.finished

.ok4:		PUTS	.chk_2nd

		;
		;
		;

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		lda	#<$6400
		sta	<__al
		lda	#>$6400
		sta	<__ah

		lda	#$0A			; Sector address.
		ldx	#$04			; Sector count.
		jsr	mb1_write_data

		beq	.ok5
		jmp	.finished

.ok5:		PUTS	.write_ok

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		lda	#<$6400
		sta	<__al
		lda	#>$6400
		sta	<__ah

		lda	#$0A			; Sector address.
		ldx	#$04			; Sector count.
		jsr	mb1_check_data

		beq	.ok6
		jmp	.finished

.ok6:		PUTS	.write_match

		endif

		;
		;
		;

.finished:
		ldx	#MB1_OK			; Return MB128_OK, i.e. found!

		cpx	#0
		rts



.dir_saved:	db	" MB128 directory saved.",$0A,0
.dir_loaded:	db	" MB128 directory loaded.",$0A,0
.all_loaded:	db	" MB128 everything loaded.",$0A,0

.checksum_ok:	db	" MB128 checksum OK.",$0A,0

.failed:	db	" MB128 checksum failed!",$0A,0

.chk_1st:	db	" 1st file matches original.",$0A,0
.chk_2nd:	db	" 2nd file matches original.",$0A,0
.write_ok:	db	" Data write OK.",$0A,0
.write_match:	db	" Data written matches original.",$0A,0

mb1_format:	dw	$0632 ; MB1_HEAD_CSUM
		dw	$0002 ; MB1_HEAD_USED
mb1_signature:	db	$D2, $D3, $D8, $CD, $DE, $B0, $BD, $31, $32, $38, 0, 0


mb1_msg_init:	db	$0C
		db	"%>%p5"
		db	" MBASE128 SD: Initializing ..."
		db	"%<%p0",$0A,$0A,$0A,0

mb1_detect_ok:	db	" MB128 detected.",$0A,0
mb1_detect_fail:db	" MB128 not found!",$0A,0


mb1_msg_rdd_now:db	" Loading MB128 directory.",0
mb1_msg_wrd_now:db	" Saving MB128 directory.",0

mb1_msg_rdd_ok:	db	$0D
		db	" MB128 directory loaded. ",$0A,0
mb1_msg_wrd_ok:	db	$0D
		db	" MB128 directory saved. ",$0A,0

mb1_msg_rdf_now:db	$0D," Loading MB128 file ",$22,0
mb1_msg_wrf_now:db	$0D," Saving MB128 file ",$22,0

mb1_msg_name:	db	"           ",0
mb1_msg_rdf_ok:	db	$0D
		db	" MB128 files loaded.            ",$0A,0
mb1_msg_wrf_ok:	db	$0D
		db	" MB128 files saved.             ",$0A,0

mb1_msg_rd_fail:db	$0A
		db	" Load failed!",$0A,0
mb1_msg_wr_fail:db	$0A
		db	" Save failed!",$0A,0



; ****************************************************************************
; ****************************************************************************
;
; mb1_new_image - Format a new MB128 image in memory.
;
; Args: A = bank # of image (uses 16 contiguous banks)
; Uses: mb1_directory address in mb1_base_bank.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_new_image:	sta	mb1_base_bank		; Set the image base bank.
		tma3

		tai	tos_zero, mb1_directory, 1024
		tii	mb1_format, mb1_directory, 16

		lda	#>(mb1_directory + 1024)
		sta	__ax + 1
		stz	__ax + 0
		lda	#$FF
		ldx	#254
		jsr	mb1_fill_memory

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		ldx	#MB1_OK
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_load_image - Load up the entire contents of the MB128 into memory.
;
; Args: A = bank # of image (uses 16 contiguous banks)
; Uses: mb1_directory address in mb1_base_bank.
; Uses: __bp = Directory ptr.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_load_image:	sta	mb1_base_bank		; Set the image base bank.

		PUTS	cls_m128_save
		PUTS	mb1_msg_rdd_now

		jsr	mb1_load_dir		; Load the directory & verify
;		beq	.got_dir		; that it makes sense.
		bne	.show_load_err

.got_dir:	PUTS	mb1_msg_rdd_ok

		lda	#2			; Starting sector for image.
		sta	mb1_sector_num

		lda	#<(mb1_directory + 16)	; Start with the 1st file.
		sta	<__bp + 0
		lda	#>(mb1_directory + 16)
		sta	<__bp + 1

.file_loop:	lda	#MB1_NUM_RETRIES	; Each file gets the full
		sta	mb1_retry_left		; number of retries.

.file_retry:	lda	mb1_base_bank		; Map in the directory bank.
		tam3

		lda	[__bp]			; Get file's MB1_FILE_ADDR.
		bne	.file_load
		jmp	.all_loaded		; Found END-OF-DIRECTORY!

.file_load:	jsr	mb1_copy_name		; Report to the user.
		PUTS	mb1_msg_rdf_now
		PUTS	mb1_msg_name

		ldy	#MB1_FILE_SIZE		; Get file's MB1_FILE_SIZE.
		lda	[__bp],y
		tax
		lda	[__bp]			; Get file's MB1_FILE_ADDR.
		jsr	mb1_image_addr		; Calculate address in image.

		jsr	mb1_read_data		; Load the file's data.
		bne	.file_load_err

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		ldy	#MB1_FILE_SIZE		; Get file's MB1_FILE_SIZE.
		lda	[__bp],y
		tax
		lda	[__bp]			; Get file's MB1_FILE_ADDR.

		jsr	mb1_image_addr		; Calculate address in image.

		jsr	mb1_check_data		; Read again to confirm data.
		beq	.file_load_ok

.file_load_err:	dec	mb1_retry_left		; Timeout?
		bne	.file_retry		; Timeout!

		ldx	#MB1_ERR_READ

.show_load_err:	phx
		PUTS	mb1_msg_rd_fail
		plx

		rts

.file_load_ok:	lda	mb1_base_bank		; Map in the directory bank.
		tam3

		if	1

		ldy	#MB1_FILE_SIZE		; Get file's MB1_FILE_SIZE.
		lda	[__bp],y
		dec	a			; Exclude last sector.
		jsr	mb1_xvert_size

		tya				; Add last sector size to csum
		sax				; byte count.
		ldy	#MB1_FILE_TAIL + 1	; Hi-Byte of # in last sector.
		clc
		adc	[__bp],y
		bcc	.skip
		inx				; bit17 of byte count.
.skip:		phx
		tax
		ldy	#MB1_FILE_TAIL + 0	; Lo-Byte of # in last sector.
		lda	[__bp],y
		ply

		jsr	mb1_image_addr		; Calculate address in image.
		jsr	mb1_csum_memory		; Calculate the file checksum.

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		ldy	#MB1_FILE_CSUM		; Compare the checksum with
		lda	[__bp],y		; the one in the directory.
		cmp	<__ax + 0
		bne	.csum_mismatch
		iny
		lda	[__bp],y
		cmp	<__ax + 1
		beq	.next_file

.csum_mismatch:	bra	.file_load_err

		endif

.next_file:	lda	mb1_sector_num		; Set file's MB1_FILE_ADDR.
		sta	[__bp]
		ldy	#MB1_FILE_SIZE		; Add file's MB1_FILE_SIZE.
		clc
		adc	[__bp],y
		sta	mb1_sector_num		; Next sector in image.
		bcs	.all_loaded		; Check for overflow.

		lda	<__bp + 0		; Goto next file entry.
		clc
		adc	#$10
		sta	<__bp + 0
		lda	<__bp + 1
		adc	#$00
		sta	<__bp + 1
		cmp	#>(mb1_directory + 1024)
		beq	.all_loaded
		jmp	.file_loop

.all_loaded:	jsr	mb1_image_addr		; Blank out the rest of the
		cla				; memory in the image.
		sec
		sbc	mb1_sector_num
		beq	.fix_csum
		tax				; # of sectors to clear.
		cla				; Value to write.
		jsr	mb1_fill_memory

.fix_csum:	jsr	mb1_check_dir		; Update directory checksum.
		bne	.show_load_err		; This should never happen!

		PUTS	mb1_msg_rdf_ok

		ldx	#MB1_OK			; Return MB128_OK.

.finished:	txa				; Set the N & Z result flags.
		rts

		;
		;
		;

mb1_copy_name:	ldy	#$08
.name_loop:	lda	[__bp],y
		beq	.name_tail
		sta	mb1_msg_name - 8,y
		iny
		cpy	#$10
		bne	.name_loop

.name_tail:	lda	#'"'
		sta	mb1_msg_name - 8,y
		iny
		lda	#'.'
		sta	mb1_msg_name - 8,y
.tail_loop:	iny
		lda	#' '
		sta	mb1_msg_name - 8,y
		cpy	#$12
		bne	.tail_loop
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_save_image - Save the entire contents of memory to the MB128.
;
; Args: mb1_base_bank = bank # of image (uses 16 contiguous banks)
; Uses: mb1_directory
; Uses: __bp = Directory ptr.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_save_image:	sta	mb1_base_bank		; Set the image base bank.

		PUTS	cls_m128_load

		jsr	mb1_check_dir		; Sanity Check the directory
		beq	.got_dir		; contents *before* saving.
		jmp	.finished

.got_dir:	lda	#0			; Write an invalid directory
		sta	mb1_sector_num		; to the MB128 to mark the
		jsr	mb1_image_addr		; file contents as trashed.

		tai	tos_zero, mb1_directory + MB1_HEAD_IDENT, 12

		cla				; Sector #.
		ldx	#2			; Sector Count.
		jsr	mb1_write_data		; Wipe out old directory data.

		tii	mb1_signature, mb1_directory + MB1_HEAD_IDENT, 12

;		bne	.finished

		lda	#<(mb1_directory + 16)	; Start with the 1st file.
		sta	<__bp + 0
		lda	#>(mb1_directory + 16)
		sta	<__bp + 1

.file_loop:	lda	#MB1_NUM_RETRIES	; Each file gets the full
		sta	mb1_retry_left		; number of retries.

.file_retry:	lda	mb1_base_bank		; Map in the directory bank.
		tam3

		lda	[__bp]			; Get file's MB1_FILE_ADDR.
		bne	.file_save
		jmp	.all_saved		; Found END-OF-DIRECTORY!

.file_save:	jsr	mb1_copy_name		; Report to the user.
		PUTS	mb1_msg_wrf_now
		PUTS	mb1_msg_name

		ldy	#MB1_FILE_SIZE		; Get file's MB1_FILE_SIZE.
		lda	[__bp],y
		tax
		lda	[__bp]			; Get file's MB1_FILE_ADDR.
		sta	mb1_sector_num		; Use sector # in directory.
		jsr	mb1_image_addr		; Calculate address in image.

		jsr	mb1_write_data		; Save the file's data.
		bne	.file_save_err

		lda	mb1_base_bank		; Map in the directory bank.
		tam3

		ldy	#MB1_FILE_SIZE		; Get file's MB1_FILE_SIZE.
		lda	[__bp],y
		tax
		lda	[__bp]			; Get file's MB1_FILE_ADDR.

		jsr	mb1_image_addr		; Calculate address in image.

		jsr	mb1_check_data		; Check to confirm the write.
		beq	.file_save_ok

.file_save_err:	dec	mb1_retry_left		; Timeout?
		bne	.file_retry		; Timeout!

		ldx	#MB1_ERR_WRITE

.show_save_err:	phx
		PUTS	mb1_msg_wr_fail
		plx

		rts

.file_save_ok:	lda	mb1_base_bank		; Map in the directory bank.
		tam3

.next_file:	lda	[__bp]			; Get file's MB1_FILE_ADDR.
		ldy	#MB1_FILE_SIZE		; Add file's MB1_FILE_SIZE.
		clc
		adc	[__bp],y
		bcs	.all_saved		; Check for overflow.

		lda	<__bp + 0		; Goto next file entry.
		clc
		adc	#$10
		sta	<__bp + 0
		lda	<__bp + 1
		adc	#$00
		sta	<__bp + 1
		cmp	#>(mb1_directory + 1024)
		beq	.all_saved
		jmp	.file_loop

.all_saved:	PUTS	mb1_msg_wrf_ok
		PUTS	mb1_msg_wrd_now

		jsr	mb1_save_dir		; Save the directory.
		bne	.show_save_err

		PUTS	mb1_msg_wrd_ok

		ldx	#MB1_OK			; Return MB128_OK.

.finished:	txa				; Set the N & Z result flags.
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_image_addr - Calculate a sector's address within the memory image.
;
; Args: mb1_sector_num = Sector #.
; Uses: __ax           = Address.
; Uses: MPR3           = Address.
;
; Preserves A, X, Y.
;

mb1_image_addr:	pha
		lda	mb1_sector_num		; Xvert sector # to address
		and	#$0F			; in the memory-image.
		asl	a
		adc	#>mb1_directory
		stz	<__ax + 0
		sta	<__ax + 1
		lda	mb1_sector_num
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		clc
		adc	mb1_base_bank
		tam3
		pla
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_load_dir - Load up the directory from the MB128.
;
; Args: mb1_base_bank = bank # of image (uses 16 contiguous banks)
; Args: __ax = ptr to memory (page-aligned in MPR3).
; Args: A    = Value.
; Args: X    = Sector count.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_load_dir:	lda	mb1_base_bank		; Map in the directory bank.
		tam3

		lda	#MB1_NUM_RETRIES	; Retry count.
.retry:		pha

		stz	<__ax + 0		; Load the 2 directory sectors.
		lda	#>mb1_directory
		sta	<__ax + 1
		cla				; Sector address.
		ldx	#$02			; Sector count.
		jsr	mb1_read_data
		bne	.failed

		stz	<__ax + 0		; Read again to confirm data.
		lda	#>mb1_directory
		sta	<__ax + 1
		cla				; Sector address.
		ldx	#$02			; Sector count.
		jsr	mb1_check_data
		bne	.failed

		ldy	#12-1			; Compare the signature.
.ident:		lda	mb1_directory + MB1_HEAD_IDENT,y
		cmp	mb1_signature,y
		bne	.failed
		dey
		bpl	.ident

		lda	#<(mb1_directory + 2)	; Generate the checksum.
		sta	<__al
		lda	#>(mb1_directory + 2)
		sta	<__ah
		lda	#<$03FE
		ldx	#>$03FE
		cly
		jsr	mb1_csum_memory

		ldx	#MB1_ERR_CSUM

		lda	<__al			; Confirm the checksum.
		cmp	mb1_directory + MB1_HEAD_CSUM + 0
		bne	.failed
		lda	<__ah
		cmp	mb1_directory + MB1_HEAD_CSUM + 1
		bne	.failed

		pla				; Throw away retry count.

		jmp	mb1_check_dir		; Validate the directory.

;		ldx	#MB1_OK

.finished:	txa				; Set the N & Z result flags.
		rts

.failed:	pla				; Restore retry count.
		dec	a			; Timeout?
		bne	.retry			; Timeout!
		bra	.finished



; ****************************************************************************
; ****************************************************************************
;
; mb1_save_dir - Save the directory to the MB128.
;
; Args: mb1_base_bank = bank # of image (uses 16 contiguous banks)
; Args: __ax = ptr to memory (page-aligned in MPR3).
; Args: A    = Value.
; Args: X    = Sector count.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_save_dir:	lda	mb1_base_bank		; Map in the directory bank.
		tam3

		jsr	mb1_check_dir		; Fix HEAD_USED & HEAD_CSUM.
		bne	.finished

		lda	#MB1_NUM_RETRIES	; Retry count.
.retry:		pha

		stz	<__ax + 0		; Save the 2 directory sectors.
		lda	#>mb1_directory
		sta	<__ax + 1
		cla				; Sector address.
		ldx	#$02			; Sector count.
		jsr	mb1_write_data
		bne	.failed

		stz	<__ax + 0		; Confirm they were written OK.
		lda	#>mb1_directory
		sta	<__ax + 1
		cla				; Sector address.
		ldx	#$02			; Sector count.
		jsr	mb1_check_data
		bne	.failed

		pla				; Throw away retry count.
		ldx	#MB1_OK

.finished:	txa				; Set the N & Z result flags.
		rts

.failed:	pla				; Restore retry count.
		dec	a			; Timeout?
		bne	.retry			; Timeout!
		bra	.finished



; ****************************************************************************
; ****************************************************************************
;
; mb1_check_dir - Test directory and update the USED and CSUM values.
;
; Args: mb1_base_bank = bank # of image (uses 16 contiguous banks)
; Args: none
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_check_dir:	lda	mb1_base_bank		; Map in the directory bank.
		tam3

		stz	mb1_file_count		; #files.

		ldy	#12-1			; Compare the signature.
.ident:		lda	mb1_directory + MB1_HEAD_IDENT,y
		cmp	mb1_signature,y
		bne	.invalid_file
		dey
		bpl	.ident


		stz	<__ax + 0		; Calculate the # of sectors
		lda	#>mb1_directory		; used, incl the directory.
		sta	<__ax + 1		; (2..256).

		lda	#2			; Current file's minimum valid
		sta	<__bl			; sector address.
		sta	mb1_directory + MB1_HEAD_USED + 0
		stz	<__bh
		stz	mb1_directory + MB1_HEAD_USED + 1

		lda	#1024 / 256		; # of pages in directory.
		sta	<__cl

		ldy	#$10			; Skip the header.

.used_loop:	lda	[__ax],y		; Check MB1_FILE_ADDR.
		beq	.used_done		; Found END-OF-DIRECTORY?
		iny

		cmp	<__bl			; Current minimum sector.
		bcc	.invalid_file
		bbs0	<__bh, .invalid_file	; Current minimum == $0100?
		clc
		adc	[__ax],y		; Add MB1_FILE_SIZE to ADDR.
		sta	<__bl			; Next file's minimum.
		bcc	.file_ok
		smb0	<__bh			; Next file's minimum = $0100.
		bne	.invalid_file		; Sector beyond end of 128KB?

.file_ok:	lda	[__ax],y		; Zero length file?
		beq	.invalid_file
		clc				; Maximum used is 254, so CC.
		adc	mb1_directory + MB1_HEAD_USED + 0
		sta	mb1_directory + MB1_HEAD_USED + 0
		bcc	.used_ok
		inc	mb1_directory + MB1_HEAD_USED + 1

.used_ok:	inc	mb1_file_count		; #files.

		tya				; Goto next file entry.
		clc
		adc	#$10-1
		tay
		bne	.used_loop
		inc	<__ax + 1
		dec	<__cl
		bne	.used_loop
		bra	.used_done		; All directory entries used!

.invalid_file:	ldx	#MB1_ERR_INVALID	; Directory invalid, don't
		rts				; trust it!

.used_done:	sec				; Clear out the rest of the
		tya				; directory.
		sta	.self_mod + 3		; Destination-lo.
		eor	#$FF
		adc	#<(mb1_directory + 1024)
		sta	.self_mod + 5		; Length-lo.
		lda	<__ax + 1
		sta	.self_mod + 4		; Destination-hi.
		eor	#$FF
		adc	#>(mb1_directory + 1024)
		sta	.self_mod + 6		; Length-hi.
		ora	.self_mod + 5
		beq	.full

.self_mod:	tai	tos_zero, mb1_directory, $0400

.full:		lda	#<(mb1_directory + 2)	; Generate the checksum.
		sta	<__ax + 0
		lda	#>(mb1_directory + 2)
		sta	<__ax + 1
		lda	#<$03FE
		ldx	#>$03FE
		cly
		jsr	mb1_csum_memory

		lda	<__al			; Store the checksum.
		sta	mb1_directory + MB1_HEAD_CSUM + 0
		lda	<__ah
		sta	mb1_directory + MB1_HEAD_CSUM + 1

.self_cnt:	ldy	#0			; #files.
		ldx	#MB1_OK
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_fill_memory - Fill memory with a value (for testing).
;
; Args: __ax = ptr to memory (page-aligned in MPR3).
; Args: A    = Fill value to write.
; Args: X    = Sector count.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_fill_memory:pha				; Preserve fill value.
		txa
		jsr	mb1_xvert_size		; Xvert sectors to bytes.
		pla				; Restore fill value.

		cpx	#$00
		beq	.next_64kb

.64kb_loop:	phy
		cly
.byte_loop:	sta	[__ax],y
		iny
		bne	.byte_loop
		inc	<__ax + 1
		bpl	.next_page
		pha
		tma3
		inc	a
		tam3
		lda	#$60
		sta	<__ax + 1
		pla
.next_page:	dex
		bne	.byte_loop
		ply
.next_64kb:	dey
		bpl	.64kb_loop

		ldx	#MB1_OK
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_csum_memory - Calculate the checksum of a range of memory.
;
; Args: __ax = ptr to memory (below MPR4 or just page-aligned in MPR3).
; Args: A    = lo-byte of # of bytes to checksum.
; Args: X    = hi-byte of # of bytes to checksum.
; Args: Y    = bit-17  of # of bytes to checksum.
; Uses: __bx = Counter.
;
; Returns: __ax = Checksum value.
;

		if	0

mb1_csum_memory:sta	.self_mod + 1		; Save # of bytes.
		stx	<__bl			; Save # of pages.
		sty	<__bh			; Save # of 64KB.
		cly

		txa				; Test # of pages.
		cla				; Set lo-byte of checksum.
		clx				; Set hi-byte of checksum.
		beq	.next_64kb

.page_loop:	clc				; Update checksum.
		adc	[__ax],y
		bcc	.next_byte
		inx

.next_byte:	iny				; Update offset.
		bne	.page_loop
		inc	<__ax + 1
		bpl	.next_page

		pha				; Checksums that cross this
		tma3                            ; boundary are page-aligned
		inc	a                       ; or else we'd need to map
		tam3                            ; in two new banks.
		lda	#$60
		sta	<__ax + 1
		pla

.next_page:	dec	<__bl
		bne	.page_loop
.next_64kb:	dec	<__bh			; Is there a full 64KB block
		bpl	.page_loop		; still left to checksum?
		bra	.self_mod		; Any remaining data to do?

.byte_loop:	clc				; Checksum the final 1..255
		adc	[__ax],y		; bytes.
		bcc	.skip2
		inx
.skip2:		iny
.self_mod:	cpy	#00
		bne	.byte_loop

.finished:	sta	<__ax + 0		; Save the checksum.
		stx	<__ax + 1
		rts


		else

mb1_csum_memory:sty	<__bh			; Save # of 64KB.
		inx				; +1 for bytes in last page.
		stx	<__bl			; Save # of pages.
		clx				; Set hi-byte of checksum.

		tay				; Save # of bytes.
		beq	.next_page
		cla				; Set lo-byte of checksum.

.byte_loop:	clc				; Update checksum.
		adc	[__ax]
		bcc	.next_addr
		inx

.next_addr:	inc	<__ax + 0		; Update offset.
		beq	.page_wrap

.next_byte:	dey
		bne	.byte_loop
.next_page:	dec	<__bl			; Is there a full page still
		bne	.byte_loop              ; left to checksum?
.next_64kb:	dec	<__bh			; Is there a full 64KB block
		bpl	.byte_loop		; still left to checksum?

.finished:	sta	<__ax + 0		; Save the checksum.
		stx	<__ax + 1
		rts

.page_wrap:	inc	<__ax + 1
		bpl	.next_byte

		pha				; Checksums that cross this
		tma3                            ; boundary are page-aligned
		inc	a                       ; or else we'd need to map
		tam3                            ; in two new banks.
		lda	#$60
		sta	<__ax + 1
		pla

		bra	.next_byte
		endif



; ****************************************************************************
; ****************************************************************************
;
; mb1_write_data - Write sectors of data from memory to the MB128.
;
; Args: __ax = ptr to memory (page-aligned in MPR3).
; Args: A    = Sector address.
; Args: X    = Sector count.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_write_data:	php				; Disable interrupts during
		sei				; this function.

		phx				; Preserve sector count.
		pha				; Preserve sector address.

		jsr	mb1_wakeup		; Wakeup the MB128 interface.
		beq	.ready

		pla				; Throw away address/count.
		pla
		bra	.finished		; Return error code.

.ready:		jsr	mb1_send_wr_cmd
		pla				; Restore sector address.
		jsr	mb1_send_addr
		pla				; Restore sector count.
		pha
		jsr	mb1_send_size
		pla				; Restore sector count.
		jsr	mb1_xvert_size		; Xvert sectors to bytes.

		cpx	#$00
		beq	.next_64kb
.64kb_loop:	phy
		cly
.page_loop:	phx
		clx
		sec
.byte_loop:	lda	[__ax],y
;		stx	IO_PORT			; RWCLK lo for 30 cycles > 4us.
		ror	a			; Put next bit in C.

.bit_loop:	pha
		cla				; Move C flag into bit 0.
		rol	a
		pha
		sta	IO_PORT			; CLR lo, SEL is bit.
		pla				; RWCLK hi for 14 cycles = 2us.
		pha
		ora	#2
		sta	IO_PORT			; CLR hi, SEL is bit (reset).
		plx				; RWCLK lo for 29 cycles = 4us.
		pla
		lsr	a
		bne	.bit_loop

.next_byte:	iny
		bne	.byte_loop
		inc	<__ax + 1
		nop
		stx	IO_PORT			; RWCLK lo for 29 cycles = 4us.
		bpl	.next_page
		tma3
		inc	a
		tam3
		lda	#$60
		sta	<__ax + 1
.next_page:	plx
		dex
		bne	.page_loop
		ply
.next_64kb:	dey
		bpl	.64kb_loop

		jsr	mb1_flush_data		; Prime wakeup buffer.

		ldx	#MB1_OK

.finished:	lda	VDC_SR			; Skip any pending VDC irq.
		nop
		plp				; Restore interrupts.
		txa				; Set the N & Z result flags.
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_read_data - Read sectors of data into memory from the MB128.
;
; Args: __ax = ptr to memory (page-aligned in MPR3).
; Args: A    = Sector address.
; Args: X    = Sector count.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_read_data:	php				; Disable interrupts during
		sei				; this function.

		phx				; Preserve sector count.
		pha				; Preserve sector address.

		jsr	mb1_wakeup		; Wakeup the MB128 interface.
		beq	.ready

		pla				; Throw away address/count.
		pla
		bra	.finished		; Return error code.

.ready:		jsr	mb1_send_rd_cmd
		pla				; Restore sector address.
		jsr	mb1_send_addr
		pla				; Restore sector count.
		pha
		jsr	mb1_send_size
		pla				; Restore sector count.
		jsr	mb1_xvert_size		; Xvert sectors to bytes.

		stz	IO_PORT			; CLR lo, SEL lo (buttons).
		cpx	#$00
		beq	.next_64kb
.64kb_loop:	phy
		cly
.page_loop:	phx
.byte_loop:	lda	#$80

.bit_loop:	pha
		lda	#2
		sta	IO_PORT			; CLR hi, SEL lo (reset).
		pha				; RWCLK lo for 29 cycles = 4us.
		pla
		nop
		nop
		lda	IO_PORT			; Read while in reset state.
		lsr	a
		pla
		ror	a
		stz	IO_PORT			; CLR lo, SEL lo (buttons).
		bcc	.bit_loop		; RWCLK hi for 14 cycles = 2us.

		sta	[__ax],y		; Save the byte in memory.

.next_byte:	iny
		bne	.byte_loop
		inc	<__ax + 1
		bpl	.next_page
		tma3
		inc	a
		tam3
		lda	#$60
		sta	<__ax + 1
.next_page:	plx
		dex
		bne	.page_loop
		ply
.next_64kb:	dey
		bpl	.64kb_loop

		jsr	mb1_flush_data		; Prime wakeup buffer.

		ldx	#MB1_OK

.finished:	lda	VDC_SR			; Skip any pending VDC irq.
		nop
		plp				; Restore interrupts.
		txa				; Set the N & Z result flags.
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_check_data - Check that sectors of data from the MB128 match memory.
;
; Args: __ax = ptr to memory (page-aligned in MPR3).
; Args: A    = Sector address.
; Args: X    = Sector count.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_check_data:	php				; Disable interrupts during
		sei				; this function.

		phx				; Preserve sector count.
		pha				; Preserve sector address.

		jsr	mb1_wakeup		; Wakeup the MB128 interface.
		beq	.ready

		pla				; Throw away address/count.
		pla
		bra	.finished		; Return error code.

.ready:		jsr	mb1_send_rd_cmd
		pla				; Restore sector address.
		jsr	mb1_send_addr
		pla				; Restore sector count.
		pha
		jsr	mb1_send_size
		pla				; Restore sector count.
		jsr	mb1_xvert_size		; Xvert sectors to bytes.

		cld				; D flag indicates failure.
		stz	IO_PORT			; CLR lo, SEL lo (buttons).
		cpx	#$00
		beq	.next_64kb
.64kb_loop:	phy
		cly
.page_loop:	phx
.byte_loop:	lda	#$80

.bit_loop:	pha
		lda	#2
		sta	IO_PORT			; CLR hi, SEL lo (reset).
		pha				; RWCLK lo for 29 cycles = 4us.
		pla
		nop
		nop
		lda	IO_PORT			; Read while in reset state.
		lsr	a
		pla
		ror	a
		stz	IO_PORT			; CLR lo, SEL lo (buttons).
		bcc	.bit_loop		; RWCLK hi for 14 cycles = 2us.

		cmp	[__ax],y		; Compare the byte in memory.
		beq	.next_byte
		sed				; Signal that the test failed!

.next_byte:	iny
		bne	.byte_loop
		inc	<__ax + 1
		bpl	.next_page
		tma3
		inc	a
		tam3
		lda	#$60
		sta	<__ax + 1
.next_page:	plx
		dex
		bne	.page_loop
		ply
.next_64kb:	dey
		bpl	.64kb_loop

		php				; Preserve the D flag.
		cld				; Reset D flag.

		jsr	mb1_flush_data		; Prime wakeup buffer.

		pla				; Get the D flag value.
		and	#$08			; Test the D flag.
		tax

.finished:	lda	VDC_SR			; Skip any pending VDC irq.
		nop
		plp				; Restore interrupts.
		txa				; Set the N & Z result flags.
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_detect - Detect whether an MB128 is present.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;

mb1_detect:	php				; Disable interrupts during
		sei				; this function.

		jsr	mb1_wakeup		; Wakeup the MB128 interface.
		bne	.finished

.ready:		lda	#%00000001		; Send READ 1 BIT FROM
		jsr	mb1_send_byte		; ADDR 0 command.
		lda	#%00001000
		jsr	mb1_send_byte
;		lda	#%00000000
		jsr	mb1_send_byte
;		lda	#%00000000		; Send 7 bits, Recv 1 bit,
		jsr	mb1_send_byte		; but we ignore the value.

		ldx	#MB1_OK			; Return MB128_OK, i.e. found!

.finished:	jsr	mb1_flush_data		; Prime wakeup buffer.

		lda	VDC_SR			; Skip any pending VDC irq.
		nop
		plp				; Restore interrupts.
		txa				; Set the N & Z result flags.
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_wakeup - Wake up the MB128 so that it is ready for a command.
;
; Returns: X = MB1_OK (and Z flag) or an error code.
;
; N.B. For INTERNAL use, not for APPLICATION use!
;

mb1_wakeup:	ldy	#<(MB1_UNJAM_SIZE + 1)	; Max # of sectors to "unjam".

		ldx	#3			; Retry 3 times.

		ldx	#1			; Retry 1 time.

.retry:		lda	#$A8			; Send "$A8" to MB128.
		jsr	mb1_send_byte		; Send a byte to MB128.

		lda	#%10			; Send '0' bit to MB128.
		jsr	mb1_send_bits		; Selects buttons.

		lda	IO_PORT			; Read buttons.
		and	#$0F
		sta	.self_mod + 1

		lda	#%11			; Send '1' bit to MB128.
		jsr	mb1_send_bits		; Selects direction-pad.

		lda	IO_PORT			; Read direction-pad.
		asl	a
		asl	a
		asl	a
		asl	a

.self_mod:	ora	#$00			; Composite the buttons.
		cmp	#$40			; Magic value for detection.
		bne	.not_detected		; L, R, U, RUN, SEL, I & II.

		sta	mb1_detected		; Remember this for the future.

		ldx	#MB1_OK			; MB128_OK, found and ready!
		rts				;

.not_detected:	dex				; Timeout?
		bne	.retry			; Timeout!

		if	MB1_TRY_UNJAM

		lda	mb1_detected		; Did we ever detect an MB128?
		beq	.fail			; Only try unjamming if "yes".

		dey				; Are there any more sectors
		beq	.fail			; to flush?

		cla				; Send/Recv a sector's worth
.unjam:		bsr	mb1_send_byte		; of data with the MB128 to
		bsr	mb1_send_byte		; unjam the interface.
		dex
		bne	.unjam
;		bsr	mb1_send_byte		; Prime wakeup buffer.

		ldx	#1			; Try again (just once).
		bra	.retry

		endif

.fail:		ldx	#MB1_ERR_INIT		; MB1_ERR_INIT, i.e. not found.
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_send_byte - Send a single byte to the MB128.
; mb1_send_bits - Send a # of bits to the MB128.
;
; Args: A = data to send.
;
; Returns: A = 0 and CS.
;
; N.B. X,Y are preserved.
;
; N.B. For INTERNAL use, not for APPLICATION use!
;

mb1_send_bits:	clc
		db	$B0			; BCS instruction to skip SEC.
mb1_send_byte:	sec
		ror	a
		phx
.bit_loop:	pha
		cla				; Move C flag into bit 0.
		rol	a
		pha
		sta	IO_PORT			; CLR lo, SEL is bit.
		pla				; RWCLK hi for 14 cycles = 2us.
		pha
		ora	#2
		sta	IO_PORT			; CLR hi, SEL is bit (reset).
		plx				; RWCLK lo for 29 cycles = 4us.
		pla
		lsr	a
		bne	.bit_loop
		sax
		sax
		sax
		sax
		stx	IO_PORT			; Keep SEL as last bit sent so
		plx				; mb1_wakeup can read the port.
		rts				; Return with A = 0.

		;

mb1_send_rd_cmd:lda	#%00001001		; Send '1' followed by
		bra	mb1_send_bits		; low 2 address bits.

		;

mb1_send_wr_cmd:lda	#%00001000		; Send '0' followed by
		bra	mb1_send_bits		; low 2 address bits.

		;

mb1_flush_data:	lda	#%00100000		; Send five '0' bits
		bra	mb1_send_bits		; to flush MB128 buffer.

		;

mb1_send_addr:	bsr	mb1_send_byte		; Send top 8 bits of
		lda	#%00001000		; address followed by
		bra	mb1_send_bits		; #-of-bits count.

		;

mb1_send_size:	jsr	mb1_xvert_size		; Xvert sectors to bytes.
		bsr	mb1_send_byte		; Send lo-byte.
		txa
		bsr	mb1_send_byte		; Send hi-byte.
		tya
		and	#%00000001
		ora	#%00000010
		bra	mb1_send_bits		; Send bit-17.

		;

mb1_xvert_size:	asl	a			; Xvert #-of-sectors
		tax				; to #-of-bytes.
		cla
		rol	a
		tay
		cla
		rts



; ****************************************************************************
; ****************************************************************************
;
; mb1_recv_bit - Read one bit from MB128.
;
; Returns: CF = bit value.
;
; N.B. X,Y are preserved.
;
; N.B. For INTERNAL use, not for APPLICATION use!
;

		if	0

mb1_recv_bit:	stz	IO_PORT			; CLR lo, SEL lo (buttons).
		pha				; RWCLK hi for 14 cycles = 2us.
		pla
		lda	#2
		sta	IO_PORT			; CLR hi, SEL lo (reset).
		pha				; RWCLK lo for 29 cycles = 4us.
		pla
		nop
		nop
		lda	IO_PORT			; Read while in reset state.
		lsr	a
		sxy
		sxy
		stz	IO_PORT			; CLR lo, SEL lo (buttons).
		rts

		endif
