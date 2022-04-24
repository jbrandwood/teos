; ***************************************************************************
; ***************************************************************************
;
; filefuncs.s
;
; File loading and saving.
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
; tos_chdir_tbed - Change to the "/TBED/" directory.
;
; Uses: __bp = Pointer to directory entry in cache.
;
; Returns: X = F32_OK (and Z flag) or an error code.
;

tos_chdir_tbed:	jsr	f32_select_root		; Start back at the root.
		bne	.exit

		lda	#<.filename		; Locate the 'TBED' folder.
		ldy	#>.filename
		sta	<__ax + 0
		sty	<__ax + 1
		jsr	f32_find_name
		bne	.exit

		jmp	f32_change_dir

.exit:		rts

.filename:	db	"TBED",0



; ***************************************************************************
; ***************************************************************************
;
; tos_slot_file -
;
; Args: tos_bram_slot = Slot #.
; Uses: __bp          = Pointer to directory entry in cache.
;
; Returns: X = F32_OK (and Z flag) or an error code.
;

tos_slot_file:	jsr	tos_chdir_tbed		; Select "/TBED/" directory.
		bne	.exit

		lda	tos_bram_slot		; Divide slot # by 10.
		inc	a
		sta	<__ax + 0
		stz	<__ax + 1
		lda	#10
		sta	<__cl
		jsr	tos_div16_7u

		lda	<__al			; Dividend.
		clc
		adc	#'0'
		sta	.filename + 5
		lda	<__dl			; Remainder.
		clc
		adc	#'0'
		sta	.filename + 6

		lda	#<.filename
		ldy	#>.filename
		sta	<__ax + 0
		sty	<__ax + 1
		jsr	f32_find_name		; Locate the named file in
		bne	.exit			; the current directory.

		jsr	f32_open_file		; Open the file.
		bne	.exit

		ldx	#TOS_WRONG_SIZE

		lda	f32_file_length + 1	; Check that the file is 2KB.
		cmp	#$08
		bne	.close
		lda	f32_file_length + 3
		ora	f32_file_length + 2
		ora	f32_file_length + 0
		bne	.close

		lda	#2048 / 512
		sta	<__ax + 0		; Lo-byte of # blocks in file.
		stz	<__ax + 1		; Hi-byte of # blocks in file.

		stz	<sdc_data_addr + 0	; Xfer into $6000-$67FF.
		lda	#$60
		sta	<sdc_data_addr + 1

		lda	#SLOT_BANK
		tam3

		ldx	#TOS_OK

.exit:		rts

.close:		jmp	f32_close_file		; Close the file & set N & Z.

.filename:	db	"BRAM-01.BIN",0



; ***************************************************************************
; ***************************************************************************
;
; tos_load_slot - Load a BRAM slot file into memory.
;

tos_load_slot:	jsr	tos_slot_file		; Select and open the
		bne	.exit			; slot file.

		jsr	f32_file_read		; Read the file into memory.
		jsr	f32_close_file		; Close the file & set N & Z.

.exit:		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_save_slot - Load a BRAM slot file into memory.
;

tos_save_slot:	jsr	tos_slot_file		; Select and open the
		bne	.exit			; slot file.

		jsr	f32_file_write		; Write the file from memory.
		jsr	f32_close_file		; Close the file & set N & Z.

.exit:		rts




; ***************************************************************************
; ***************************************************************************
;
; tos_m128_file -
;
; Args: A = bank # of image (uses 16 contiguous banks)
; Args: tos_m128_slot = Slot #.
; Uses: __bp          = Pointer to directory entry in cache.
;
; Returns: X = F32_OK (and Z flag) or an error code.
;

tos_m128_file:	sta	mb1_base_bank		; Set the image base bank.

		jsr	tos_chdir_tbed		; Select "/TBED/" directory.
		bne	.exit

		lda	tos_m128_slot		; Divide slot # by 10.
		inc	a
		sta	<__ax + 0
		stz	<__ax + 1
		lda	#10
		sta	<__cl
		jsr	tos_div16_7u

		lda	<__al			; Dividend.
		clc
		adc	#'0'
		sta	.filename + 6
		lda	<__dl			; Remainder.
		clc
		adc	#'0'
		sta	.filename + 7

		lda	#<.filename
		ldy	#>.filename
		sta	<__ax + 0
		sty	<__ax + 1
		jsr	f32_find_name		; Locate the named file in
		bne	.exit			; the current directory.

		jsr	f32_open_file		; Open the file.
		bne	.exit

		ldx	#TOS_WRONG_SIZE

		lda	f32_file_length + 2	; Check that the file is 128KB.
		cmp	#$02
		bne	.close
		lda	f32_file_length + 3
		ora	f32_file_length + 1
		ora	f32_file_length + 0
		bne	.close

		stz	<__ax + 0		; Lo-byte of # blocks in file.
		lda	#>(131072 / 512)
		sta	<__ax + 1		; Hi-byte of # blocks in file.

		lda	#$4F			; Map in TED2 512KB bank 4.
		sta	sdc_data_bank

		stz	<sdc_data_addr + 0	; Xfer into $6000-$67FF.
		lda	#$60
		sta	<sdc_data_addr + 1

		lda	mb1_base_bank		; Select the starting bank.
		tam3

		ldx	#TOS_OK

.exit:		rts

.close:		jmp	f32_close_file		; Close the file & set N & Z.

.filename:	db	"MB128-01.BIN",0



; ***************************************************************************
; ***************************************************************************
;
; tos_load_m128 - Load an MB128 slot file into memory.
;
; Args: A = bank # of image (uses 16 contiguous banks)
; Args: tos_m128_slot = Slot #.
; Uses: __bp          = Pointer to directory entry in cache.
;
; Returns: X = F32_OK (and Z flag) or an error code.
;

tos_load_m128:	jsr	tos_m128_file		; Select the and open the
		bne	.exit			; slot file.

		jsr	f32_file_read		; Read the file into memory.
		jsr	f32_close_file		; Close the file & set N & Z.

.exit:		rts



; ***************************************************************************
; ***************************************************************************
;
; tos_save_m128 - Load an MB128 slot file into memory.
;
; Args: A = bank # of image (uses 16 contiguous banks)
; Args: tos_m128_slot = Slot #.
; Uses: __bp          = Pointer to directory entry in cache.
;
; Returns: X = F32_OK (and Z flag) or an error code.
;

tos_save_m128:	jsr	tos_m128_file		; Select the and open the
		bne	.exit			; slot file.

		jsr	f32_file_write		; Write the file from memory.
		jsr	f32_close_file		; Close the file & set N & Z.

.exit:		rts
