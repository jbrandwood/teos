; ***************************************************************************
; ***************************************************************************
;
; menu_info.s
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
; tos_info_menu - System Information Menu.
;


tos_info_menu:	; 16-bit Date : 7-bits Year, 4-bits Month, 5-bits Day.

		lda	ted_assem_date + 0	; Day.
		tax
		and	#$1F
		sta	tos_info_temp + 3

		lda	ted_assem_date + 1	; Month
		lsr	a
		sax
		ror	a
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		sta	tos_info_temp + 2

		txa				; Year starts from 1980.
		clc
		adc	#<1980
		sta	tos_info_temp + 0
		cla
		adc	#>1980
		sta	tos_info_temp + 1

		; 16-bit time : 2-second increments since midnight.

		tii	ted_assem_time, __ax, 2
		lda	#30
		sta	<__cl
		jsr	tos_div16_7u
		lda	<__dl
		asl	a
		sta	tos_info_temp + 6	; Seconds.
		lda	#60
		sta	<__cl
		jsr	tos_div16_7u
		lda	<__dl
		sta	tos_info_temp + 5	; Minutes.
		lda	<__al
		sta	tos_info_temp + 4	; Hours.

		;

		PUTS	msg_system_info

.wait_input:	stz	joytrg
.wait_loop:	jsr	wait_vsync
		lda	joytrg
		beq	.wait_loop

		bit	#JOY_SEL
		beq	.same_menu
		jmp	tos_hucard_menu

.same_menu:	bra	.wait_input

		bss

tos_info_temp:	ds	7

		code

		;
		;
		;

msg_system_info:db	"%>%p5"
		db	"%xl",0
		db	$0C
		db	" SYSTEM INFORMATION%p0"
		db	$0A,$0A
		db	"%<"

		db	"%xl",2
		db	"%y",2
		db	$0A

		db	"%p2"
		db	"Turbo EverDrive V2",$0A
		db	"Developed by I. Golubovskiy",$0A,$0A
		db	"%p0"
		db	"%r"
		dw	TED_BASE_ADDR + TED_REG_VERSION
		db	"FPGA Core Ver   : %hX",$0A
		db	"%r"
		dw	$5000
		db	"BootLoader1 Ver : %X",$0A
		db	"%r"
		dw	ted_boot2_ver
		db	"BootLoader2 Ver : %X",$0A
		db	"%r"
		dw	tos_info_temp
		db	"Assembly Date   : %u-%hu-%hu",$0A
		db	"Assembly Time   : %hu:%hu:%hu",$0A
		db	"%r"
		dw	ted_serial_num
		db	"Serial Number   : %X",$0A
		db	"Support Email   : biokrik@gmail.com",$0A
		db	"Support Forum   : http://krikzz.com",$0A,$0A,$0A

		db	"%p2"
		db	"TEOS (Turbo EverDrive OS)",$0A
		db	"Developed by J. Brandwood",$0A,$0A
		db	"%p0"
		db	"%r"
		dw	$FFF0
		db	"OS Version      : %X beta 6",$0A
		db	"Support Forum   :",$0A
		db	"      http://pcengine.proboards.com",$0A

		db	"%xl",0,$0A
		db	"%p5%y",24
;		db	"----------------------------------------"
		db	"              SEL%p6:Chg Menu%p5",$0A
		db	"%p0"
		db	0
