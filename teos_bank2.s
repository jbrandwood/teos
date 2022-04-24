; ***************************************************************************
; ***************************************************************************
;
; BANK $02 - MENU SCREENS
;
; Copyright John Brandwood 2019.
;
; Distributed under the Boost Software License, Version 1.0.
; (See accompanying file LICENSE_1_0.txt or copy at
;  http://www.boost.org/LICENSE_1_0.txt)
;
; ***************************************************************************
; ***************************************************************************



		bank	2
		org	$C000

		include	"menu_init.s"
		include	"menu_hucard.s"
		include	"menu_bram.s"
		include	"menu_mb128.s"
		include	"menu_info.s"
