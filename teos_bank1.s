; ***************************************************************************
; ***************************************************************************
;
; BANK $01 - LIBRARY CODE
;
; Copyright John Brandwood 2019.
;
; Distributed under the Boost Software License, Version 1.0.
; (See accompanying file LICENSE_1_0.txt or copy at
;  http://www.boost.org/LICENSE_1_0.txt)
;
; ***************************************************************************
; ***************************************************************************



		.bank	1
		.org	$A000

		include	"mb128.s"
		include	"huc6280.s"
		include	"osfunc.s"
		include	"filefuncs.s"
