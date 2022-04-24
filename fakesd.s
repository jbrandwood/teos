; ***************************************************************************
; ***************************************************************************
;
; fakesd.s
;
; Various sectors from an existing SDcard that can be used to simulate a
; real FAT32 volume when debugging TEOS in mednafen.
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
; $0a3a fat1_begins sector
; 
; $27f0 data_begins sector
; 
; 8 sectors per cluster
; 
; 0c 00 00 00 "SuperGrafx" cluster        -> sector  $2850 -> offset  $50a000   2 sectors
; c2 15 00 00 "USA" directory cluster     -> sector  $d600 -> offset $1ac0000   2 sectors
; 7b 3c 00 00 "JAPAN" directory cluster   -> sector $20bc8 -> offset $4179000   2 sectors
; 05 3e 00 00 "A-C" directory cluster     -> sector $21818 -> offset $4303000   1 cluster
; 
; byte offset $014
; sect offset $07c
; 
; -> FAT sector address $ab6
; 
; -> next cluster $4a86 -> sector $27c20 -> offset $4f84000
;

		if	REALHW
		else

		.bank	7
		.org	$4000

fake_bram1:	incbin	"fake/save0.bin"
fake_bram2:	incbin	"fake/save2.bin"

		;
		;
		;

fake_sector_l:	db	$00
		db	$00
		db	$B6
		db	$B7
		db	$00
		db	$00

		db	$50
		db	$51

		db	$C8
		db	$C9

		db	$18
		db	$19
		db	$1A
		db	$1B
		db	$1C
		db	$1D
		db	$1E
		db	$1F

		db	$20
		db	$21
		db	$22
		db	$FF

fake_sector_m:	db	$00
		db	$08
		db	$0A
		db	$0A
		db	$20
		db	$28

		db	$28
		db	$28

		db	$0B
		db	$0B

		db	$18
		db	$18
		db	$18
		db	$18
		db	$18
		db	$18
		db	$18
		db	$18

		db	$7C
		db	$7C
		db	$7C
		db	$FF

fake_sector_h:	db	$00
		db	$00
		db	$00
		db	$00
		db	$00
		db	$00

		db	$00
		db	$00

		db	$02
		db	$02

		db	$02
		db	$02
		db	$02
		db	$02
		db	$02
		db	$02
		db	$02
		db	$02

		db	$02
		db	$02
		db	$02
		db	$FF

fake_sector_tii:dw	fake_000000
		dw      fake_000800
		dw      fake_000AB6
		dw      fake_000AB7
		dw      fake_002000
		dw      fake_002800

		dw      fake_002850
		dw      fake_002851

		dw      fake_020BC8
		dw      fake_020BC9

		dw      fake_021818
		dw      fake_021819
		dw      fake_02181A
		dw      fake_02181B
		dw      fake_02181C
		dw      fake_02181D
		dw      fake_02181E
		dw      fake_02181F

		dw      fake_027C20
		dw      fake_027C21
		dw      fake_027C22

fake_000000:	tii	sector000000 + $0000,f32_cache_buf,512
		rts
fake_000800:	tii	sector000800 + $0000,f32_cache_buf,512
		rts
fake_000AB6:	tii	sector000AB6 + $0000,f32_cache_buf,512
		rts
fake_000AB7:	tii	sector000AB6 + $0200,f32_cache_buf,512
		rts
fake_002000:	tii	sector002000 + $0000,f32_cache_buf,512
		rts
fake_002800:	tii	sector002800 + $0000,f32_cache_buf,512
		rts

fake_002850:	tii	sector002850 + $0000,f32_cache_buf,512
		rts
fake_002851:	tii	sector002850 + $0200,f32_cache_buf,512
		rts

fake_020BC8:	tii	sector020BC8 + $0000,f32_cache_buf,512
		rts
fake_020BC9:	tii	sector020BC8 + $0200,f32_cache_buf,512
		rts

fake_021818:	tii	sector021818 + $0000,f32_cache_buf,512
		rts
fake_021819:	tii	sector021818 + $0200,f32_cache_buf,512
		rts
fake_02181A:	tii	sector021818 + $0400,f32_cache_buf,512
		rts
fake_02181B:	tii	sector021818 + $0600,f32_cache_buf,512
		rts
fake_02181C:	tii	sector021818 + $0800,f32_cache_buf,512
		rts
fake_02181D:	tii	sector021818 + $0A00,f32_cache_buf,512
		rts
fake_02181E:	tii	sector021818 + $0C00,f32_cache_buf,512
		rts
fake_02181F:	tii	sector021818 + $0E00,f32_cache_buf,512
		rts

fake_027C20:	tii	sector027C20 + $0000,f32_cache_buf,512
		rts
fake_027C21:	tii	sector027C20 + $0200,f32_cache_buf,512
		rts
fake_027C22:	tii	sector027C20 + $0400,f32_cache_buf,512
		rts

		;
		;
		;

sector000000:	incbin	"fake/sector00000000.bin" ; (1) MBR
sector000800:	incbin	"fake/sector00000800.bin" ; (1) FAT32 BPB
sector000AB6:	incbin	"fake/sector00000AB6.bin" ; (2) FAT for  A-C directory

;sector000000:	incbin	"fake/t32-s000000.bin" ; (1) MBR
sector002000:	incbin	"fake/t32-s002000.bin" ; (1) MBR


		.bank	8
		.org	$6000

sector002800:	incbin	"fake/sector00002800.bin" ; (1) Root directory
sector002850:	incbin	"fake/sector00002850.bin" ; (2) SuperGrafx directory
sector020BC8:	incbin	"fake/sector00020BC8.bin" ; (2) Japan directory
sector021818:	incbin	"fake/sector00021818.bin" ; (8) A-C directory 1st
sector027C20:	incbin	"fake/sector00027C20.bin" ; (3) A-C directory 2nd

		endif
