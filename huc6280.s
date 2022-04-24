; ***************************************************************************
; ***************************************************************************
;
; huc6280.s
;
; Disassembler Functions for the HuC6280 processor.
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
; Here are the HuC6280 opcodes in numeric order ...
;
; ----------------------------------------------------------------------------------------------
; |    | x0       | x1       | x2       | x3       | x4       | x5       | x6       | x7       |
; ----------------------------------------------------------------------------------------------
; | 0x | BRK  nul | ORA  izx |:SXY  nul |:ST0  val | TSB  zp  | ORA  zp  | ASL  zp  | RMB0  zp |
; | 1x | BPL  rel | ORA  izy | ORA  izp |:ST1  val | TRB  zp  | ORA  zpx | ASL  zpx | RMB1  zp |
; | 2x | JSR  abs | AND  izx |:SAX  nul |:ST2  val | BIT  zp  | AND  zp  | ROL  zp  | RMB2  zp |
; | 3x | BMI  rel | AND  izy | AND  izp | ???  bad | BIT  zpx | AND  zpx | ROL  zpx | RMB3  zp |
; | 4x | RTI  nul | EOR  izx |:SAY  nul |:TMA  val |:BSR  rel | EOR  zp  | LSR  zp  | RMB4  zp |
; | 5x | BVC  rel | EOR  izy | EOR  izp |:TAM  val |:CSL  nul | EOR  zpx | LSR  zpx | RMB5  zp |
; | 6x | RTS  nul | ADC  izx |:CLA  nul | ???  bad | STZ  zp  | ADC  zp  | ROR  zp  | RMB6  zp |
; | 7x | BVS  rel | ADC  izy | ADC  izp |:TII  blk | STZ  zpx | ADC  zpx | ROR  zpx | RMB7  zp |
; | 8x | BRA  rel | STA  izx |:CLX  nul |:TST  vzp | STY  zp  | STA  zp  | STX  zp  | SMB0  zp |
; | 9x | BCC  rel | STA  izy | STA  izp |:TST  vab | STY  zpx | STA  zpx | STX  zpy | SMB1  zp |
; | Ax | LDY  val | LDA  izx | LDX  val |:TST  vzx | LDY  zp  | LDA  zp  | LDX  zp  | SMB2  zp |
; | Bx | BCS  rel | LDA  izy | LDA  izp |:TST  vax | LDY  zpx | LDA  zpx | LDX  zpy | SMB3  zp |
; | Cx | CPY  val | CMP  izx |:CLY  nul |:TDD  blk | CPY  zp  | CMP  zp  | DEC  zp  | SMB4  zp |
; | Dx | BNE  rel | CMP  izy | CMP  izp |:TIN  blk |:CSH  nul | CMP  zpx | DEC  zpx | SMB5  zp |
; | Ex | CPX  val | SBC  izx | ???  bad |:TIA  blk | CPX  zp  | SBC  zp  | INC  zp  | SMB6  zp |
; | Fx | BEQ  rel | SBC  izy | SBC  izp |:TAI  blk |:SET  nul | SBC  zpx | INC  zpx | SMB7  zp |
; ----------------------------------------------------------------------------------------------
; |    | OPC addr | OPC addr | OPC addr | OPC addr | OPC addr | OPC addr | OPC addr | OPC addr |
; ----------------------------------------------------------------------------------------------
;
; ----------------------------------------------------------------------------------------------
; |    | x8       | x9       | xA       | xB       | xC       | xD       | xE       | xF       |
; ----------------------------------------------------------------------------------------------
; | 0x | PHP  nul | ORA  val | ASL  acc | ???  bad | TSB  abs | ORA  abs | ASL  abs | BBR0 zpr |
; | 1x | CLC  nul | ORA  aby | INC  acc | ???  bad | TRB  abs | ORA  abx | ASL  abx | BBR1 zpr |
; | 2x | PLP  nul | AND  val | ROL  acc | ???  bad | BIT  abs | AND  abs | ROL  abs | BBR2 zpr |
; | 3x | SEC  nul | AND  aby | DEC  acc | ???  bad | BIT  abx | AND  abx | ROL  abx | BBR3 zpr |
; | 4x | PHA  nul | EOR  val | LSR  acc | ???  bad | JMP  abs | EOR  abs | LSR  abs | BBR4 zpr |
; | 5x | CLI  nul | EOR  aby | PHY  nul | ???  bad | ???  bad | EOR  abx | LSR  abx | BBR5 zpr |
; | 6x | PLA  nul | ADC  val | ROR  acc | ???  bad | JMP  iab | ADC  abs | ROR  abs | BBR6 zpr |
; | 7x | SEI  nul | ADC  aby | PLY  nul | ???  bad | JMP  iax | ADC  abx | ROR  abx | BBR7 zpr |
; | 8x | DEY  nul | BIT  val | TXA  nul | ???  bad | STY  abs | STA  abs | STX  abs | BBS0 zpr |
; | 9x | TYA  nul | STA  aby | TXS  nul | ???  bad | STZ  abs | STA  abx | STZ  abx | BBS1 zpr |
; | Ax | TAY  nul | LDA  val | TAX  nul | ???  bad | LDY  abs | LDA  abs | LDX  abs | BBS2 zpr |
; | Bx | CLV  nul | LDA  aby | TSX  nul | ???  bad | LDY  abx | LDA  abx | LDX  aby | BBS3 zpr |
; | Cx | INY  nul | CMP  val | DEX  nul | ???  bad | CPY  abs | CMP  abs | DEC  abs | BBS4 zpr |
; | Dx | CLD  nul | CMP  aby | PHX  nul | ???  bad | ???  bad | CMP  abx | DEC  abx | BBS5 zpr |
; | Ex | INX  nul | SBC  val | NOP  nul | ???  bad | CPX  abs | SBC  abs | INC  abs | BBS6 zpr |
; | Fx | SED  nul | SBC  aby | PLX  nul | ???  bad | ???  bad | SBC  abx | INC  abx | BBS7 zpr |
; ----------------------------------------------------------------------------------------------
; |    | OPC addr | OPC addr | OPC addr | OPC addr | OPC addr | OPC addr | OPC addr | OPC addr |
; ----------------------------------------------------------------------------------------------

		;
		; HuC6280 Cycle Counts (in 3-bits)
		;

CYCL_n 		= 	0x00
CYCL_2 		= 	0x20
CYCL_3 		= 	0x40
CYCL_4 		= 	0x60
CYCL_5 		= 	0x80
CYCL_6 		= 	0xA0
CYCL_7 		= 	0xC0
CYCL_8 		= 	0xE0

		;
		; HuC6280 Addressing Modes (in 5-bits)
		;

		rsset	0
MODE_xxx	rs	1			; illegal
MODE_NUL	rs	1			; 
MODE_ACC	rs	1			; A
MODE_MPR	rs	1			; #nn
MODE_VAL	rs	1			; #nn
MODE_REL	rs	1			; label
MODE_ZPG	rs	1			; <$nn
MODE_ZPX	rs	1			; <$nn,X
MODE_ZPY	rs	1			; <$nn,Y
MODE_ZPR	rs	1			; <$nn,label
MODE_IZP	rs	1			; [$nn]
MODE_IZX	rs	1			; [$nn,X]
MODE_IZY	rs	1			; [$nn],Y
MODE_ABS	rs	1			; $nnnn
MODE_ABX	rs	1			; $nnnn,X
MODE_ABY	rs	1			; $nnnn,Y
MODE_IAB	rs	1			; [$nnnn]
MODE_IAX	rs	1			; [$nnnn,X]
MODE_VZP	rs	1			; #nn,<$nn
MODE_VZX	rs	1			; #nn,<$nn,X
MODE_VAB	rs	1			; #nn,$nnnn
MODE_VAX	rs	1			; #nn,$nnnn,X
MODE_BLK	rs	1			; $nnnn,$nnnn,$nnnn

		;
		; HuC6280 Instruction Names (88 total)
		;

		rsset	0
INST_xxx	rs	1
INST_ADC	rs	1
INST_AND	rs	1
INST_ASL	rs	1
INST_BBR	rs	1
INST_BBS	rs	1
INST_BCC	rs	1
INST_BCS	rs	1

INST_BEQ	rs	1
INST_BIT	rs	1
INST_BMI	rs	1
INST_BNE	rs	1
INST_BPL	rs	1
INST_BRA	rs	1
INST_BRK	rs	1
INST_BSR	rs	1

INST_BVC	rs	1
INST_BVS	rs	1
INST_CLA	rs	1
INST_CLC	rs	1
INST_CLD	rs	1
INST_CLI	rs	1
INST_CLV	rs	1
INST_CLX	rs	1

INST_CLY	rs	1
INST_CMP	rs	1
INST_CPX	rs	1
INST_CPY	rs	1
INST_CSH	rs	1
INST_CSL	rs	1
INST_DEC	rs	1
INST_DEX	rs	1

INST_DEY	rs	1
INST_EOR	rs	1
INST_INC	rs	1
INST_INX	rs	1
INST_INY	rs	1
INST_JMP	rs	1
INST_JSR	rs	1
INST_LDA	rs	1

INST_LDX	rs	1
INST_LDY	rs	1
INST_LSR	rs	1
INST_NOP	rs	1
INST_ORA	rs	1
INST_PHA	rs	1
INST_PHP	rs	1
INST_PHX	rs	1

INST_PHY	rs	1
INST_PLA	rs	1
INST_PLP	rs	1
INST_PLX	rs	1
INST_PLY	rs	1
INST_RMB	rs	1
INST_ROL	rs	1
INST_ROR	rs	1

INST_RTI	rs	1
INST_RTS	rs	1
INST_SAX	rs	1
INST_SAY	rs	1
INST_SBC	rs	1
INST_SEC	rs	1
INST_SED	rs	1
INST_SEI	rs	1

INST_SET	rs	1
INST_SMB	rs	1
INST_STV	rs	1
INST_STA	rs	1
INST_STX	rs	1
INST_STY	rs	1
INST_STZ	rs	1
INST_SXY	rs	1

INST_TAI	rs	1
INST_TAM	rs	1
INST_TAX	rs	1
INST_TAY	rs	1
INST_TDD	rs	1
INST_TIA	rs	1
INST_TII	rs	1
INST_TIN	rs	1

INST_TMA	rs	1
INST_TRB	rs	1
INST_TSB	rs	1
INST_TST	rs	1
INST_TSX	rs	1
INST_TXA	rs	1
INST_TXS	rs	1
INST_TYA	rs	1

		;
		; Pack 3 * 5-bit instruction names and 1-bit "reads memory"
		; flag into 2 bytes.
		;

PACK_INSTR	func	(\4 << 15) | ((\3 & $1F) << 10) | ((\2 & $1F) << 5) | (\1 & $1F)

dbg_inst_names:	dw	PACK_INSTR('D', 'B', '@', 0)	; INST_xxx
		dw	PACK_INSTR('A', 'D', 'C', 1)	; INST_ADC
		dw	PACK_INSTR('A', 'N', 'D', 1)	; INST_AND
		dw	PACK_INSTR('A', 'S', 'L', 1)	; INST_ASL
		dw	PACK_INSTR('B', 'B', 'R', 0)	; INST_BBR
		dw	PACK_INSTR('B', 'B', 'S', 0)	; INST_BBS
		dw	PACK_INSTR('B', 'C', 'C', 0)	; INST_BCC
		dw	PACK_INSTR('B', 'C', 'S', 0)	; INST_BCS

		dw	PACK_INSTR('B', 'E', 'Q', 0)	; INST_BEQ
		dw	PACK_INSTR('B', 'I', 'T', 1)	; INST_BIT
		dw	PACK_INSTR('B', 'M', 'I', 0)	; INST_BMI
		dw	PACK_INSTR('B', 'N', 'E', 0)	; INST_BNE
		dw	PACK_INSTR('B', 'P', 'L', 0)	; INST_BPL
		dw	PACK_INSTR('B', 'R', 'A', 0)	; INST_BRA
		dw	PACK_INSTR('B', 'R', 'K', 0)	; INST_BRK
		dw	PACK_INSTR('B', 'S', 'R', 0)	; INST_BSR

		dw	PACK_INSTR('B', 'V', 'C', 0)	; INST_BVC
		dw	PACK_INSTR('B', 'V', 'S', 0)	; INST_BVS
		dw	PACK_INSTR('C', 'L', 'A', 0)	; INST_CLA
		dw	PACK_INSTR('C', 'L', 'C', 0)	; INST_CLC
		dw	PACK_INSTR('C', 'L', 'D', 0)	; INST_CLD
		dw	PACK_INSTR('C', 'L', 'I', 0)	; INST_CLI
		dw	PACK_INSTR('C', 'L', 'V', 0)	; INST_CLV
		dw	PACK_INSTR('C', 'L', 'X', 0)	; INST_CLX

		dw	PACK_INSTR('C', 'L', 'Y', 0)	; INST_CLY
		dw	PACK_INSTR('C', 'M', 'P', 1)	; INST_CMP
		dw	PACK_INSTR('C', 'P', 'X', 1)	; INST_CPX
		dw	PACK_INSTR('C', 'P', 'Y', 1)	; INST_CPY
		dw	PACK_INSTR('C', 'S', 'H', 0)	; INST_CSH
		dw	PACK_INSTR('C', 'S', 'L', 0)	; INST_CSL
		dw	PACK_INSTR('D', 'E', 'C', 1)	; INST_DEC
		dw	PACK_INSTR('D', 'E', 'X', 0)	; INST_DEX

		dw	PACK_INSTR('D', 'E', 'Y', 0)	; INST_DEY
		dw	PACK_INSTR('E', 'O', 'R', 1)	; INST_EOR
		dw	PACK_INSTR('I', 'N', 'C', 1)	; INST_INC
		dw	PACK_INSTR('I', 'N', 'X', 0)	; INST_INX
		dw	PACK_INSTR('I', 'N', 'Y', 0)	; INST_INY
		dw	PACK_INSTR('J', 'M', 'P', 0)	; INST_JMP
		dw	PACK_INSTR('J', 'S', 'R', 0)	; INST_JSR
		dw	PACK_INSTR('L', 'D', 'A', 1)	; INST_LDA

		dw	PACK_INSTR('L', 'D', 'X', 1)	; INST_LDX
		dw	PACK_INSTR('L', 'D', 'Y', 1)	; INST_LDY
		dw	PACK_INSTR('L', 'S', 'R', 1)	; INST_LSR
		dw	PACK_INSTR('N', 'O', 'P', 0)	; INST_NOP
		dw	PACK_INSTR('O', 'R', 'A', 1)	; INST_ORA
		dw	PACK_INSTR('P', 'H', 'A', 0)	; INST_PHA
		dw	PACK_INSTR('P', 'H', 'P', 0)	; INST_PHP
		dw	PACK_INSTR('P', 'H', 'X', 0)	; INST_PHX

		dw	PACK_INSTR('P', 'H', 'Y', 0)	; INST_PHY
		dw	PACK_INSTR('P', 'L', 'A', 0)	; INST_PLA
		dw	PACK_INSTR('P', 'L', 'P', 0)	; INST_PLP
		dw	PACK_INSTR('P', 'L', 'X', 0)	; INST_PLX
		dw	PACK_INSTR('P', 'L', 'Y', 0)	; INST_PLY
		dw	PACK_INSTR('R', 'M', 'B', 0)	; INST_RMB
		dw	PACK_INSTR('R', 'O', 'L', 1)	; INST_ROL
		dw	PACK_INSTR('R', 'O', 'R', 1)	; INST_ROR

		dw	PACK_INSTR('R', 'T', 'I', 0)	; INST_RTI
		dw	PACK_INSTR('R', 'T', 'S', 0)	; INST_RTS
		dw	PACK_INSTR('S', 'A', 'X', 0)	; INST_SAX
		dw	PACK_INSTR('S', 'A', 'Y', 0)	; INST_SAY
		dw	PACK_INSTR('S', 'B', 'C', 1)	; INST_SBC
		dw	PACK_INSTR('S', 'E', 'C', 0)	; INST_SEC
		dw	PACK_INSTR('S', 'E', 'D', 0)	; INST_SED
		dw	PACK_INSTR('S', 'E', 'I', 0)	; INST_SEI

		dw	PACK_INSTR('S', 'E', 'T', 0)	; INST_SET
		dw	PACK_INSTR('S', 'M', 'B', 1)	; INST_SMB
		dw	PACK_INSTR('S', 'T', '@', 1)	; INST_STV
		dw	PACK_INSTR('S', 'T', 'A', 0)	; INST_STA
		dw	PACK_INSTR('S', 'T', 'X', 0)	; INST_STX
		dw	PACK_INSTR('S', 'T', 'Y', 0)	; INST_STY
		dw	PACK_INSTR('S', 'T', 'Z', 0)	; INST_STZ
		dw	PACK_INSTR('S', 'X', 'Y', 0)	; INST_SXY

		dw	PACK_INSTR('T', 'A', 'I', 0)	; INST_TAI
		dw	PACK_INSTR('T', 'A', 'M', 0)	; INST_TAM
		dw	PACK_INSTR('T', 'A', 'X', 0)	; INST_TAX
		dw	PACK_INSTR('T', 'A', 'Y', 0)	; INST_TAY
		dw	PACK_INSTR('T', 'D', 'D', 0)	; INST_TDD
		dw	PACK_INSTR('T', 'I', 'A', 0)	; INST_TIA
		dw	PACK_INSTR('T', 'I', 'I', 0)	; INST_TII
		dw	PACK_INSTR('T', 'I', 'N', 0)	; INST_TIN

		dw	PACK_INSTR('T', 'M', 'A', 0)	; INST_TMA
		dw	PACK_INSTR('T', 'R', 'B', 0)	; INST_TRB
		dw	PACK_INSTR('T', 'S', 'B', 1)	; INST_TSB
		dw	PACK_INSTR('T', 'S', 'T', 1)	; INST_TST
		dw	PACK_INSTR('T', 'S', 'X', 0)	; INST_TSX
		dw	PACK_INSTR('T', 'X', 'A', 0)	; INST_TXA
		dw	PACK_INSTR('T', 'X', 'S', 0)	; INST_TXS
		dw	PACK_INSTR('T', 'Y', 'A', 0)	; INST_TYA

		;
		; Pack 7-bit instruction index and 1-bit "includes bit number"
		; flag into 1 byte.
		;

		; 0x
dbg_opcode2inst:db	INST_BRK, INST_ORA, INST_SXY, INST_STV + $80
		db	INST_TSB, INST_ORA, INST_ASL, INST_RMB + $80
		db	INST_PHP, INST_ORA, INST_ASL, INST_xxx
		db	INST_TSB, INST_ORA, INST_ASL, INST_BBR + $80
		; 1x
		db	INST_BPL, INST_ORA, INST_ORA, INST_STV + $80
		db	INST_TRB, INST_ORA, INST_ASL, INST_RMB + $80
		db	INST_CLC, INST_ORA, INST_INC, INST_xxx
		db	INST_TRB, INST_ORA, INST_ASL, INST_BBR + $80
		; 2x
		db	INST_JSR, INST_AND, INST_SAX, INST_STV + $80
		db	INST_BIT, INST_AND, INST_ROL, INST_RMB + $80
		db	INST_PLP, INST_AND, INST_ROL, INST_xxx
		db	INST_BIT, INST_AND, INST_ROL, INST_BBR + $80
		; 3x
		db	INST_BMI, INST_AND, INST_AND, INST_xxx
		db	INST_BIT, INST_AND, INST_ROL, INST_RMB + $80
		db	INST_SEC, INST_AND, INST_DEC, INST_xxx
		db	INST_BIT, INST_AND, INST_ROL, INST_BBR + $80
		; 4x
		db	INST_RTI, INST_EOR, INST_SAY, INST_TMA + $80
		db	INST_BSR, INST_EOR, INST_LSR, INST_RMB + $80
		db	INST_PHA, INST_EOR, INST_LSR, INST_xxx
		db	INST_JMP, INST_EOR, INST_LSR, INST_BBR + $80
		; 5x
		db	INST_BVC, INST_EOR, INST_EOR, INST_TAM + $80
		db	INST_CSL, INST_EOR, INST_LSR, INST_RMB + $80
		db	INST_CLI, INST_EOR, INST_PHY, INST_xxx
		db	INST_xxx, INST_EOR, INST_LSR, INST_BBR + $80
		; 6x
		db	INST_RTS, INST_ADC, INST_CLA, INST_xxx
		db	INST_STZ, INST_ADC, INST_ROR, INST_RMB + $80
		db	INST_PLA, INST_ADC, INST_ROR, INST_xxx
		db	INST_JMP, INST_ADC, INST_ROR, INST_BBR + $80
		; 7x
		db	INST_BVS, INST_ADC, INST_ADC, INST_TII
		db	INST_STZ, INST_ADC, INST_ROR, INST_RMB + $80
		db	INST_SEI, INST_ADC, INST_PLY, INST_xxx
		db	INST_JMP, INST_ADC, INST_ROR, INST_BBR + $80
		; 8x
		db	INST_BRA, INST_STA, INST_CLX, INST_TST
		db	INST_STY, INST_STA, INST_STX, INST_SMB + $80
		db	INST_DEY, INST_BIT, INST_TXA, INST_xxx
		db	INST_STY, INST_STA, INST_STX, INST_BBS + $80
		; 9x
		db	INST_BCC, INST_STA, INST_STA, INST_TST
		db	INST_STY, INST_STA, INST_STX, INST_SMB + $80
		db	INST_TYA, INST_STA, INST_TXS, INST_xxx
		db	INST_STZ, INST_STA, INST_STZ, INST_BBS + $80
		; Ax
		db	INST_LDY, INST_LDA, INST_LDX, INST_TST
		db	INST_LDY, INST_LDA, INST_LDX, INST_SMB + $80
		db	INST_TAY, INST_LDA, INST_TAX, INST_xxx
		db	INST_LDY, INST_LDA, INST_LDX, INST_BBS + $80
		; Bx
		db	INST_BCS, INST_LDA, INST_LDA, INST_TST
		db	INST_LDY, INST_LDA, INST_LDX, INST_SMB + $80
		db	INST_CLV, INST_LDA, INST_TSX, INST_xxx
		db	INST_LDY, INST_LDA, INST_LDX, INST_BBS + $80
		; Cx
		db	INST_CPY, INST_CMP, INST_CLY, INST_TDD
		db	INST_CPY, INST_CMP, INST_DEC, INST_SMB + $80
		db	INST_INY, INST_CMP, INST_DEX, INST_xxx
		db	INST_CPY, INST_CMP, INST_DEC, INST_BBS + $80
		; Dx
		db	INST_BNE, INST_CMP, INST_CMP, INST_TIN
		db	INST_CSH, INST_CMP, INST_DEC, INST_SMB + $80
		db	INST_CLD, INST_CMP, INST_PHX, INST_xxx
		db	INST_xxx, INST_CMP, INST_DEC, INST_BBS + $80
		; Ex
		db	INST_CPX, INST_SBC, INST_xxx, INST_TIA
		db	INST_CPX, INST_SBC, INST_INC, INST_SMB + $80
		db	INST_INX, INST_SBC, INST_NOP, INST_xxx
		db	INST_CPX, INST_SBC, INST_INC, INST_BBS + $80
		; Fx
		db	INST_BEQ, INST_SBC, INST_SBC, INST_TAI
		db	INST_SET, INST_SBC, INST_INC, INST_SMB + $80
		db	INST_SED, INST_SBC, INST_PLX, INST_xxx
		db	INST_xxx, INST_SBC, INST_INC, INST_BBS + $80

		;
		; Pack 3-bit cycle count and 5-bit addressing mode into 1 byte.
		;

		; 0x
dbg_opcode2mode:db	MODE_NUL + CYCL_8, MODE_IZX + CYCL_7, MODE_NUL + CYCL_3, MODE_VAL + CYCL_4
		db	MODE_ZPG + CYCL_6, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_3, MODE_VAL + CYCL_2, MODE_ACC + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABS + CYCL_7, MODE_ABS + CYCL_5, MODE_ABS + CYCL_7, MODE_ZPR + CYCL_6
		; 1x
		db	MODE_REL + CYCL_4, MODE_IZY + CYCL_7, MODE_IZP + CYCL_7, MODE_VAL + CYCL_4
		db	MODE_ZPG + CYCL_6, MODE_ZPX + CYCL_4, MODE_ZPX + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_ABY + CYCL_5, MODE_ACC + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABS + CYCL_7, MODE_ABX + CYCL_5, MODE_ABX + CYCL_7, MODE_ZPR + CYCL_6
		; 2x
		db	MODE_ABS + CYCL_7, MODE_IZX + CYCL_7, MODE_NUL + CYCL_3, MODE_VAL + CYCL_4
		db	MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_4, MODE_VAL + CYCL_2, MODE_ACC + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABS + CYCL_5, MODE_ABS + CYCL_5, MODE_ABS + CYCL_7, MODE_ZPR + CYCL_6
		; 3x
		db	MODE_REL + CYCL_4, MODE_IZY + CYCL_7, MODE_IZP + CYCL_7, MODE_xxx + CYCL_2
		db	MODE_ZPX + CYCL_4, MODE_ZPX + CYCL_7, MODE_ZPX + CYCL_7, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_ABY + CYCL_5, MODE_ACC + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABX + CYCL_5, MODE_ABX + CYCL_5, MODE_ABX + CYCL_7, MODE_ZPR + CYCL_6
		; 4x
		db	MODE_NUL + CYCL_7, MODE_IZX + CYCL_7, MODE_NUL + CYCL_3, MODE_MPR + CYCL_4
		db	MODE_REL + CYCL_8, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_3, MODE_VAL + CYCL_2, MODE_ACC + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABS + CYCL_4, MODE_ABS + CYCL_5, MODE_ABS + CYCL_7, MODE_ZPR + CYCL_6
		; 5x
		db	MODE_REL + CYCL_4, MODE_IZY + CYCL_7, MODE_IZP + CYCL_7, MODE_MPR + CYCL_5
		db	MODE_NUL + CYCL_3, MODE_ZPX + CYCL_4, MODE_ZPX + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_ABY + CYCL_5, MODE_NUL + CYCL_3, MODE_xxx + CYCL_2
		db	MODE_xxx + CYCL_2, MODE_ABX + CYCL_5, MODE_ABX + CYCL_7, MODE_ZPR + CYCL_6
		; 6x
		db	MODE_NUL + CYCL_7, MODE_IZX + CYCL_7, MODE_NUL + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_4, MODE_VAL + CYCL_2, MODE_ACC + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_IAB + CYCL_7, MODE_ABS + CYCL_5, MODE_ABS + CYCL_7, MODE_ZPR + CYCL_6
		; 7x
		db	MODE_REL + CYCL_4, MODE_IZY + CYCL_7, MODE_IZP + CYCL_7, MODE_BLK + CYCL_n
		db	MODE_ZPX + CYCL_4, MODE_ZPX + CYCL_4, MODE_ZPX + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_ABY + CYCL_5, MODE_NUL + CYCL_4, MODE_xxx + CYCL_2
		db	MODE_IAX + CYCL_7, MODE_ABX + CYCL_5, MODE_ABX + CYCL_7, MODE_ZPR + CYCL_6
		; 8x
		db	MODE_REL + CYCL_4, MODE_IZX + CYCL_7, MODE_NUL + CYCL_2, MODE_VZP + CYCL_7
		db	MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_VAL + CYCL_2, MODE_NUL + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABS + CYCL_5, MODE_ABS + CYCL_5, MODE_ABS + CYCL_5, MODE_ZPR + CYCL_6
		; 9x
		db	MODE_REL + CYCL_4, MODE_IZY + CYCL_7, MODE_IZP + CYCL_7, MODE_VAB + CYCL_8
		db	MODE_ZPX + CYCL_4, MODE_ZPX + CYCL_4, MODE_ZPY + CYCL_4, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_ABY + CYCL_5, MODE_NUL + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABS + CYCL_5, MODE_ABX + CYCL_5, MODE_ABX + CYCL_5, MODE_ZPR + CYCL_6
		; Ax
		db	MODE_VAL + CYCL_2, MODE_IZX + CYCL_7, MODE_VAL + CYCL_2, MODE_VZX + CYCL_7
		db	MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_VAL + CYCL_2, MODE_NUL + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABS + CYCL_5, MODE_ABS + CYCL_5, MODE_ABS + CYCL_5, MODE_ZPR + CYCL_6
		; Bx
		db	MODE_REL + CYCL_4, MODE_IZY + CYCL_7, MODE_IZP + CYCL_7, MODE_VAX + CYCL_8
		db	MODE_ZPX + CYCL_4, MODE_ZPX + CYCL_7, MODE_ZPY + CYCL_4, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_ABY + CYCL_5, MODE_NUL + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABX + CYCL_5, MODE_ABX + CYCL_5, MODE_ABY + CYCL_5, MODE_ZPR + CYCL_6
		; Cx
		db	MODE_VAL + CYCL_2, MODE_IZX + CYCL_7, MODE_NUL + CYCL_2, MODE_BLK + CYCL_n
		db	MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_VAL + CYCL_2, MODE_NUL + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABS + CYCL_5, MODE_ABS + CYCL_5, MODE_ABS + CYCL_7, MODE_ZPR + CYCL_6
		; Dx
		db	MODE_REL + CYCL_4, MODE_IZY + CYCL_7, MODE_IZP + CYCL_7, MODE_BLK + CYCL_n
		db	MODE_NUL + CYCL_3, MODE_ZPX + CYCL_4, MODE_ZPX + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_ABY + CYCL_5, MODE_NUL + CYCL_3, MODE_xxx + CYCL_2
		db	MODE_xxx + CYCL_2, MODE_ABX + CYCL_5, MODE_ABX + CYCL_7, MODE_ZPR + CYCL_6
		; Ex
		db	MODE_VAL + CYCL_2, MODE_IZX + CYCL_7, MODE_xxx + CYCL_2, MODE_BLK + CYCL_n
		db	MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_4, MODE_ZPG + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_VAL + CYCL_2, MODE_NUL + CYCL_2, MODE_xxx + CYCL_2
		db	MODE_ABS + CYCL_5, MODE_ABS + CYCL_5, MODE_ABS + CYCL_7, MODE_ZPR + CYCL_6
		; Fx
		db	MODE_REL + CYCL_4, MODE_IZY + CYCL_7, MODE_IZP + CYCL_7, MODE_BLK + CYCL_n
		db	MODE_NUL + CYCL_2, MODE_ZPX + CYCL_4, MODE_ZPX + CYCL_6, MODE_ZPG + CYCL_7
		db	MODE_NUL + CYCL_2, MODE_ABY + CYCL_5, MODE_NUL + CYCL_4, MODE_xxx + CYCL_2
		db	MODE_xxx + CYCL_2, MODE_ABX + CYCL_5, MODE_ABX + CYCL_7, MODE_ZPR + CYCL_6
