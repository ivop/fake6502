; Altirra Acid800 test suite
; Copyright (C) 2010 Avery Lee, All Rights Reserved.
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE. 

.ifndef ATARI
    opt h-
    opt f+
    org 0
    dta 0
.endif

    org $c0
a0      dta a(0)
a1      dta a(0)
a2      dta a(0)
a3      dta a(0)
d0      dta 0
d1      dta 0
d2      dta 0
d3      dta 0
d4      dta 0
d5      dta 0
d6      dta 0
d7      dta 0

        org     $2000
main:
        mwa     #test_start a0

loop_start:
        ;load instruction
        ldy     #0
        mva     (a0),y insn
        iny
        mva     (a0),y insn+1
        iny
        mva     (a0),y insn+2
        iny
        
        ;setup temp registers
        mwa     #d5 a1
        mwa     #d4 a2
        mwa     #d5-$ff a3
        
        ;TEMP -- display opcode
.if 0
        lda     insn
        lsr
        lsr
        lsr
        lsr
        tax
        lda     hexdig2,x
        ldy     #2
        sta     ($58),y
        iny
        lda     insn
        and     #$0f
        tax
        lda     hexdig2,x
        sta     ($58),y
        
        ldy     #3
.endif
        
        ;stash A
        mva     (a0),y d0
        iny
        
        ;load X
        lda     (a0),y
        tax
        iny
        
        ;stash Y
        mva     (a0),y d1
        iny
        
        ;stash P
        lda     (a0),y
        iny
        pha
        
        ;load d5
        mva     (a0),y d5
        iny
        ;load Y
        ldy     d1
        
        ;load A
        lda     d0
        
        ;load P
        plp
        
        ;execute insn
        jsr     insn

        ;store registers
        sta     d1
        stx     d2
        sty     d3
        php
        pla
        ora     #$30
        sta     d4
        
        ;reset flags
        cli
        cld
        
        ;compare registers
        ldy     #8
        lda     (a0),y
        cmp     d1
        bne     fail
        iny
        lda     (a0),y
        cmp     d2
        bne     fail
        iny
        lda     (a0),y
        cmp     d3
        bne     fail
        iny
        lda     (a0),y
        iny
        cmp     d4
        bne     fail
        lda     (a0),y
        cmp     d5
        bne     fail
        
        ;go another round
        lda     a0
        clc
        adc     #13
        sta     a0
        scc:inc a0+1
        
        cmp     #<test_end
        bne     loop_end
        lda     a0+1
        cmp     #>test_end
        beq     loop_exit
loop_end:
        jmp     loop_start
loop_exit:
success
        jmp *

fail:
        cld
        cli

        ;save off A0 (it'll be stomped by imprintf)
        lda     a0+1
        pha
        lda     a0
        pha

        ;jsr        _imprintf
        ;dta     c"FAIL.",$9b,c"Test failed: A=%x X=%x Y=%x P=%x M=%x",$9b,0

        pla
        sta     a0
        pla
        sta     a0+1

        ldy     #7
        ldx     #4
        mva:rpl (a0),y- d1,x-

        ;jsr        _imprintf
        ;dta     c"  Inputs: A=%x X=%x Y=%x P=%x M=%x",$9b,0

        mva     insn d1
        mva     insn+1 d2
        mva     insn+2 d3
        
        ;ldy     #>failmsg
        ;lda     #<failmsg
        ;jsr        _testFailed2
        lda insn
        ldx insn+1
        ldy insn+2
FAILED:
        jmp *
failmsg:
        dta     c"  Insn: %x %x %x",0

hexdig2:
        dta     d"0123456789ABCDEF"

;============================================================================
test_start:
        ;       insn          A   X   Y   P   M    A   X   Y   P   M
        ;SLO (zp,X) (ASL + ORA)
        dta     $03,<a0,$60, $00,$02,$00,$30,$81, $02,$02,$00,$31,$02
        dta     $03,<a0,$60, $f0,$02,$00,$30,$81, $f2,$02,$00,$b1,$02
        
        ;NOP zp
        dta     $04,<d5,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $04,<d5,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SLO zp (ASL + ORA)
        dta     $07,<d5,$60, $00,$00,$00,$30,$81, $02,$00,$00,$31,$02
        dta     $07,<d5,$60, $f0,$00,$00,$30,$81, $f2,$00,$00,$b1,$02
        
        ;AAC #imm (modified AND)
        dta     $0b,$00,$60, $00,$00,$00,$30,$00, $00,$00,$00,$32,$00
        dta     $0b,$55,$60, $ff,$00,$00,$30,$00, $55,$00,$00,$30,$00
        dta     $0b,$a0,$60, $f0,$00,$00,$30,$00, $a0,$00,$00,$b1,$00
        dta     $0b,$55,$60, $aa,$00,$00,$30,$00, $00,$00,$00,$32,$00

        ;NOP abs
        dta     $0c,<d5,>d5, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $0c,<d5,>d5, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SLO abs (ASL + ORA)
        dta     $0f,<d5,>d5, $00,$00,$00,$30,$81, $02,$00,$00,$31,$02
        dta     $0f,<d5,>d5, $f0,$00,$00,$30,$81, $f2,$00,$00,$b1,$02

        ;SLO (zp),Y (ASL + ORA)
        dta     $13,<a2,$60, $00,$00,$01,$30,$81, $02,$00,$01,$31,$02
        dta     $13,<a2,$60, $f0,$00,$01,$30,$81, $f2,$00,$01,$b1,$02
        
        ;NOP zp,X
        dta     $14,<d5,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $14,<d5,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SLO zp,X (ASL + ORA)
        dta     $17,<d4,$60, $00,$01,$00,$30,$81, $02,$01,$00,$31,$02
        dta     $17,<d4,$60, $f0,$01,$00,$30,$81, $f2,$01,$00,$b1,$02
        
        ;NOP
        dta     $1A,$60,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $1A,$60,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SLO abs,Y (ASL + ORA)
        dta     $1B,<d2,>d2, $00,$00,$03,$30,$81, $02,$00,$03,$31,$02
        dta     $1B,<d2,>d2, $f0,$00,$03,$30,$81, $f2,$00,$03,$b1,$02
        
        ;NOP abs,X
        dta     $1C,<d5,>d5, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $1C,<d5,>d5, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SLO abs,X (ASL + ORA)
        dta     $1F,<d3,>d3, $00,$02,$03,$30,$81, $02,$02,$03,$31,$02
        dta     $1F,<d3,>d3, $f0,$02,$03,$30,$81, $f2,$02,$03,$b1,$02
        
        ;RLA (zp,X) (ROL + AND)
        dta     $23,<a0,$60, $ff,$02,$00,$31,$81, $03,$02,$00,$31,$03
        
        ;RLA zp (ROL + AND)
        dta     $27,<d5,$60, $ff,$00,$00,$31,$81, $03,$00,$00,$31,$03

        ;RLA abs (ROL + AND)
        dta     $2F,<d5,>d5, $ff,$00,$00,$31,$81, $03,$00,$00,$31,$03
        
        ;RLA (zp),Y (ROL + AND)
        dta     $33,<a2,$60, $ff,$00,$01,$31,$81, $03,$00,$01,$31,$03
        
        ;NOP zp,X
        dta     $34,<d5,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $34,<d5,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;RLA zp,X (ROL + AND)
        dta     $37,<d4,$60, $ff,$01,$00,$31,$81, $03,$01,$00,$31,$03
        
        ;NOP
        dta     $3A,$60,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $3A,$60,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;RLA abs,Y (ROL + AND)
        dta     $3B,<d4,>d4, $ff,$00,$01,$31,$81, $03,$00,$01,$31,$03
        
        ;NOP abs,X
        dta     $3C,<d5,>d5, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $3C,<d5,>d5, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;RLA abs,X (ROL + AND)
        dta     $3F,<d4,>d4, $ff,$01,$00,$31,$81, $03,$01,$00,$31,$03
        
        ;SRE (zp,X) (LSR + EOR)
        dta     $43,<a0,$60, $0f,$02,$00,$30,$55, $25,$02,$00,$31,$2a
        
        ;NOP zp
        dta     $44,<d5,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $44,<d5,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SRE zp (LSR + EOR)
        dta     $47,<d5,$60, $0f,$00,$00,$30,$55, $25,$00,$00,$31,$2a
        
        ;ASR #imm (AND + LSR)
        dta     $4B,$55,$60, $ff,$00,$00,$30,$55, $2a,$00,$00,$31,$55
        
        ;SRE abs (LSR + EOR)
        dta     $4F,<d5,>d5, $0f,$00,$00,$30,$55, $25,$00,$00,$31,$2a
        
        ;SRE (zp),Y (LSR + EOR)
        dta     $53,<a2,$60, $0f,$00,$01,$30,$55, $25,$00,$01,$31,$2a
        
        ;NOP zp,X
        dta     $54,<d5,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $54,<d5,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SRE zp,X (LSR + EOR)
        dta     $57,<d2,$60, $0f,$03,$00,$30,$55, $25,$03,$00,$31,$2a
        
        ;NOP
        dta     $5A,$60,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $5A,$60,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SRE abs,Y (LSR + EOR)
        dta     $5B,<d1,>d1, $0f,$00,$04,$30,$55, $25,$00,$04,$31,$2a
        
        ;NOP abs,X
        dta     $5C,<d5,>d5, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $5C,<d5,>d5, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SRE abs,X (LSR + EOR)
        dta     $5F,<d1,>d1, $0f,$04,$00,$30,$55, $25,$04,$00,$31,$2a
        
        ;RRA (zp,X) (ROR + ADC)
        dta     $63,<a0,$60, $05,$02,$00,$31,$01, $86,$02,$00,$b0,$80
        
        ;NOP zp
        dta     $64,<d5,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $64,<d5,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;RRA zp (ROR + ADC)
        dta     $67,<d5,$60, $05,$00,$00,$31,$01, $86,$00,$00,$b0,$80
        
        ;ARR #imm (AND + ROR + N/V fiddling)
        dta     $6B,$2a,$60, $f0,$00,$00,$31,$01, $90,$00,$00,$b0,$01
        dta     $6B,$6a,$60, $f0,$00,$00,$31,$01, $b0,$00,$00,$f0,$01
        dta     $6B,$aa,$60, $f0,$00,$00,$31,$01, $d0,$00,$00,$f1,$01
        dta     $6B,$ea,$60, $f0,$00,$00,$31,$01, $f0,$00,$00,$b1,$01

        ;RRA abs (ROR + ADC)
        dta     $6F,<d5,>d5, $05,$00,$00,$31,$01, $86,$00,$00,$b0,$80
        
        ;RRA (zp),Y (ROR + ADC)
        dta     $73,<a2,$60, $05,$00,$01,$31,$01, $86,$00,$01,$b0,$80
        
        ;NOP zp,X
        dta     $74,<d5,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $74,<d5,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;RRA zp,X (ROR + ADC)
        dta     $77,<d4,$60, $05,$01,$00,$31,$01, $86,$01,$00,$b0,$80
        
        ;NOP
        dta     $7A,$60,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $7A,$60,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;RRA abs,Y (ROR + ADC)
        dta     $7B,<d4,>d4, $05,$00,$01,$31,$01, $86,$00,$01,$b0,$80
        
        ;NOP abs,X
        dta     $7C,<d5,>d5, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $7C,<d5,>d5, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;RRA abs,X (ROR + ADC)
        dta     $7F,<d4,>d4, $05,$01,$00,$31,$01, $86,$01,$00,$b0,$80
        
        ;NOP #imm
        dta     $80,$00,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $80,$00,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;NOP #imm
        dta     $82,$00,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $82,$00,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SAX (zp,X) (store A&X)
        dta     $83,<(a1-$ff),$60, $ff,$ff,$00,$31,$00, $ff,$ff,$00,$31,$ff
        dta     $83,<(a1-$55),$60, $ff,$55,$00,$31,$00, $ff,$55,$00,$31,$55
        dta     $83,<(a1-$ff),$60, $aa,$ff,$00,$31,$00, $aa,$ff,$00,$31,$aa
        dta     $83,<(a1-$00),$60, $00,$00,$00,$31,$00, $00,$00,$00,$31,$00
        
        ;SAX zp (store A&X)
        dta     $87,<d5,$60, $ff,$ff,$00,$31,$00, $ff,$ff,$00,$31,$ff
        dta     $87,<d5,$60, $ff,$55,$00,$31,$00, $ff,$55,$00,$31,$55
        dta     $87,<d5,$60, $aa,$ff,$00,$31,$00, $aa,$ff,$00,$31,$aa
        dta     $87,<d5,$60, $00,$00,$00,$31,$00, $00,$00,$00,$31,$00
        
        ;XAA **** ($8B)
        
        ;SAX abs (store A&X)
        dta     $8F,<d5,>d5, $ff,$ff,$00,$31,$00, $ff,$ff,$00,$31,$ff
        dta     $8F,<d5,>d5, $ff,$55,$00,$31,$00, $ff,$55,$00,$31,$55
        dta     $8F,<d5,>d5, $aa,$ff,$00,$31,$00, $aa,$ff,$00,$31,$aa
        dta     $8F,<d5,>d5, $00,$00,$00,$31,$00, $00,$00,$00,$31,$00

        ;NOP #imm
        dta     $89,$00,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $89,$00,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;SHA (zp),Y
        ; Test disabled -- AND mask is unstable on different CPUs.
        ;dta        $93,<a2,$60, $00,$00,$01,$30,$55, $00,$00,$01,$30,$00
        ;dta        $93,<a2,$60, $33,$55,$01,$30,$55, $33,$55,$01,$30,$01
        ;dta        $93,<a3,$60, $ff,$55,$ff,$30,$55, $ff,$55,$ff,$30,$00
        
        ;SAX zp,Y (store A&X)
        dta     $97,<d4,$60, $ff,$ff,$01,$31,$00, $ff,$ff,$01,$31,$ff
        dta     $97,<d4,$60, $ff,$55,$01,$31,$00, $ff,$55,$01,$31,$55
        dta     $97,<d4,$60, $aa,$ff,$01,$31,$00, $aa,$ff,$01,$31,$aa
        dta     $97,<d4,$60, $00,$00,$01,$31,$00, $00,$00,$01,$31,$00
        
        ;**** XAS $9B
        
        ;SHY abs,X
        dta     $9C,<d3,>d3, $00,$02,$00,$30,$aa, $00,$02,$00,$30,$00
        dta     $9C,<d3,>d3, $00,$02,$01,$30,$aa, $00,$02,$01,$30,$01
        dta     $9C,<d3,>d3, $00,$02,$ff,$30,$aa, $00,$02,$ff,$30,$01
        dta     $9C,<d5,$3f, $00,$02,$bf,$30,$aa, $00,$02,$bf,$30,$aa
        dta     $9C,<d6,$3f, $00,$ff,$bf,$30,$aa, $00,$ff,$bf,$30,$00
        
        ;SHX abs,Y
        dta     $9E,<d3,>d3, $00,$00,$02,$30,$aa, $00,$00,$02,$30,$00
        dta     $9E,<d3,>d3, $00,$01,$02,$30,$aa, $00,$01,$02,$30,$01
        dta     $9E,<d3,>d3, $00,$ff,$02,$30,$aa, $00,$ff,$02,$30,$01
        dta     $9E,<d5,$3f, $00,$bf,$00,$30,$aa, $00,$bf,$00,$30,$aa
        dta     $9E,<d6,$3f, $00,$bf,$ff,$30,$aa, $00,$bf,$ff,$30,$00
        
        ;LAX (zp,X) (load A&X)
        dta     $A3,<a0,$60, $aa,$02,$00,$31,$00, $00,$00,$00,$33,$00
        dta     $A3,<a0,$60, $aa,$02,$00,$31,$01, $01,$01,$00,$31,$01
        dta     $A3,<a0,$60, $aa,$02,$00,$31,$ff, $ff,$ff,$00,$b1,$ff
        
        ;LAX zp (load A&X)
        dta     $A7,<d5,$60, $aa,$55,$00,$31,$00, $00,$00,$00,$33,$00
        dta     $A7,<d5,$60, $aa,$55,$00,$31,$01, $01,$01,$00,$31,$01
        dta     $A7,<d5,$60, $aa,$55,$00,$31,$ff, $ff,$ff,$00,$b1,$ff
        
        ;ATX #imm (AND + TAX)
        ; Test disabled - reported unable on an Atari 800. The second test
        ; resulted in A,X=$55 instead of $11.
        ;dta        $AB,$00,$60, $00,$ff,$00,$30,$00, $00,$00,$00,$32,$00
        ;dta        $AB,$55,$60, $33,$ff,$00,$30,$00, $11,$11,$00,$30,$00
        ;dta        $AB,$80,$60, $ff,$ff,$00,$30,$00, $80,$80,$00,$b0,$00
        
        ;LAX abs (load A&X)
        dta     $AF,<d5,>d5, $aa,$55,$00,$31,$00, $00,$00,$00,$33,$00
        dta     $AF,<d5,>d5, $aa,$55,$00,$31,$01, $01,$01,$00,$31,$01
        dta     $AF,<d5,>d5, $aa,$55,$00,$31,$ff, $ff,$ff,$00,$b1,$ff
        
        ;LAX (zp),Y (load A&X)
        dta     $B3,<a2,$60, $aa,$55,$01,$31,$00, $00,$00,$01,$33,$00
        dta     $B3,<a2,$60, $aa,$55,$01,$31,$01, $01,$01,$01,$31,$01
        dta     $B3,<a2,$60, $aa,$55,$01,$31,$ff, $ff,$ff,$01,$b1,$ff
        
        ;LAX zp,Y (load A&X)
        dta     $B7,<d3,$60, $aa,$55,$02,$31,$00, $00,$00,$02,$33,$00
        dta     $B7,<d3,$60, $aa,$55,$02,$31,$01, $01,$01,$02,$31,$01
        dta     $B7,<d3,$60, $aa,$55,$02,$31,$ff, $ff,$ff,$02,$b1,$ff
        
        ;*** LAS $BB
        
        ;LAX abs,Y (load A&X)
        dta     $BF,<d4,>d4, $aa,$55,$01,$31,$00, $00,$00,$01,$33,$00
        dta     $BF,<d4,>d4, $aa,$55,$01,$31,$01, $01,$01,$01,$31,$01
        dta     $BF,<d4,>d4, $aa,$55,$01,$31,$ff, $ff,$ff,$01,$b1,$ff
        
        ;NOP #imm
        dta     $C2,$00,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $C2,$00,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;DCP (zp,X) (DEC + CMP)
        dta     $C3,<a0,$60, $f0,$02,$00,$31,$01, $f0,$02,$00,$b1,$00
        dta     $C3,<a0,$60, $f0,$02,$00,$31,$00, $f0,$02,$00,$b0,$ff
        dta     $C3,<a0,$60, $ff,$02,$00,$31,$01, $ff,$02,$00,$b1,$00
        dta     $C3,<a0,$60, $00,$02,$00,$31,$00, $00,$02,$00,$30,$ff
        
        ;DCP zp (DEC + CMP)
        dta     $C7,<d5,$60, $f0,$55,$00,$31,$01, $f0,$55,$00,$b1,$00
        dta     $C7,<d5,$60, $f0,$55,$00,$31,$00, $f0,$55,$00,$b0,$ff
        dta     $C7,<d5,$60, $ff,$55,$00,$31,$01, $ff,$55,$00,$b1,$00
        dta     $C7,<d5,$60, $00,$55,$00,$31,$00, $00,$55,$00,$30,$ff
        
        ;SBX #imm (A&X -> X, X-imm -> X)
        dta     $CB,$00,$60, $ff,$ff,$00,$31,$01, $ff,$ff,$00,$b1,$01
        dta     $CB,$00,$60, $aa,$55,$00,$31,$01, $aa,$00,$00,$33,$01
        dta     $CB,$00,$60, $f0,$55,$00,$31,$01, $f0,$50,$00,$31,$01
        dta     $CB,$05,$60, $f0,$55,$00,$31,$01, $f0,$4b,$00,$31,$01
        dta     $CB,$85,$60, $f0,$55,$00,$31,$01, $f0,$cb,$00,$b0,$01
        dta     $CB,$50,$60, $f0,$55,$00,$31,$01, $f0,$00,$00,$33,$01

        ;DCP abs (DEC + CMP)
        dta     $CF,<d5,>d5, $f0,$55,$00,$31,$01, $f0,$55,$00,$b1,$00
        dta     $CF,<d5,>d5, $f0,$55,$00,$31,$00, $f0,$55,$00,$b0,$ff
        dta     $CF,<d5,>d5, $ff,$55,$00,$31,$01, $ff,$55,$00,$b1,$00
        dta     $CF,<d5,>d5, $00,$55,$00,$31,$00, $00,$55,$00,$30,$ff
        
        ;DCP (zp),Y (DEC + CMP)
        dta     $D3,<a2,$60, $f0,$55,$01,$31,$01, $f0,$55,$01,$b1,$00
        dta     $D3,<a2,$60, $f0,$55,$01,$31,$00, $f0,$55,$01,$b0,$ff
        dta     $D3,<a2,$60, $ff,$55,$01,$31,$01, $ff,$55,$01,$b1,$00
        dta     $D3,<a2,$60, $00,$55,$01,$31,$00, $00,$55,$01,$30,$ff
        
        ;NOP zp,X
        dta     $D4,<d5,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $D4,<d5,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;DCP zp,X (DEC + CMP)
        dta     $D7,<d3,$60, $f0,$02,$00,$31,$01, $f0,$02,$00,$b1,$00
        dta     $D7,<d3,$60, $f0,$02,$00,$31,$00, $f0,$02,$00,$b0,$ff
        dta     $D7,<d3,$60, $ff,$02,$00,$31,$01, $ff,$02,$00,$b1,$00
        dta     $D7,<d3,$60, $00,$02,$00,$31,$00, $00,$02,$00,$30,$ff
        
        ;NOP
        dta     $DA,$60,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $DA,$60,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;DCP abs,Y (DEC + CMP)
        dta     $DB,<d4,>d4, $f0,$55,$01,$31,$01, $f0,$55,$01,$b1,$00
        dta     $DB,<d4,>d4, $f0,$55,$01,$31,$00, $f0,$55,$01,$b0,$ff
        dta     $DB,<d4,>d4, $ff,$55,$01,$31,$01, $ff,$55,$01,$b1,$00
        dta     $DB,<d4,>d4, $00,$55,$01,$31,$00, $00,$55,$01,$30,$ff
        
        ;NOP abs,X
        dta     $DC,<d5,>d5, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $DC,<d5,>d5, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;DCP abs,X (DEC + CMP)
        dta     $DF,<d4,>d4, $f0,$01,$55,$31,$01, $f0,$01,$55,$b1,$00
        dta     $DF,<d4,>d4, $f0,$01,$55,$31,$00, $f0,$01,$55,$b0,$ff
        dta     $DF,<d4,>d4, $ff,$01,$55,$31,$01, $ff,$01,$55,$b1,$00
        dta     $DF,<d4,>d4, $00,$01,$55,$31,$00, $00,$01,$55,$30,$ff
        
        ;NOP #imm
        dta     $E2,$00,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $E2,$00,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;ISB (zp,X) (INC + SBC)
        dta     $E3,<a0,$60, $00,$02,$00,$31,$00, $ff,$02,$00,$b0,$01
        dta     $E3,<a0,$60, $00,$02,$00,$31,$7f, $80,$02,$00,$f0,$80
        dta     $E3,<a0,$60, $00,$02,$00,$31,$ff, $00,$02,$00,$33,$00
        dta     $E3,<a0,$60, $00,$02,$00,$30,$00, $fe,$02,$00,$b0,$01
        dta     $E3,<a0,$60, $00,$02,$00,$30,$7f, $7f,$02,$00,$30,$80
        dta     $E3,<a0,$60, $00,$02,$00,$30,$ff, $ff,$02,$00,$b0,$00
        dta     $E3,<a0,$60, $55,$02,$00,$31,$00, $54,$02,$00,$31,$01
        dta     $E3,<a0,$60, $55,$02,$00,$31,$7f, $d5,$02,$00,$f0,$80
        dta     $E3,<a0,$60, $55,$02,$00,$31,$ff, $55,$02,$00,$31,$00
        dta     $E3,<a0,$60, $55,$02,$00,$30,$00, $53,$02,$00,$31,$01
        dta     $E3,<a0,$60, $55,$02,$00,$30,$7f, $d4,$02,$00,$f0,$80
        dta     $E3,<a0,$60, $55,$02,$00,$30,$ff, $54,$02,$00,$31,$00
        
        ;ISB zp (INC + SBC)
        dta     $E7,<d5,$60, $00,$00,$00,$31,$00, $ff,$00,$00,$b0,$01
        dta     $E7,<d5,$60, $00,$00,$00,$31,$7f, $80,$00,$00,$f0,$80
        dta     $E7,<d5,$60, $00,$00,$00,$31,$ff, $00,$00,$00,$33,$00
        dta     $E7,<d5,$60, $00,$00,$00,$30,$00, $fe,$00,$00,$b0,$01
        dta     $E7,<d5,$60, $00,$00,$00,$30,$7f, $7f,$00,$00,$30,$80
        dta     $E7,<d5,$60, $00,$00,$00,$30,$ff, $ff,$00,$00,$b0,$00
        dta     $E7,<d5,$60, $55,$00,$00,$31,$00, $54,$00,$00,$31,$01
        dta     $E7,<d5,$60, $55,$00,$00,$31,$7f, $d5,$00,$00,$f0,$80
        dta     $E7,<d5,$60, $55,$00,$00,$31,$ff, $55,$00,$00,$31,$00
        dta     $E7,<d5,$60, $55,$00,$00,$30,$00, $53,$00,$00,$31,$01
        dta     $E7,<d5,$60, $55,$00,$00,$30,$7f, $d4,$00,$00,$f0,$80
        dta     $E7,<d5,$60, $55,$00,$00,$30,$ff, $54,$00,$00,$31,$00
        
        ;SBC #imm
        dta     $EB,$00,$60, $00,$00,$00,$31,$00, $00,$00,$00,$33,$00
        dta     $EB,$01,$60, $00,$00,$00,$31,$00, $ff,$00,$00,$b0,$00
        dta     $EB,$80,$60, $00,$00,$00,$31,$00, $80,$00,$00,$f0,$00
        dta     $EB,$ff,$60, $00,$00,$00,$31,$00, $01,$00,$00,$30,$00
        dta     $EB,$00,$60, $55,$00,$00,$30,$00, $54,$00,$00,$31,$00
        dta     $EB,$01,$60, $55,$00,$00,$30,$00, $53,$00,$00,$31,$00
        dta     $EB,$80,$60, $55,$00,$00,$30,$00, $d4,$00,$00,$f0,$00
        dta     $EB,$ff,$60, $55,$00,$00,$30,$00, $55,$00,$00,$30,$00
        
        ;ISB abs (INC + SBC)
        dta     $EF,<d5,>d5, $00,$00,$00,$31,$00, $ff,$00,$00,$b0,$01
        dta     $EF,<d5,>d5, $00,$00,$00,$31,$7f, $80,$00,$00,$f0,$80
        dta     $EF,<d5,>d5, $00,$00,$00,$31,$ff, $00,$00,$00,$33,$00
        dta     $EF,<d5,>d5, $00,$00,$00,$30,$00, $fe,$00,$00,$b0,$01
        dta     $EF,<d5,>d5, $00,$00,$00,$30,$7f, $7f,$00,$00,$30,$80
        dta     $EF,<d5,>d5, $00,$00,$00,$30,$ff, $ff,$00,$00,$b0,$00
        dta     $EF,<d5,>d5, $55,$00,$00,$31,$00, $54,$00,$00,$31,$01
        dta     $EF,<d5,>d5, $55,$00,$00,$31,$7f, $d5,$00,$00,$f0,$80
        dta     $EF,<d5,>d5, $55,$00,$00,$31,$ff, $55,$00,$00,$31,$00
        dta     $EF,<d5,>d5, $55,$00,$00,$30,$00, $53,$00,$00,$31,$01
        dta     $EF,<d5,>d5, $55,$00,$00,$30,$7f, $d4,$00,$00,$f0,$80
        dta     $EF,<d5,>d5, $55,$00,$00,$30,$ff, $54,$00,$00,$31,$00

        ;ISB (zp),Y (INC + SBC)
        dta     $F3,<a2,$60, $00,$02,$01,$31,$00, $ff,$02,$01,$b0,$01
        dta     $F3,<a2,$60, $00,$02,$01,$31,$7f, $80,$02,$01,$f0,$80
        dta     $F3,<a2,$60, $00,$02,$01,$31,$ff, $00,$02,$01,$33,$00
        dta     $F3,<a2,$60, $00,$02,$01,$30,$00, $fe,$02,$01,$b0,$01
        dta     $F3,<a2,$60, $00,$02,$01,$30,$7f, $7f,$02,$01,$30,$80
        dta     $F3,<a2,$60, $00,$02,$01,$30,$ff, $ff,$02,$01,$b0,$00
        dta     $F3,<a2,$60, $55,$02,$01,$31,$00, $54,$02,$01,$31,$01
        dta     $F3,<a2,$60, $55,$02,$01,$31,$7f, $d5,$02,$01,$f0,$80
        dta     $F3,<a2,$60, $55,$02,$01,$31,$ff, $55,$02,$01,$31,$00
        dta     $F3,<a2,$60, $55,$02,$01,$30,$00, $53,$02,$01,$31,$01
        dta     $F3,<a2,$60, $55,$02,$01,$30,$7f, $d4,$02,$01,$f0,$80
        dta     $F3,<a2,$60, $55,$02,$01,$30,$ff, $54,$02,$01,$31,$00

        ;NOP zp,X
        dta     $F4,<d5,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $F4,<d5,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF

        ;ISB zp,X (INC + SBC)
        dta     $F7,<d3,$60, $00,$02,$00,$31,$00, $ff,$02,$00,$b0,$01
        dta     $F7,<d3,$60, $00,$02,$00,$31,$7f, $80,$02,$00,$f0,$80
        dta     $F7,<d3,$60, $00,$02,$00,$31,$ff, $00,$02,$00,$33,$00
        dta     $F7,<d3,$60, $00,$02,$00,$30,$00, $fe,$02,$00,$b0,$01
        dta     $F7,<d3,$60, $00,$02,$00,$30,$7f, $7f,$02,$00,$30,$80
        dta     $F7,<d3,$60, $00,$02,$00,$30,$ff, $ff,$02,$00,$b0,$00
        dta     $F7,<d3,$60, $55,$02,$00,$31,$00, $54,$02,$00,$31,$01
        dta     $F7,<d3,$60, $55,$02,$00,$31,$7f, $d5,$02,$00,$f0,$80
        dta     $F7,<d3,$60, $55,$02,$00,$31,$ff, $55,$02,$00,$31,$00
        dta     $F7,<d3,$60, $55,$02,$00,$30,$00, $53,$02,$00,$31,$01
        dta     $F7,<d3,$60, $55,$02,$00,$30,$7f, $d4,$02,$00,$f0,$80
        dta     $F7,<d3,$60, $55,$02,$00,$30,$ff, $54,$02,$00,$31,$00

        ;NOP
        dta     $FA,$60,$60, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $FA,$60,$60, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF
        
        ;ISB abs,Y (INC + SBC)
        dta     $FB,<d3,>d3, $00,$00,$02,$31,$00, $ff,$00,$02,$b0,$01
        dta     $FB,<d3,>d3, $00,$00,$02,$31,$7f, $80,$00,$02,$f0,$80
        dta     $FB,<d3,>d3, $00,$00,$02,$31,$ff, $00,$00,$02,$33,$00
        dta     $FB,<d3,>d3, $00,$00,$02,$30,$00, $fe,$00,$02,$b0,$01
        dta     $FB,<d3,>d3, $00,$00,$02,$30,$7f, $7f,$00,$02,$30,$80
        dta     $FB,<d3,>d3, $00,$00,$02,$30,$ff, $ff,$00,$02,$b0,$00
        dta     $FB,<d3,>d3, $55,$00,$02,$31,$00, $54,$00,$02,$31,$01
        dta     $FB,<d3,>d3, $55,$00,$02,$31,$7f, $d5,$00,$02,$f0,$80
        dta     $FB,<d3,>d3, $55,$00,$02,$31,$ff, $55,$00,$02,$31,$00
        dta     $FB,<d3,>d3, $55,$00,$02,$30,$00, $53,$00,$02,$31,$01
        dta     $FB,<d3,>d3, $55,$00,$02,$30,$7f, $d4,$00,$02,$f0,$80
        dta     $FB,<d3,>d3, $55,$00,$02,$30,$ff, $54,$00,$02,$31,$00
        
        ;NOP abs,X
        dta     $FC,<d5,>d5, $00,$00,$00,$30,$00, $00,$00,$00,$30,$00
        dta     $FC,<d5,>d5, $FF,$FF,$FF,$FB,$FF, $FF,$FF,$FF,$FB,$FF

        ;ISB abs,X (INC + SBC)
        dta     $FF,<d3,>d3, $00,$02,$00,$31,$00, $ff,$02,$00,$b0,$01
        dta     $FF,<d3,>d3, $00,$02,$00,$31,$7f, $80,$02,$00,$f0,$80
        dta     $FF,<d3,>d3, $00,$02,$00,$31,$ff, $00,$02,$00,$33,$00
        dta     $FF,<d3,>d3, $00,$02,$00,$30,$00, $fe,$02,$00,$b0,$01
        dta     $FF,<d3,>d3, $00,$02,$00,$30,$7f, $7f,$02,$00,$30,$80
        dta     $FF,<d3,>d3, $00,$02,$00,$30,$ff, $ff,$02,$00,$b0,$00
        dta     $FF,<d3,>d3, $55,$02,$00,$31,$00, $54,$02,$00,$31,$01
        dta     $FF,<d3,>d3, $55,$02,$00,$31,$7f, $d5,$02,$00,$f0,$80
        dta     $FF,<d3,>d3, $55,$02,$00,$31,$ff, $55,$02,$00,$31,$00
        dta     $FF,<d3,>d3, $55,$02,$00,$30,$00, $53,$02,$00,$31,$01
        dta     $FF,<d3,>d3, $55,$02,$00,$30,$7f, $d4,$02,$00,$f0,$80
        dta     $FF,<d3,>d3, $55,$02,$00,$30,$ff, $54,$02,$00,$31,$00
test_end:

;============================================================================
        org     $3f00
insn:
        nop
        nop
        nop
        rts

.ifndef ATARI
    org $fffa
    dta a(0),a(main),a(0)
.else
    run main
.endif
