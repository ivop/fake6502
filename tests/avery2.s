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
    org $0
    dta 0
    .align $0080,0
.else
        org     $0080
.endif

dc_con  dta     $00         ;decompression control byte
dc_in0  dta     $60         ;instruction
dc_in1  dta     $60
dc_in2  dta     $60
        dta     $60
dc_a1   dta     0           ;input data
dc_x1   dta     0
dc_y1   dta     0
dc_p1   dta     0
dc_m1   dta     0
dc_a2   dta     0           ;output data
dc_x2   dta     0
dc_y2   dta     0
dc_p2   dta     0
dc_m2   dta     0

rand0   dta     $78         ;32-bit LFSR
rand1   dta     $56
rand2   dta     $34
rand3   dta     $12
randt   dta     0

dc_conm dta     0           ;decompression: new control byte mask
dc_conc dta     0           ;decompression: new control byte mask bit count
dc_conv dta     0           ;decompression: tracked control byte

.ifndef ATARI
    .align $00c0,0
.else
    org $c0
.endif
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

.ifndef ATARI
    .align $2000,0
.else
        org     $2000
.endif

main:
        mwa     #test_start a0

loop_start:
        ldy     #0
        
        ;check if have to get a new mask byte
        dec     dc_conc
        bpl     no_newmask
        
        mva     (a0),y+ dc_conm
        mva     #7 dc_conc
        
no_newmask:
        ;check next mask bit
        asl     dc_conm
        bcc     no_new_control

        ;load control byte
        mva     (a0),y+ dc_conv
        
no_new_control:
        mva     dc_conv dc_con

        ;check if we should reload the opcode
        asl     dc_con
        bcc     same_opcode
        
        mva     (a0),y+ dc_in0
        lda     #$60
        sta     dc_in1
        sta     dc_in2
        
same_opcode:
        ;update instruction bytes if needed
        asl     dc_con
        scc:mva (a0),y+ dc_in1
        asl     dc_con
        scc:mva (a0),y+ dc_in2
        
        ;compute implicit A1
        jsr     detrand2
        sta     dc_a1
        
        ;decompress X1 and Y1; note that X1 = ~Y1
        lda     (a0),y+
        sta     dc_y1
        eor     #$ff
        sta     dc_x1
        
        ;compute implicit P1 and M1
        jsr     detrand2
        ora     #$30
        and     #$f7
        sta     dc_p1
        
        jsr     detrand2
        sta     dc_m1
                
        ;decompress A2, X2, Y2, P2, and M2
        asl     dc_con
        lda     dc_a1   
        scc:lda (a0),y+
        sta     dc_a2

        asl     dc_con
        lda     dc_x1
        scc:lda (a0),y+
        sta     dc_x2

        asl     dc_con
        lda     dc_y1   
        scc:lda (a0),y+
        sta     dc_y2

        asl     dc_con
        lda     dc_p1   
        scc:lda (a0),y+
        sta     dc_p2

        asl     dc_con
        lda     dc_m1   
        scc:lda (a0),y+
        sta     dc_m2
        
        ;bump test pointer
        tya
        add     a0
        sta     a0
        scc:inc a0+1

        ;setup temp registers
        mwa     #d5 a1
        mwa     #d4 a2
                
        ;stash P
        lda     dc_p1
        pha
        
        ;load d5
        mva     dc_m1 d5

        ;load A, X, Y
        lda     dc_a1
        ldx     dc_x1
        ldy     dc_y1
        
        ;load P
        plp
        
        ;execute insn
        jsr     dc_in0

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
        lda     dc_a2
        cmp     d1
        ;bne     fail
        bne *
        
        cpx     dc_x2
        ;bne     fail
        bne *
        
        cpy     dc_y2
        ;bne     fail
        bne *
        
        lda     dc_p2
        cmp     d4
        ;bne     fail
        bne *
        
        lda     dc_m2
        cmp     d5
        ;bne     fail
        bne *

        ;go another round
        lda     a0      
        cmp     #<test_end
        bne     loop_end
        lda     a0+1
        cmp     #>test_end
        beq     loop_exit
loop_end:
        jmp     loop_start
loop_exit:
success:
        jmp *

fail:
        ldx     #5
copyloop:
        lda     dc_a1-1,x
        sta     d0,x
        dex
        bne     copyloop
        ;jsr        _imprintf
        ;dta        c"Input: A=%x X=%x Y=%x P=%x M=%x",$9b,0

        mva     dc_in0 d1
        mva     dc_in1 d2
        mva     dc_in2 d3
        
        ldy     #>failmsg
        lda     #<failmsg
        jmp *
        ;jsr        _testFailed2
failmsg:
        ;dta        c"  Insn: %x %x %x",0

;============================================================================
; This LFSR generator must match detrand2() in cputest.cpp.
;
.proc   detrand2
        lda     rand0
        pha
        
        ;shift LFSR
        mva     rand1 rand0
        mva     rand2 rand1
        mva     rand3 rand2
        
        ;bit 31
        pla
        sta     rand3
        pha
        
        ;bit 29 (shr 2)
        mvx     #0 randt
        lsr
        ror     randt   
        lsr
        ror     randt
        tax
        eor     rand3
        sta     rand3
        lda     rand2
        eor     randt
        sta     rand2
        txa
        
        ;bit 25 (shr +4)
        lsr
        ror     randt
        lsr
        ror     randt
        lsr
        ror     randt
        lsr
        ror     randt
        tax
        eor     rand3
        sta     rand3
        lda     rand2
        eor     randt
        sta     rand2
        txa
        
        ;bit 24 (shr +1)
        lsr
        ror     randt
        eor     rand3
        sta     rand3
        lda     rand2
        eor     randt
        sta     rand2
        
        pla
        rts
.endp

;============================================================================
test_start:
        ;This test data is produced by cputest.cpp. The format is as follows:
        ;
        ; * Mask byte
        ;   Bits processed from MSB to LSB indicating whether a new control
        ;   byte is included.
        ; * Control byte
        ;   D7=1    Opcode byte follows; reset op1 and op2 bytes to $60 (RTS)
        ;   D6=1    New operand 1 byte follows
        ;   D5=1    New operand 2 byte follows
        ;   D4=1    New A result byte follows
        ;   D3=1    New X result byte follows
        ;   D2=1    New Y result byte follows
        ;   D1=1    New P result byte follows
        ;   D0=1    New M result byte follows
        ; * [Opcode byte]
        ; * [Operand 1 byte]
        ; * [Operand 2 byte]
        ; * Y input byte
        ; * [A result byte]
        ; * [X result byte]
        ; * [Y result byte]
        ; * [P result byte]
        ; * [M result byte]
        ;
        ; A, P, and M input bytes are implicit based on a 32-bit LFSR with
        ; defined seed (see detrand2). The Y input byte is always included,
        ; and the X input byte is the complement of the Y input byte.


        ; $01  ORA (zp,x)
        dta $FC
        dta $D2,$01,$C3,$00,$7C,$74 ;Input: A=$78 Y=$00 P=$76 M=$34  Output: A=$7C X=$FF Y=$00 P=$74 M=$34
        dta $52,$E6,$23,$FA,$B1 ;Input: A=$02 Y=$23 P=$33 M=$FA  Output: A=$FA X=$DC Y=$23 P=$B1 M=$FA
        dta $50,$47,$84,$BF ;Input: A=$B5 Y=$84 P=$B0 M=$8B  Output: A=$BF X=$7B Y=$84 P=$B0 M=$8B
        dta $52,$2F,$6C,$7B,$71 ;Input: A=$38 Y=$6C P=$73 M=$7B  Output: A=$7B X=$93 Y=$6C P=$71 M=$7B
        dta $50,$71,$AE,$BA ;Input: A=$3A Y=$AE P=$B4 M=$AA  Output: A=$BA X=$51 Y=$AE P=$B4 M=$AA
        dta $52,$53,$90,$F8,$B4 ;Input: A=$78 Y=$90 P=$34 M=$D0  Output: A=$F8 X=$6F Y=$90 P=$B4 M=$D0
        dta $B4,$F1,$FB,$B5 ;Input: A=$93 Y=$F1 P=$37 M=$FB  Output: A=$FB X=$0E Y=$F1 P=$B5 M=$FB
        dta $7E,$BB,$FE,$B4 ;Input: A=$4C Y=$BB P=$B6 M=$FE  Output: A=$FE X=$44 Y=$BB P=$B4 M=$FE

        ; $05  ORA zp
        dta $FA
        dta $D2,$05,$CD,$EB,$7F,$30 ;Input: A=$6F Y=$EB P=$B2 M=$34  Output: A=$7F X=$14 Y=$EB P=$30 M=$34
        dta $10,$A6,$7B ;Input: A=$61 Y=$A6 P=$31 M=$3B  Output: A=$7B X=$59 Y=$A6 P=$31 M=$3B
        dta $12,$3C,$7F,$34 ;Input: A=$3F Y=$3C P=$36 M=$51  Output: A=$7F X=$C3 Y=$3C P=$34 M=$51
        dta $10,$0C,$FF ;Input: A=$77 Y=$0C P=$B4 M=$AB  Output: A=$FF X=$F3 Y=$0C P=$B4 M=$AB
        dta $12,$99,$BE,$B1 ;Input: A=$B6 Y=$99 P=$33 M=$88  Output: A=$BE X=$66 Y=$99 P=$B1 M=$88
        dta $5E,$B7,$B0 ;Input: A=$B6 Y=$5E P=$32 M=$27  Output: A=$B7 X=$A1 Y=$5E P=$B0 M=$27
        dta $10,$1C,$BD ;Input: A=$9D Y=$1C P=$B4 M=$2C  Output: A=$BD X=$E3 Y=$1C P=$B4 M=$2C
        dta $B7,$72 ;Input: A=$20 Y=$B7 P=$31 M=$72  Output: A=$72 X=$48 Y=$B7 P=$31 M=$72

        ; $06  ASL zp
        dta $C0
        dta $C3,$06,$CD,$DE,$75,$5A ;Input: A=$E7 Y=$DE P=$76 M=$AD  Output: A=$E7 X=$21 Y=$DE P=$75 M=$5A
        dta $03,$12,$B1,$B4 ;Input: A=$FD Y=$12 P=$30 M=$DA  Output: A=$FD X=$ED Y=$12 P=$B1 M=$B4
        dta $C8,$F0,$D4 ;Input: A=$CB Y=$C8 P=$70 M=$6A  Output: A=$CB X=$37 Y=$C8 P=$F0 M=$D4
        dta $BB,$31,$72 ;Input: A=$94 Y=$BB P=$33 M=$B9  Output: A=$94 X=$44 Y=$BB P=$31 M=$72
        dta $A6,$B1,$96 ;Input: A=$09 Y=$A6 P=$30 M=$CB  Output: A=$09 X=$59 Y=$A6 P=$B1 M=$96
        dta $03,$F0,$B0 ;Input: A=$E2 Y=$03 P=$F3 M=$58  Output: A=$E2 X=$FC Y=$03 P=$F0 M=$B0
        dta $7D,$30,$2E ;Input: A=$37 Y=$7D P=$32 M=$17  Output: A=$37 X=$82 Y=$7D P=$30 M=$2E
        dta $38,$35,$60 ;Input: A=$3D Y=$38 P=$B6 M=$B0  Output: A=$3D X=$C7 Y=$38 P=$35 M=$60

        ; $09  ORA #imm
        dta $C0
        dta $D2,$09,$5D,$1F,$DD,$B4 ;Input: A=$DC Y=$1F P=$36 M=$38  Output: A=$DD X=$E0 Y=$1F P=$B4 M=$38
        dta $52,$CB,$D4,$DF,$F5 ;Input: A=$57 Y=$D4 P=$75 M=$97  Output: A=$DF X=$2B Y=$D4 P=$F5 M=$97
        dta $96,$FC,$96,$B5 ;Input: A=$04 Y=$FC P=$35 M=$D1  Output: A=$96 X=$03 Y=$FC P=$B5 M=$D1
        dta $45,$F5,$ED,$F1 ;Input: A=$A9 Y=$F5 P=$73 M=$A8  Output: A=$ED X=$0A Y=$F5 P=$F1 M=$A8
        dta $13,$3B,$53,$30 ;Input: A=$51 Y=$3B P=$32 M=$80  Output: A=$53 X=$C4 Y=$3B P=$30 M=$80
        dta $89,$0D,$AF,$F0 ;Input: A=$27 Y=$0D P=$70 M=$34  Output: A=$AF X=$F2 Y=$0D P=$F0 M=$34
        dta $1C,$0A,$BD,$F4 ;Input: A=$B1 Y=$0A P=$F6 M=$F7  Output: A=$BD X=$F5 Y=$0A P=$F4 M=$F7
        dta $AE,$DB,$FF,$B0 ;Input: A=$DF Y=$DB P=$32 M=$31  Output: A=$FF X=$24 Y=$DB P=$B0 M=$31

        ; $0A  ASL
        dta $D8
        dta $92,$0A,$32,$94,$B5 ;Input: A=$CA Y=$32 P=$36 M=$FA  Output: A=$94 X=$CD Y=$32 P=$B5 M=$FA
        dta $12,$9A,$02,$74 ;Input: A=$01 Y=$9A P=$F6 M=$A7  Output: A=$02 X=$65 Y=$9A P=$74 M=$A7
        dta $EE,$00,$37 ;Input: A=$80 Y=$EE P=$35 M=$ED  Output: A=$00 X=$11 Y=$EE P=$37 M=$ED
        dta $10,$78,$1A ;Input: A=$8D Y=$78 P=$35 M=$02  Output: A=$1A X=$87 Y=$78 P=$35 M=$02
        dta $12,$FD,$74,$71 ;Input: A=$BA Y=$FD P=$73 M=$BE  Output: A=$74 X=$02 Y=$FD P=$71 M=$BE
        dta $49,$3C,$34 ;Input: A=$1E Y=$49 P=$35 M=$7E  Output: A=$3C X=$B6 Y=$49 P=$34 M=$7E
        dta $F6,$AC,$B4 ;Input: A=$56 Y=$F6 P=$37 M=$9E  Output: A=$AC X=$09 Y=$F6 P=$B4 M=$9E
        dta $7D,$28,$70 ;Input: A=$14 Y=$7D P=$F0 M=$D6  Output: A=$28 X=$82 Y=$7D P=$70 M=$D6

        ; $0D  ORA abs
        dta $F9
        dta $F2,$0D,$CD,$00,$DC,$FA,$B1 ;Input: A=$C2 Y=$DC P=$31 M=$BA  Output: A=$FA X=$23 Y=$DC P=$B1 M=$BA
        dta $02,$4F,$B4 ;Input: A=$ED Y=$4F P=$36 M=$21  Output: A=$ED X=$B0 Y=$4F P=$B4 M=$21
        dta $12,$F2,$BD,$B0 ;Input: A=$B9 Y=$F2 P=$30 M=$95  Output: A=$BD X=$0D Y=$F2 P=$B0 M=$95
        dta $02,$40,$F4 ;Input: A=$FF Y=$40 P=$74 M=$16  Output: A=$FF X=$BF Y=$40 P=$F4 M=$16
        dta $12,$D0,$F5,$B0 ;Input: A=$71 Y=$D0 P=$32 M=$B4  Output: A=$F5 X=$2F Y=$D0 P=$B0 M=$B4
        dta $C4,$FD,$F0 ;Input: A=$75 Y=$C4 P=$70 M=$8C  Output: A=$FD X=$3B Y=$C4 P=$F0 M=$8C
        dta $B7,$E5,$F1 ;Input: A=$E4 Y=$B7 P=$71 M=$45  Output: A=$E5 X=$48 Y=$B7 P=$F1 M=$45
        dta $02,$3B,$B1 ;Input: A=$F4 Y=$3B P=$31 M=$84  Output: A=$F4 X=$C4 Y=$3B P=$B1 M=$84

        ; $0E  ASL abs
        dta $C0
        dta $E3,$0E,$CD,$00,$22,$74,$76 ;Input: A=$6D Y=$22 P=$75 M=$3B  Output: A=$6D X=$DD Y=$22 P=$74 M=$76
        dta $03,$22,$30,$72 ;Input: A=$88 Y=$22 P=$31 M=$39  Output: A=$88 X=$DD Y=$22 P=$30 M=$72
        dta $9D,$F5,$C2 ;Input: A=$05 Y=$9D P=$77 M=$E1  Output: A=$05 X=$62 Y=$9D P=$F5 M=$C2
        dta $8B,$F4,$CE ;Input: A=$69 Y=$8B P=$F6 M=$67  Output: A=$69 X=$74 Y=$8B P=$F4 M=$CE
        dta $DA,$F4,$CE ;Input: A=$ED Y=$DA P=$76 M=$67  Output: A=$ED X=$25 Y=$DA P=$F4 M=$CE
        dta $CA,$B1,$D8 ;Input: A=$51 Y=$CA P=$30 M=$EC  Output: A=$51 X=$35 Y=$CA P=$B1 M=$D8
        dta $02,$F4,$D0 ;Input: A=$D9 Y=$02 P=$74 M=$68  Output: A=$D9 X=$FD Y=$02 P=$F4 M=$D0
        dta $72,$74,$48 ;Input: A=$43 Y=$72 P=$75 M=$24  Output: A=$43 X=$8D Y=$72 P=$74 M=$48

        ; $11  ORA (zp),y
        dta $FF
        dta $D2,$11,$C4,$01,$B9,$B4 ;Input: A=$39 Y=$01 P=$34 M=$B1  Output: A=$B9 X=$FE Y=$01 P=$B4 M=$B1
        dta $42,$C2,$00,$F5 ;Input: A=$FB Y=$00 P=$F7 M=$C1  Output: A=$FB X=$FF Y=$00 P=$F5 M=$C1
        dta $52,$C4,$01,$6C,$75 ;Input: A=$44 Y=$01 P=$F5 M=$2C  Output: A=$6C X=$FE Y=$01 P=$75 M=$2C
        dta $12,$01,$7F,$30 ;Input: A=$6B Y=$01 P=$B2 M=$1E  Output: A=$7F X=$FE Y=$01 P=$30 M=$1E
        dta $50,$C2,$00,$FF ;Input: A=$9D Y=$00 P=$F4 M=$6F  Output: A=$FF X=$FF Y=$00 P=$F4 M=$6F
        dta $10,$00,$5F ;Input: A=$17 Y=$00 P=$31 M=$5F  Output: A=$5F X=$FF Y=$00 P=$31 M=$5F
        dta $12,$00,$D7,$B4 ;Input: A=$C7 Y=$00 P=$34 M=$17  Output: A=$D7 X=$FF Y=$00 P=$B4 M=$17
        dta $52,$C4,$01,$9B,$F4 ;Input: A=$1B Y=$01 P=$74 M=$8B  Output: A=$9B X=$FE Y=$01 P=$F4 M=$8B

        ; $15  ORA zp,x
        dta $C3
        dta $D0,$15,$A4,$D6,$AC ;Input: A=$88 Y=$D6 P=$B5 M=$A4  Output: A=$AC X=$29 Y=$D6 P=$B5 M=$A4
        dta $52,$E2,$14,$DB,$F5 ;Input: A=$9A Y=$14 P=$77 M=$43  Output: A=$DB X=$EB Y=$14 P=$F5 M=$43
        dta $4C,$7E,$5B,$35 ;Input: A=$52 Y=$7E P=$B5 M=$09  Output: A=$5B X=$81 Y=$7E P=$35 M=$09
        dta $AA,$DC,$FF,$B1 ;Input: A=$3E Y=$DC P=$31 M=$DB  Output: A=$FF X=$23 Y=$DC P=$B1 M=$DB
        dta $34,$66,$3F,$35 ;Input: A=$0F Y=$66 P=$B7 M=$39  Output: A=$3F X=$99 Y=$66 P=$35 M=$39
        dta $BD,$EF,$7F,$74 ;Input: A=$0D Y=$EF P=$F6 M=$73  Output: A=$7F X=$10 Y=$EF P=$74 M=$73
        dta $42,$17,$49,$70 ;Input: A=$59 Y=$49 P=$72 M=$01  Output: A=$59 X=$B6 Y=$49 P=$70 M=$01
        dta $52,$CD,$FF,$FF,$F0 ;Input: A=$F8 Y=$FF P=$72 M=$1F  Output: A=$FF X=$00 Y=$FF P=$F0 M=$1F

        ; $16  ASL zp,x
        dta $C6
        dta $C3,$16,$5D,$8F,$F1,$DA ;Input: A=$11 Y=$8F P=$70 M=$ED  Output: A=$11 X=$70 Y=$8F P=$F1 M=$DA
        dta $43,$9B,$CD,$F4,$BA ;Input: A=$3E Y=$CD P=$75 M=$5D  Output: A=$3E X=$32 Y=$CD P=$F4 M=$BA
        dta $EC,$1E,$B5,$E6 ;Input: A=$D0 Y=$1E P=$37 M=$F3  Output: A=$D0 X=$E1 Y=$1E P=$B5 M=$E6
        dta $6A,$9C,$F5,$9C ;Input: A=$AB Y=$9C P=$F4 M=$CE  Output: A=$AB X=$63 Y=$9C P=$F5 M=$9C
        dta $40,$72,$B1,$A6 ;Input: A=$F7 Y=$72 P=$32 M=$D3  Output: A=$F7 X=$8D Y=$72 P=$B1 M=$A6
        dta $41,$B4,$E6,$6C ;Input: A=$0D Y=$E6 P=$34 M=$36  Output: A=$0D X=$19 Y=$E6 P=$34 M=$6C
        dta $43,$BE,$F0,$74,$4A ;Input: A=$8B Y=$F0 P=$76 M=$25  Output: A=$8B X=$0F Y=$F0 P=$74 M=$4A
        dta $1D,$4F,$B5,$E8 ;Input: A=$C1 Y=$4F P=$36 M=$F4  Output: A=$C1 X=$B0 Y=$4F P=$B5 M=$E8

        ; $18  CLC
        dta $C8
        dta $82,$18,$77,$36 ;Input: A=$EA Y=$77 P=$37 M=$21  Output: A=$EA X=$88 Y=$77 P=$36 M=$21
        dta $00,$D7 ;Input: A=$37 Y=$D7 P=$B0 M=$BC  Output: A=$37 X=$28 Y=$D7 P=$B0 M=$BC
        dta $39 ;Input: A=$5B Y=$39 P=$F2 M=$03  Output: A=$5B X=$C6 Y=$39 P=$F2 M=$03
        dta $53 ;Input: A=$8A Y=$53 P=$F0 M=$0C  Output: A=$8A X=$AC Y=$53 P=$F0 M=$0C
        dta $02,$C9,$72 ;Input: A=$BF Y=$C9 P=$73 M=$CE  Output: A=$BF X=$36 Y=$C9 P=$72 M=$CE
        dta $1E,$F0 ;Input: A=$4D Y=$1E P=$F1 M=$7C  Output: A=$4D X=$E1 Y=$1E P=$F0 M=$7C
        dta $74,$70 ;Input: A=$11 Y=$74 P=$71 M=$D9  Output: A=$11 X=$8B Y=$74 P=$70 M=$D9
        dta $0C,$32 ;Input: A=$44 Y=$0C P=$33 M=$E4  Output: A=$44 X=$F3 Y=$0C P=$32 M=$E4

        ; $19  ORA abs,y
        dta $DC
        dta $F2,$19,$78,$00,$55,$77,$74 ;Input: A=$75 Y=$55 P=$76 M=$73  Output: A=$77 X=$AA Y=$55 P=$74 M=$73
        dta $52,$AE,$1F,$A3,$B5 ;Input: A=$A1 Y=$1F P=$35 M=$A2  Output: A=$A3 X=$E0 Y=$1F P=$B5 M=$A2
        dta $A9,$24,$EA,$B4 ;Input: A=$E8 Y=$24 P=$34 M=$6A  Output: A=$EA X=$DB Y=$24 P=$B4 M=$6A
        dta $42,$4F,$7E,$B0 ;Input: A=$F9 Y=$7E P=$30 M=$D1  Output: A=$F9 X=$81 Y=$7E P=$B0 M=$D1
        dta $50,$7E,$4F,$AF ;Input: A=$27 Y=$4F P=$F5 M=$AC  Output: A=$AF X=$B0 Y=$4F P=$F5 M=$AC
        dta $52,$AB,$22,$F7,$F0 ;Input: A=$F5 Y=$22 P=$70 M=$22  Output: A=$F7 X=$DD Y=$22 P=$F0 M=$22
        dta $81,$4C,$FB,$B0 ;Input: A=$FA Y=$4C P=$32 M=$09  Output: A=$FB X=$B3 Y=$4C P=$B0 M=$09
        dta $7A,$53,$F6,$B0 ;Input: A=$B6 Y=$53 P=$B2 M=$56  Output: A=$F6 X=$AC Y=$53 P=$B0 M=$56

        ; $1D  ORA abs,x
        dta $C4
        dta $F0,$1D,$7B,$00,$AD,$FF ;Input: A=$3F Y=$AD P=$B4 M=$E3  Output: A=$FF X=$52 Y=$AD P=$B4 M=$E3
        dta $50,$A1,$D3,$2C ;Input: A=$00 Y=$D3 P=$30 M=$2C  Output: A=$2C X=$2C Y=$D3 P=$30 M=$2C
        dta $C3,$F5,$DB ;Input: A=$D9 Y=$F5 P=$F0 M=$CA  Output: A=$DB X=$0A Y=$F5 P=$F0 M=$CA
        dta $A7,$D9,$F3 ;Input: A=$B1 Y=$D9 P=$F5 M=$F2  Output: A=$F3 X=$26 Y=$D9 P=$F5 M=$F2
        dta $78,$AA,$5C ;Input: A=$1C Y=$AA P=$70 M=$50  Output: A=$5C X=$55 Y=$AA P=$70 M=$50
        dta $52,$8E,$C0,$8D,$B1 ;Input: A=$84 Y=$C0 P=$33 M=$8D  Output: A=$8D X=$3F Y=$C0 P=$B1 M=$8D
        dta $79,$AB,$5F,$74 ;Input: A=$5D Y=$AB P=$F4 M=$5B  Output: A=$5F X=$54 Y=$AB P=$74 M=$5B
        dta $4F,$81,$FB,$F1 ;Input: A=$23 Y=$81 P=$73 M=$DB  Output: A=$FB X=$7E Y=$81 P=$F1 M=$DB

        ; $1E  ASL abs,x
        dta $D8
        dta $E3,$1E,$92,$00,$C4,$F0,$90 ;Input: A=$46 Y=$C4 P=$71 M=$48  Output: A=$46 X=$3B Y=$C4 P=$F0 M=$90
        dta $43,$AD,$DF,$F1,$90 ;Input: A=$FB Y=$DF P=$70 M=$C8  Output: A=$FB X=$20 Y=$DF P=$F1 M=$90
        dta $4E,$80,$B5,$EA ;Input: A=$81 Y=$80 P=$37 M=$F5  Output: A=$81 X=$7F Y=$80 P=$B5 M=$EA
        dta $41,$AF,$E1,$A0 ;Input: A=$BE Y=$E1 P=$B0 M=$50  Output: A=$BE X=$1E Y=$E1 P=$B0 M=$A0
        dta $43,$8C,$BE,$B0,$FE ;Input: A=$CE Y=$BE P=$32 M=$7F  Output: A=$CE X=$41 Y=$BE P=$B0 M=$FE
        dta $53,$85,$F1,$F8 ;Input: A=$61 Y=$85 P=$F3 M=$FC  Output: A=$61 X=$7A Y=$85 P=$F1 M=$F8
        dta $55,$87,$F1,$DA ;Input: A=$67 Y=$87 P=$72 M=$ED  Output: A=$67 X=$78 Y=$87 P=$F1 M=$DA
        dta $81,$B3,$B0,$D2 ;Input: A=$53 Y=$B3 P=$33 M=$69  Output: A=$53 X=$4C Y=$B3 P=$B0 M=$D2

        ; $21  AND (zp,x)
        dta $C0
        dta $D2,$21,$E2,$1F,$20,$74 ;Input: A=$FE Y=$1F P=$F4 M=$21  Output: A=$20 X=$E0 Y=$1F P=$74 M=$21
        dta $52,$33,$70,$52,$31 ;Input: A=$F6 Y=$70 P=$B3 M=$53  Output: A=$52 X=$8F Y=$70 P=$31 M=$53
        dta $9D,$DA,$88,$F1 ;Input: A=$9D Y=$DA P=$F3 M=$A8  Output: A=$88 X=$25 Y=$DA P=$F1 M=$A8
        dta $17,$54,$08,$71 ;Input: A=$48 Y=$54 P=$73 M=$3D  Output: A=$08 X=$AB Y=$54 P=$71 M=$3D
        dta $74,$B1,$00,$73 ;Input: A=$31 Y=$B1 P=$71 M=$88  Output: A=$00 X=$4E Y=$B1 P=$73 M=$88
        dta $01,$3E,$54,$71 ;Input: A=$D4 Y=$3E P=$73 M=$5C  Output: A=$54 X=$C1 Y=$3E P=$71 M=$5C
        dta $D8,$15,$00,$73 ;Input: A=$51 Y=$15 P=$F1 M=$84  Output: A=$00 X=$EA Y=$15 P=$73 M=$84
        dta $09,$46,$CC,$B0 ;Input: A=$EC Y=$46 P=$32 M=$DD  Output: A=$CC X=$B9 Y=$46 P=$B0 M=$DD

        ; $24  BIT zp
        dta $C0
        dta $C2,$24,$CD,$04,$B1 ;Input: A=$CE Y=$04 P=$31 M=$98  Output: A=$CE X=$FB Y=$04 P=$B1 M=$98
        dta $02,$73,$71 ;Input: A=$CC Y=$73 P=$31 M=$67  Output: A=$CC X=$8C Y=$73 P=$71 M=$67
        dta $D9,$B1 ;Input: A=$15 Y=$D9 P=$73 M=$B1  Output: A=$15 X=$26 Y=$D9 P=$B1 M=$B1
        dta $2F,$B2 ;Input: A=$41 Y=$2F P=$72 M=$BE  Output: A=$41 X=$D0 Y=$2F P=$B2 M=$BE
        dta $68,$74 ;Input: A=$58 Y=$68 P=$B4 M=$75  Output: A=$58 X=$97 Y=$68 P=$74 M=$75
        dta $F7,$F5 ;Input: A=$42 Y=$F7 P=$B7 M=$FA  Output: A=$42 X=$08 Y=$F7 P=$F5 M=$FA
        dta $4A,$31 ;Input: A=$65 Y=$4A P=$71 M=$11  Output: A=$65 X=$B5 Y=$4A P=$31 M=$11
        dta $57,$71 ;Input: A=$D8 Y=$57 P=$B3 M=$76  Output: A=$D8 X=$A8 Y=$57 P=$71 M=$76

        ; $25  AND zp
        dta $C0
        dta $D2,$25,$CD,$76,$00,$36 ;Input: A=$C5 Y=$76 P=$B6 M=$22  Output: A=$00 X=$89 Y=$76 P=$36 M=$22
        dta $12,$16,$94,$F0 ;Input: A=$B4 Y=$16 P=$F2 M=$D4  Output: A=$94 X=$E9 Y=$16 P=$F0 M=$D4
        dta $11,$00,$36 ;Input: A=$92 Y=$11 P=$B6 M=$08  Output: A=$00 X=$EE Y=$11 P=$36 M=$08
        dta $AE,$00,$73 ;Input: A=$0F Y=$AE P=$71 M=$80  Output: A=$00 X=$51 Y=$AE P=$73 M=$80
        dta $88,$40,$70 ;Input: A=$E8 Y=$88 P=$F2 M=$50  Output: A=$40 X=$77 Y=$88 P=$70 M=$50
        dta $FE,$12,$74 ;Input: A=$D3 Y=$FE P=$F4 M=$1A  Output: A=$12 X=$01 Y=$FE P=$74 M=$1A
        dta $DB,$09,$35 ;Input: A=$6F Y=$DB P=$B5 M=$09  Output: A=$09 X=$24 Y=$DB P=$35 M=$09
        dta $43,$9A,$B1 ;Input: A=$BE Y=$43 P=$33 M=$DB  Output: A=$9A X=$BC Y=$43 P=$B1 M=$DB

        ; $26  ROL zp
        dta $C0
        dta $C1,$26,$CD,$3C,$0E ;Input: A=$0F Y=$3C P=$30 M=$07  Output: A=$0F X=$C3 Y=$3C P=$30 M=$0E
        dta $03,$45,$B5,$B0 ;Input: A=$0D Y=$45 P=$34 M=$D8  Output: A=$0D X=$BA Y=$45 P=$B5 M=$B0
        dta $D8,$B5,$C6 ;Input: A=$68 Y=$D8 P=$B6 M=$E3  Output: A=$68 X=$27 Y=$D8 P=$B5 M=$C6
        dta $CE,$74,$5D ;Input: A=$9C Y=$CE P=$F7 M=$2E  Output: A=$9C X=$31 Y=$CE P=$74 M=$5D
        dta $F5,$31,$70 ;Input: A=$91 Y=$F5 P=$32 M=$B8  Output: A=$91 X=$0A Y=$F5 P=$31 M=$70
        dta $60,$71,$60 ;Input: A=$03 Y=$60 P=$F2 M=$B0  Output: A=$03 X=$9F Y=$60 P=$71 M=$60
        dta $3D,$B4,$B5 ;Input: A=$5F Y=$3D P=$37 M=$5A  Output: A=$5F X=$C2 Y=$3D P=$B4 M=$B5
        dta $27,$B0,$D9 ;Input: A=$9D Y=$27 P=$33 M=$6C  Output: A=$9D X=$D8 Y=$27 P=$B0 M=$D9

        ; $29  AND #imm
        dta $F8
        dta $D2,$29,$76,$59,$42,$71 ;Input: A=$43 Y=$59 P=$73 M=$61  Output: A=$42 X=$A6 Y=$59 P=$71 M=$61
        dta $50,$D0,$2D,$10 ;Input: A=$3C Y=$2D P=$30 M=$54  Output: A=$10 X=$D2 Y=$2D P=$30 M=$54
        dta $52,$C9,$C2,$C0,$F1 ;Input: A=$F0 Y=$C2 P=$73 M=$E6  Output: A=$C0 X=$3D Y=$C2 P=$F1 M=$E6
        dta $40,$68,$CD ;Input: A=$60 Y=$CD P=$74 M=$AE  Output: A=$60 X=$32 Y=$CD P=$74 M=$AE
        dta $52,$49,$D4,$09,$71 ;Input: A=$9D Y=$D4 P=$F1 M=$30  Output: A=$09 X=$2B Y=$D4 P=$71 M=$30
        dta $79,$6A,$08,$35 ;Input: A=$88 Y=$6A P=$B7 M=$7B  Output: A=$08 X=$95 Y=$6A P=$35 M=$7B
        dta $08,$25,$08,$71 ;Input: A=$0C Y=$25 P=$F3 M=$49  Output: A=$08 X=$DA Y=$25 P=$71 M=$49
        dta $40,$61,$40,$35 ;Input: A=$4C Y=$61 P=$B5 M=$25  Output: A=$40 X=$9E Y=$61 P=$35 M=$25

        ; $2A  ROL
        dta $CF
        dta $92,$2A,$14,$E4,$B1 ;Input: A=$F2 Y=$14 P=$B0 M=$05  Output: A=$E4 X=$EB Y=$14 P=$B1 M=$05
        dta $12,$3B,$00,$37 ;Input: A=$80 Y=$3B P=$34 M=$D5  Output: A=$00 X=$C4 Y=$3B P=$37 M=$D5
        dta $A5,$09,$30 ;Input: A=$04 Y=$A5 P=$B3 M=$B1  Output: A=$09 X=$5A Y=$A5 P=$30 M=$B1
        dta $28,$F5,$F5 ;Input: A=$FA Y=$28 P=$F7 M=$4C  Output: A=$F5 X=$D7 Y=$28 P=$F5 M=$4C
        dta $10,$8C,$04 ;Input: A=$02 Y=$8C P=$34 M=$6A  Output: A=$04 X=$73 Y=$8C P=$34 M=$6A
        dta $12,$A9,$A4,$B1 ;Input: A=$D2 Y=$A9 P=$32 M=$F9  Output: A=$A4 X=$56 Y=$A9 P=$B1 M=$F9
        dta $10,$87,$3A ;Input: A=$1D Y=$87 P=$30 M=$4A  Output: A=$3A X=$78 Y=$87 P=$30 M=$4A
        dta $12,$8C,$96,$B1 ;Input: A=$CB Y=$8C P=$B2 M=$0A  Output: A=$96 X=$73 Y=$8C P=$B1 M=$0A

        ; $2C  BIT abs
        dta $F0
        dta $E2,$2C,$CD,$00,$F1,$35 ;Input: A=$23 Y=$F1 P=$77 M=$17  Output: A=$23 X=$0E Y=$F1 P=$35 M=$17
        dta $02,$1D,$F3 ;Input: A=$02 Y=$1D P=$71 M=$E5  Output: A=$02 X=$E2 Y=$1D P=$F3 M=$E5
        dta $00,$95 ;Input: A=$9E Y=$95 P=$74 M=$78  Output: A=$9E X=$6A Y=$95 P=$74 M=$78
        dta $02,$9B,$70 ;Input: A=$1A Y=$9B P=$72 M=$50  Output: A=$1A X=$64 Y=$9B P=$70 M=$50
        dta $C0,$B0 ;Input: A=$BB Y=$C0 P=$30 M=$B3  Output: A=$BB X=$3F Y=$C0 P=$B0 M=$B3
        dta $E9,$74 ;Input: A=$1F Y=$E9 P=$F6 M=$7E  Output: A=$1F X=$16 Y=$E9 P=$74 M=$7E
        dta $9A,$74 ;Input: A=$1E Y=$9A P=$B4 M=$4D  Output: A=$1E X=$65 Y=$9A P=$74 M=$4D
        dta $86,$71 ;Input: A=$A4 Y=$86 P=$F1 M=$6A  Output: A=$A4 X=$79 Y=$86 P=$71 M=$6A

        ; $2D  AND abs
        dta $FB
        dta $F2,$2D,$CD,$00,$B5,$03,$70 ;Input: A=$87 Y=$B5 P=$F0 M=$33  Output: A=$03 X=$4A Y=$B5 P=$70 M=$33
        dta $10,$BF,$A2 ;Input: A=$A3 Y=$BF P=$F5 M=$BA  Output: A=$A2 X=$40 Y=$BF P=$F5 M=$BA
        dta $12,$E7,$34,$34 ;Input: A=$35 Y=$E7 P=$36 M=$FE  Output: A=$34 X=$18 Y=$E7 P=$34 M=$FE
        dta $10,$23,$29 ;Input: A=$69 Y=$23 P=$34 M=$BF  Output: A=$29 X=$DC Y=$23 P=$34 M=$BF
        dta $12,$55,$45,$70 ;Input: A=$F5 Y=$55 P=$72 M=$4D  Output: A=$45 X=$AA Y=$55 P=$70 M=$4D
        dta $38,$A1,$B4 ;Input: A=$ED Y=$38 P=$B6 M=$A3  Output: A=$A1 X=$C7 Y=$38 P=$B4 M=$A3
        dta $10,$D1,$30 ;Input: A=$71 Y=$D1 P=$30 M=$BA  Output: A=$30 X=$2E Y=$D1 P=$30 M=$BA
        dta $12,$6C,$24,$34 ;Input: A=$EE Y=$6C P=$B4 M=$34  Output: A=$24 X=$93 Y=$6C P=$34 M=$34

        ; $2E  ROL abs
        dta $F0
        dta $E3,$2E,$CD,$00,$66,$34,$79 ;Input: A=$73 Y=$66 P=$37 M=$3C  Output: A=$73 X=$99 Y=$66 P=$34 M=$79
        dta $03,$4E,$35,$70 ;Input: A=$D3 Y=$4E P=$34 M=$B8  Output: A=$D3 X=$B1 Y=$4E P=$35 M=$70
        dta $01,$30,$6F ;Input: A=$19 Y=$30 P=$35 M=$B7  Output: A=$19 X=$CF Y=$30 P=$35 M=$6F
        dta $03,$FE,$F0,$F9 ;Input: A=$83 Y=$FE P=$71 M=$7C  Output: A=$83 X=$01 Y=$FE P=$F0 M=$F9
        dta $71,$F4,$C8 ;Input: A=$53 Y=$71 P=$F6 M=$64  Output: A=$53 X=$8E Y=$71 P=$F4 M=$C8
        dta $E2,$71,$5A ;Input: A=$48 Y=$E2 P=$72 M=$AD  Output: A=$48 X=$1D Y=$E2 P=$71 M=$5A
        dta $E2,$B5,$D3 ;Input: A=$CC Y=$E2 P=$37 M=$E9  Output: A=$CC X=$1D Y=$E2 P=$B5 M=$D3
        dta $9B,$34,$19 ;Input: A=$2D Y=$9B P=$B7 M=$0C  Output: A=$2D X=$64 Y=$9B P=$34 M=$19

        ; $31  AND (zp),y
        dta $FF
        dta $D2,$31,$C4,$01,$07,$74 ;Input: A=$7F Y=$01 P=$F4 M=$87  Output: A=$07 X=$FE Y=$01 P=$74 M=$87
        dta $52,$C2,$00,$05,$71 ;Input: A=$CD Y=$00 P=$F1 M=$25  Output: A=$05 X=$FF Y=$00 P=$71 M=$25
        dta $02,$00,$30 ;Input: A=$4B Y=$00 P=$B2 M=$5B  Output: A=$4B X=$FF Y=$00 P=$30 M=$5B
        dta $50,$C4,$01,$10 ;Input: A=$56 Y=$01 P=$34 M=$99  Output: A=$10 X=$FE Y=$01 P=$34 M=$99
        dta $02,$01,$30 ;Input: A=$38 Y=$01 P=$B2 M=$3B  Output: A=$38 X=$FE Y=$01 P=$30 M=$3B
        dta $52,$C2,$00,$24,$70 ;Input: A=$2C Y=$00 P=$F2 M=$E5  Output: A=$24 X=$FF Y=$00 P=$70 M=$E5
        dta $12,$00,$CC,$F1 ;Input: A=$DD Y=$00 P=$F3 M=$CC  Output: A=$CC X=$FF Y=$00 P=$F1 M=$CC
        dta $52,$C4,$01,$40,$70 ;Input: A=$50 Y=$01 P=$F2 M=$47  Output: A=$40 X=$FE Y=$01 P=$70 M=$47

        ; $35  AND zp,x
        dta $E7
        dta $D2,$35,$AD,$DF,$1C,$71 ;Input: A=$1D Y=$DF P=$F1 M=$9E  Output: A=$1C X=$20 Y=$DF P=$71 M=$9E
        dta $50,$F7,$29,$11 ;Input: A=$59 Y=$29 P=$34 M=$15  Output: A=$11 X=$D6 Y=$29 P=$34 M=$15
        dta $52,$0C,$3E,$08,$34 ;Input: A=$2C Y=$3E P=$B6 M=$19  Output: A=$08 X=$C1 Y=$3E P=$34 M=$19
        dta $AE,$E0,$98,$F1 ;Input: A=$F8 Y=$E0 P=$73 M=$9A  Output: A=$98 X=$1F Y=$E0 P=$F1 M=$9A
        dta $E0,$12,$02,$34 ;Input: A=$0F Y=$12 P=$36 M=$B2  Output: A=$02 X=$ED Y=$12 P=$34 M=$B2
        dta $50,$8D,$BF,$09 ;Input: A=$5D Y=$BF P=$70 M=$09  Output: A=$09 X=$40 Y=$BF P=$70 M=$09
        dta $52,$74,$A6,$01,$31 ;Input: A=$13 Y=$A6 P=$33 M=$05  Output: A=$01 X=$59 Y=$A6 P=$31 M=$05
        dta $50,$57,$89,$21 ;Input: A=$A1 Y=$89 P=$75 M=$6B  Output: A=$21 X=$76 Y=$89 P=$75 M=$6B

        ; $36  ROL zp,x
        dta $C0
        dta $C3,$36,$BF,$F1,$F5,$CA ;Input: A=$82 Y=$F1 P=$74 M=$E5  Output: A=$82 X=$0E Y=$F1 P=$F5 M=$CA
        dta $43,$DC,$0E,$F0,$C5 ;Input: A=$FC Y=$0E P=$F1 M=$62  Output: A=$FC X=$F1 Y=$0E P=$F0 M=$C5
        dta $3A,$6C,$34,$13 ;Input: A=$D6 Y=$6C P=$B7 M=$09  Output: A=$D6 X=$93 Y=$6C P=$34 M=$13
        dta $6F,$A1,$F1,$8F ;Input: A=$0F Y=$A1 P=$73 M=$C7  Output: A=$0F X=$5E Y=$A1 P=$F1 M=$8F
        dta $EE,$20,$34,$28 ;Input: A=$E9 Y=$20 P=$36 M=$14  Output: A=$E9 X=$DF Y=$20 P=$34 M=$28
        dta $99,$CB,$B4,$AF ;Input: A=$C2 Y=$CB P=$B5 M=$57  Output: A=$C2 X=$34 Y=$CB P=$B4 M=$AF
        dta $D1,$03,$35,$2A ;Input: A=$1D Y=$03 P=$B6 M=$95  Output: A=$1D X=$FC Y=$03 P=$35 M=$2A
        dta $0F,$41,$75,$70 ;Input: A=$4D Y=$41 P=$76 M=$B8  Output: A=$4D X=$BE Y=$41 P=$75 M=$70

        ; $38  SEC
        dta $DE
        dta $80,$38,$84 ;Input: A=$5D Y=$84 P=$F3 M=$F0  Output: A=$5D X=$7B Y=$84 P=$F3 M=$F0
        dta $00,$14 ;Input: A=$1B Y=$14 P=$71 M=$CF  Output: A=$1B X=$EB Y=$14 P=$71 M=$CF
        dta $65 ;Input: A=$54 Y=$65 P=$B3 M=$26  Output: A=$54 X=$9A Y=$65 P=$B3 M=$26
        dta $02,$28,$33 ;Input: A=$06 Y=$28 P=$32 M=$C2  Output: A=$06 X=$D7 Y=$28 P=$33 M=$C2
        dta $00,$C9 ;Input: A=$BB Y=$C9 P=$F3 M=$10  Output: A=$BB X=$36 Y=$C9 P=$F3 M=$10
        dta $02,$E7,$B5 ;Input: A=$AA Y=$E7 P=$B4 M=$8F  Output: A=$AA X=$18 Y=$E7 P=$B5 M=$8F
        dta $00,$8E ;Input: A=$68 Y=$8E P=$B3 M=$4E  Output: A=$68 X=$71 Y=$8E P=$B3 M=$4E
        dta $36 ;Input: A=$DF Y=$36 P=$71 M=$A6  Output: A=$DF X=$C9 Y=$36 P=$71 M=$A6

        ; $39  AND abs,y
        dta $CC
        dta $F0,$39,$71,$00,$5C,$0E ;Input: A=$5E Y=$5C P=$34 M=$0E  Output: A=$0E X=$A3 Y=$5C P=$34 M=$0E
        dta $52,$A5,$28,$88,$B0 ;Input: A=$C8 Y=$28 P=$30 M=$BF  Output: A=$88 X=$D7 Y=$28 P=$B0 M=$BF
        dta $4F,$7E,$00,$72 ;Input: A=$BD Y=$7E P=$F0 M=$42  Output: A=$00 X=$81 Y=$7E P=$72 M=$42
        dta $5B,$72,$48,$31 ;Input: A=$5D Y=$72 P=$B1 M=$C8  Output: A=$48 X=$8D Y=$72 P=$31 M=$C8
        dta $50,$5C,$71,$10 ;Input: A=$DD Y=$71 P=$35 M=$12  Output: A=$10 X=$8E Y=$71 P=$35 M=$12
        dta $52,$AD,$20,$62,$34 ;Input: A=$76 Y=$20 P=$B6 M=$E2  Output: A=$62 X=$DF Y=$20 P=$34 M=$E2
        dta $92,$3B,$20,$74 ;Input: A=$A2 Y=$3B P=$F6 M=$68  Output: A=$20 X=$C4 Y=$3B P=$74 M=$68
        dta $C7,$06,$90,$B5 ;Input: A=$94 Y=$06 P=$35 M=$B3  Output: A=$90 X=$F9 Y=$06 P=$B5 M=$B3

        ; $3D  AND abs,x
        dta $DD
        dta $F2,$3D,$CC,$00,$FE,$00,$76 ;Input: A=$0B Y=$FE P=$F4 M=$64  Output: A=$00 X=$01 Y=$FE P=$76 M=$64
        dta $52,$C6,$F8,$20,$71 ;Input: A=$66 Y=$F8 P=$F1 M=$B1  Output: A=$20 X=$07 Y=$F8 P=$71 M=$B1
        dta $9B,$CD,$00,$32 ;Input: A=$A8 Y=$CD P=$B0 M=$15  Output: A=$00 X=$32 Y=$CD P=$32 M=$15
        dta $50,$8C,$BE,$2A ;Input: A=$6E Y=$BE P=$31 M=$AB  Output: A=$2A X=$41 Y=$BE P=$31 M=$AB
        dta $12,$BE,$24,$70 ;Input: A=$F4 Y=$BE P=$72 M=$2F  Output: A=$24 X=$41 Y=$BE P=$70 M=$2F
        dta $52,$90,$C2,$20,$35 ;Input: A=$BA Y=$C2 P=$B7 M=$65  Output: A=$20 X=$3D Y=$C2 P=$35 M=$65
        dta $8F,$C1,$10,$35 ;Input: A=$38 Y=$C1 P=$B5 M=$93  Output: A=$10 X=$3E Y=$C1 P=$35 M=$93
        dta $50,$5A,$8C,$0D ;Input: A=$ED Y=$8C P=$70 M=$0D  Output: A=$0D X=$73 Y=$8C P=$70 M=$0D

        ; $3E  ROL abs,x
        dta $C5
        dta $E3,$3E,$7E,$00,$B0,$34,$3A ;Input: A=$9A Y=$B0 P=$B4 M=$1D  Output: A=$9A X=$4F Y=$B0 P=$34 M=$3A
        dta $43,$BE,$F0,$75,$01 ;Input: A=$D2 Y=$F0 P=$77 M=$80  Output: A=$D2 X=$0F Y=$F0 P=$75 M=$01
        dta $75,$A7,$F4,$FE ;Input: A=$76 Y=$A7 P=$76 M=$7F  Output: A=$76 X=$58 Y=$A7 P=$F4 M=$FE
        dta $54,$86,$75,$50 ;Input: A=$17 Y=$86 P=$F6 M=$A8  Output: A=$17 X=$79 Y=$86 P=$75 M=$50
        dta $63,$95,$74,$36 ;Input: A=$D3 Y=$95 P=$76 M=$1B  Output: A=$D3 X=$6A Y=$95 P=$74 M=$36
        dta $41,$A9,$DB,$B1 ;Input: A=$AB Y=$DB P=$B1 M=$D8  Output: A=$AB X=$24 Y=$DB P=$B1 M=$B1
        dta $61,$93,$CC ;Input: A=$27 Y=$93 P=$B4 M=$66  Output: A=$27 X=$6C Y=$93 P=$B4 M=$CC
        dta $43,$B4,$E6,$F4,$B4 ;Input: A=$FE Y=$E6 P=$F6 M=$5A  Output: A=$FE X=$19 Y=$E6 P=$F4 M=$B4

        ; $41  EOR (zp,x)
        dta $CF
        dta $D2,$41,$B8,$F5,$6F,$75 ;Input: A=$FA Y=$F5 P=$77 M=$95  Output: A=$6F X=$0A Y=$F5 P=$75 M=$95
        dta $52,$81,$BE,$84,$B4 ;Input: A=$D1 Y=$BE P=$34 M=$55  Output: A=$84 X=$41 Y=$BE P=$B4 M=$55
        dta $62,$9F,$92,$F5 ;Input: A=$15 Y=$9F P=$77 M=$87  Output: A=$92 X=$60 Y=$9F P=$F5 M=$87
        dta $4D,$8A,$E4,$B0 ;Input: A=$7F Y=$8A P=$32 M=$9B  Output: A=$E4 X=$75 Y=$8A P=$B0 M=$9B
        dta $50,$DE,$1B,$EB ;Input: A=$67 Y=$1B P=$B5 M=$8C  Output: A=$EB X=$E4 Y=$1B P=$B5 M=$8C
        dta $52,$7B,$B8,$A9,$B1 ;Input: A=$2C Y=$B8 P=$31 M=$85  Output: A=$A9 X=$47 Y=$B8 P=$B1 M=$85
        dta $50,$2B,$68,$0F ;Input: A=$44 Y=$68 P=$31 M=$4B  Output: A=$0F X=$97 Y=$68 P=$31 M=$4B
        dta $52,$35,$72,$44,$30 ;Input: A=$3F Y=$72 P=$32 M=$7B  Output: A=$44 X=$8D Y=$72 P=$30 M=$7B

        ; $45  EOR zp
        dta $D0
        dta $D0,$45,$CD,$14,$D6 ;Input: A=$1A Y=$14 P=$F4 M=$CC  Output: A=$D6 X=$EB Y=$14 P=$F4 M=$CC
        dta $10,$CD,$F9 ;Input: A=$B8 Y=$CD P=$F4 M=$41  Output: A=$F9 X=$32 Y=$CD P=$F4 M=$41
        dta $0D,$48 ;Input: A=$6D Y=$0D P=$75 M=$25  Output: A=$48 X=$F2 Y=$0D P=$75 M=$25
        dta $12,$44,$97,$F1 ;Input: A=$7E Y=$44 P=$71 M=$E9  Output: A=$97 X=$BB Y=$44 P=$F1 M=$E9
        dta $B4,$D0,$F4 ;Input: A=$A8 Y=$B4 P=$F6 M=$78  Output: A=$D0 X=$4B Y=$B4 P=$F4 M=$78
        dta $66,$F8,$B5 ;Input: A=$21 Y=$66 P=$35 M=$D9  Output: A=$F8 X=$99 Y=$66 P=$B5 M=$D9
        dta $33,$4F,$75 ;Input: A=$E1 Y=$33 P=$F7 M=$AE  Output: A=$4F X=$CC Y=$33 P=$75 M=$AE
        dta $CB,$6F,$71 ;Input: A=$EB Y=$CB P=$F1 M=$84  Output: A=$6F X=$34 Y=$CB P=$71 M=$84

        ; $46  LSR zp
        dta $C7
        dta $C3,$46,$CD,$10,$75,$64 ;Input: A=$3C Y=$10 P=$F5 M=$C9  Output: A=$3C X=$EF Y=$10 P=$75 M=$64
        dta $03,$4C,$34,$14 ;Input: A=$2E Y=$4C P=$35 M=$28  Output: A=$2E X=$B3 Y=$4C P=$34 M=$14
        dta $03,$30,$6B ;Input: A=$9D Y=$03 P=$B3 M=$D6  Output: A=$9D X=$FC Y=$03 P=$30 M=$6B
        dta $73,$70,$6F ;Input: A=$2C Y=$73 P=$73 M=$DE  Output: A=$2C X=$8C Y=$73 P=$70 M=$6F
        dta $05,$74,$0B ;Input: A=$09 Y=$05 P=$75 M=$16  Output: A=$09 X=$FA Y=$05 P=$74 M=$0B
        dta $01,$31,$41 ;Input: A=$9D Y=$31 P=$35 M=$83  Output: A=$9D X=$CE Y=$31 P=$35 M=$41
        dta $03,$AA,$34,$73 ;Input: A=$1D Y=$AA P=$37 M=$E6  Output: A=$1D X=$55 Y=$AA P=$34 M=$73
        dta $01,$CF,$7D ;Input: A=$AE Y=$CF P=$30 M=$FA  Output: A=$AE X=$30 Y=$CF P=$30 M=$7D

        ; $49  EOR #imm
        dta $EC
        dta $D2,$49,$CA,$B0,$73,$34 ;Input: A=$B9 Y=$B0 P=$B6 M=$96  Output: A=$73 X=$4F Y=$B0 P=$34 M=$96
        dta $50,$FF,$27,$EF ;Input: A=$10 Y=$27 P=$B0 M=$6C  Output: A=$EF X=$D8 Y=$27 P=$B0 M=$6C
        dta $52,$9D,$D8,$4D,$74 ;Input: A=$D0 Y=$D8 P=$F4 M=$E3  Output: A=$4D X=$27 Y=$D8 P=$74 M=$E3
        dta $4D,$14,$DB,$B4 ;Input: A=$96 Y=$14 P=$36 M=$69  Output: A=$DB X=$EB Y=$14 P=$B4 M=$69
        dta $50,$79,$F4,$54 ;Input: A=$2D Y=$F4 P=$74 M=$2F  Output: A=$54 X=$0B Y=$F4 P=$74 M=$2F
        dta $52,$59,$27,$85,$B4 ;Input: A=$DC Y=$27 P=$36 M=$4A  Output: A=$85 X=$D8 Y=$27 P=$B4 M=$4A
        dta $7C,$42,$90,$B5 ;Input: A=$EC Y=$42 P=$35 M=$25  Output: A=$90 X=$BD Y=$42 P=$B5 M=$25
        dta $C1,$9C,$F0,$F1 ;Input: A=$31 Y=$9C P=$73 M=$B8  Output: A=$F0 X=$63 Y=$9C P=$F1 M=$B8

        ; $4A  LSR
        dta $C0
        dta $92,$4A,$F8,$65,$74 ;Input: A=$CA Y=$F8 P=$F7 M=$F4  Output: A=$65 X=$07 Y=$F8 P=$74 M=$F4
        dta $12,$8C,$54,$71 ;Input: A=$A9 Y=$8C P=$70 M=$E4  Output: A=$54 X=$73 Y=$8C P=$71 M=$E4
        dta $20,$3E,$71 ;Input: A=$7D Y=$20 P=$F0 M=$2B  Output: A=$3E X=$DF Y=$20 P=$71 M=$2B
        dta $64,$48,$71 ;Input: A=$91 Y=$64 P=$73 M=$F4  Output: A=$48 X=$9B Y=$64 P=$71 M=$F4
        dta $A6,$03,$75 ;Input: A=$07 Y=$A6 P=$F4 M=$6A  Output: A=$03 X=$59 Y=$A6 P=$75 M=$6A
        dta $95,$0C,$35 ;Input: A=$19 Y=$95 P=$36 M=$3D  Output: A=$0C X=$6A Y=$95 P=$35 M=$3D
        dta $B0,$33,$31 ;Input: A=$67 Y=$B0 P=$B3 M=$C3  Output: A=$33 X=$4F Y=$B0 P=$31 M=$C3
        dta $8D,$50,$34 ;Input: A=$A0 Y=$8D P=$35 M=$DC  Output: A=$50 X=$72 Y=$8D P=$34 M=$DC

        ; $4D  EOR abs
        dta $C0
        dta $F2,$4D,$CD,$00,$2D,$D5,$B5 ;Input: A=$31 Y=$2D P=$35 M=$E4  Output: A=$D5 X=$D2 Y=$2D P=$B5 M=$E4
        dta $12,$E7,$47,$31 ;Input: A=$0F Y=$E7 P=$33 M=$48  Output: A=$47 X=$18 Y=$E7 P=$31 M=$48
        dta $B1,$8E,$F4 ;Input: A=$3D Y=$B1 P=$F6 M=$B3  Output: A=$8E X=$4E Y=$B1 P=$F4 M=$B3
        dta $DE,$0A,$34 ;Input: A=$95 Y=$DE P=$36 M=$9F  Output: A=$0A X=$21 Y=$DE P=$34 M=$9F
        dta $80,$0F,$75 ;Input: A=$A2 Y=$80 P=$F7 M=$AD  Output: A=$0F X=$7F Y=$80 P=$75 M=$AD
        dta $08,$85,$B1 ;Input: A=$F7 Y=$08 P=$33 M=$72  Output: A=$85 X=$F7 Y=$08 P=$B1 M=$72
        dta $E9,$C6,$F0 ;Input: A=$77 Y=$E9 P=$72 M=$B1  Output: A=$C6 X=$16 Y=$E9 P=$F0 M=$B1
        dta $41,$3C,$35 ;Input: A=$9D Y=$41 P=$37 M=$A1  Output: A=$3C X=$BE Y=$41 P=$35 M=$A1

        ; $4E  LSR abs
        dta $C0
        dta $E3,$4E,$CD,$00,$D5,$70,$40 ;Input: A=$90 Y=$D5 P=$73 M=$80  Output: A=$90 X=$2A Y=$D5 P=$70 M=$40
        dta $03,$E4,$34,$38 ;Input: A=$EA Y=$E4 P=$35 M=$70  Output: A=$EA X=$1B Y=$E4 P=$34 M=$38
        dta $9F,$34,$17 ;Input: A=$5F Y=$9F P=$B4 M=$2E  Output: A=$5F X=$60 Y=$9F P=$34 M=$17
        dta $42,$70,$7A ;Input: A=$6F Y=$42 P=$F1 M=$F4  Output: A=$6F X=$BD Y=$42 P=$70 M=$7A
        dta $26,$31,$65 ;Input: A=$87 Y=$26 P=$B3 M=$CB  Output: A=$87 X=$D9 Y=$26 P=$31 M=$65
        dta $4C,$74,$73 ;Input: A=$19 Y=$4C P=$F7 M=$E6  Output: A=$19 X=$B3 Y=$4C P=$74 M=$73
        dta $21,$74,$15 ;Input: A=$ED Y=$21 P=$75 M=$2A  Output: A=$ED X=$DE Y=$21 P=$74 M=$15
        dta $93,$31,$0F ;Input: A=$F3 Y=$93 P=$B2 M=$1F  Output: A=$F3 X=$6C Y=$93 P=$31 M=$0F

        ; $51  EOR (zp),y
        dta $F6
        dta $D2,$51,$C4,$01,$F7,$B1 ;Input: A=$CA Y=$01 P=$31 M=$3D  Output: A=$F7 X=$FE Y=$01 P=$B1 M=$3D
        dta $10,$01,$FF ;Input: A=$24 Y=$01 P=$F4 M=$DB  Output: A=$FF X=$FE Y=$01 P=$F4 M=$DB
        dta $52,$C2,$00,$19,$75 ;Input: A=$EA Y=$00 P=$F5 M=$F3  Output: A=$19 X=$FF Y=$00 P=$75 M=$F3
        dta $12,$00,$27,$74 ;Input: A=$13 Y=$00 P=$F4 M=$34  Output: A=$27 X=$FF Y=$00 P=$74 M=$34
        dta $00,$22,$35 ;Input: A=$67 Y=$00 P=$B7 M=$45  Output: A=$22 X=$FF Y=$00 P=$35 M=$45
        dta $52,$C4,$01,$E6,$B5 ;Input: A=$AB Y=$01 P=$35 M=$4D  Output: A=$E6 X=$FE Y=$01 P=$B5 M=$4D
        dta $12,$01,$B3,$F4 ;Input: A=$6F Y=$01 P=$74 M=$DC  Output: A=$B3 X=$FE Y=$01 P=$F4 M=$DC
        dta $01,$6B,$75 ;Input: A=$FD Y=$01 P=$F5 M=$96  Output: A=$6B X=$FE Y=$01 P=$75 M=$96

        ; $55  EOR zp,x
        dta $F0
        dta $D2,$55,$9C,$CE,$BB,$F4 ;Input: A=$A7 Y=$CE P=$76 M=$1C  Output: A=$BB X=$31 Y=$CE P=$F4 M=$1C
        dta $50,$8C,$BE,$B6 ;Input: A=$A2 Y=$BE P=$B1 M=$14  Output: A=$B6 X=$41 Y=$BE P=$B1 M=$14
        dta $52,$F5,$27,$AF,$B5 ;Input: A=$57 Y=$27 P=$37 M=$F8  Output: A=$AF X=$D8 Y=$27 P=$B5 M=$F8
        dta $50,$C4,$F6,$03 ;Input: A=$23 Y=$F6 P=$31 M=$20  Output: A=$03 X=$09 Y=$F6 P=$31 M=$20
        dta $7B,$AD,$0F ;Input: A=$CE Y=$AD P=$75 M=$C1  Output: A=$0F X=$52 Y=$AD P=$75 M=$C1
        dta $8C,$BE,$BD ;Input: A=$0C Y=$BE P=$F1 M=$B1  Output: A=$BD X=$41 Y=$BE P=$F1 M=$B1
        dta $74,$A6,$DA ;Input: A=$DB Y=$A6 P=$B1 M=$01  Output: A=$DA X=$59 Y=$A6 P=$B1 M=$01
        dta $97,$C9,$42 ;Input: A=$84 Y=$C9 P=$71 M=$C6  Output: A=$42 X=$36 Y=$C9 P=$71 M=$C6

        ; $56  LSR zp,x
        dta $C6
        dta $C1,$56,$37,$69,$2D ;Input: A=$19 Y=$69 P=$30 M=$5A  Output: A=$19 X=$96 Y=$69 P=$30 M=$2D
        dta $43,$02,$34,$34,$30 ;Input: A=$E3 Y=$34 P=$B7 M=$60  Output: A=$E3 X=$CB Y=$34 P=$34 M=$30
        dta $FA,$2C,$31,$69 ;Input: A=$C7 Y=$2C P=$B3 M=$D3  Output: A=$C7 X=$D3 Y=$2C P=$31 M=$69
        dta $CF,$01,$74,$4A ;Input: A=$2B Y=$01 P=$76 M=$94  Output: A=$2B X=$FE Y=$01 P=$74 M=$4A
        dta $E5,$17,$74,$06 ;Input: A=$DF Y=$17 P=$F5 M=$0C  Output: A=$DF X=$E8 Y=$17 P=$74 M=$06
        dta $41,$94,$C6,$6F ;Input: A=$B0 Y=$C6 P=$34 M=$DE  Output: A=$B0 X=$39 Y=$C6 P=$34 M=$6F
        dta $43,$6B,$9D,$35,$3E ;Input: A=$AF Y=$9D P=$37 M=$7D  Output: A=$AF X=$62 Y=$9D P=$35 M=$3E
        dta $74,$A6,$34,$30 ;Input: A=$C9 Y=$A6 P=$B5 M=$60  Output: A=$C9 X=$59 Y=$A6 P=$34 M=$30

        ; $58  CLI
        dta $D9
        dta $82,$58,$0B,$F3 ;Input: A=$95 Y=$0B P=$F7 M=$F3  Output: A=$95 X=$F4 Y=$0B P=$F3 M=$F3
        dta $00,$81 ;Input: A=$47 Y=$81 P=$F1 M=$1E  Output: A=$47 X=$7E Y=$81 P=$F1 M=$1E
        dta $61 ;Input: A=$9F Y=$61 P=$71 M=$1F  Output: A=$9F X=$9E Y=$61 P=$71 M=$1F
        dta $02,$68,$31 ;Input: A=$9B Y=$68 P=$35 M=$C6  Output: A=$9B X=$97 Y=$68 P=$31 M=$C6
        dta $00,$62 ;Input: A=$82 Y=$62 P=$B0 M=$0E  Output: A=$82 X=$9D Y=$62 P=$B0 M=$0E
        dta $54 ;Input: A=$79 Y=$54 P=$31 M=$3B  Output: A=$79 X=$AB Y=$54 P=$31 M=$3B
        dta $E7 ;Input: A=$5B Y=$E7 P=$30 M=$5B  Output: A=$5B X=$18 Y=$E7 P=$30 M=$5B
        dta $02,$17,$B0 ;Input: A=$2F Y=$17 P=$B4 M=$32  Output: A=$2F X=$E8 Y=$17 P=$B0 M=$32

        ; $59  EOR abs,y
        dta $C6
        dta $F2,$59,$55,$00,$78,$EE,$B4 ;Input: A=$6E Y=$78 P=$34 M=$80  Output: A=$EE X=$87 Y=$78 P=$B4 M=$80
        dta $52,$BB,$12,$D5,$F4 ;Input: A=$DA Y=$12 P=$74 M=$0F  Output: A=$D5 X=$ED Y=$12 P=$F4 M=$0F
        dta $B0,$1D,$57,$34 ;Input: A=$FF Y=$1D P=$36 M=$A8  Output: A=$57 X=$E2 Y=$1D P=$34 M=$A8
        dta $A4,$29,$11,$34 ;Input: A=$CE Y=$29 P=$B6 M=$DF  Output: A=$11 X=$D6 Y=$29 P=$34 M=$DF
        dta $5B,$72,$17,$31 ;Input: A=$A5 Y=$72 P=$33 M=$B2  Output: A=$17 X=$8D Y=$72 P=$31 M=$B2
        dta $50,$59,$74,$51 ;Input: A=$74 Y=$74 P=$75 M=$25  Output: A=$51 X=$8B Y=$74 P=$75 M=$25
        dta $52,$53,$7A,$52,$34 ;Input: A=$A5 Y=$7A P=$36 M=$F7  Output: A=$52 X=$85 Y=$7A P=$34 M=$F7
        dta $96,$37,$53,$71 ;Input: A=$B2 Y=$37 P=$73 M=$E1  Output: A=$53 X=$C8 Y=$37 P=$71 M=$E1

        ; $5D  EOR abs,x
        dta $C0
        dta $F2,$5D,$83,$00,$B5,$86,$F5 ;Input: A=$E4 Y=$B5 P=$77 M=$62  Output: A=$86 X=$4A Y=$B5 P=$F5 M=$62
        dta $52,$63,$95,$18,$35 ;Input: A=$83 Y=$95 P=$B5 M=$9B  Output: A=$18 X=$6A Y=$95 P=$35 M=$9B
        dta $C3,$F5,$86,$F4 ;Input: A=$B1 Y=$F5 P=$F6 M=$37  Output: A=$86 X=$0A Y=$F5 P=$F4 M=$37
        dta $4E,$80,$D5,$B0 ;Input: A=$58 Y=$80 P=$B2 M=$8D  Output: A=$D5 X=$7F Y=$80 P=$B0 M=$8D
        dta $BB,$ED,$13,$71 ;Input: A=$EA Y=$ED P=$73 M=$F9  Output: A=$13 X=$12 Y=$ED P=$71 M=$F9
        dta $C2,$F4,$41,$70 ;Input: A=$51 Y=$F4 P=$F0 M=$10  Output: A=$41 X=$0B Y=$F4 P=$70 M=$10
        dta $59,$8B,$C7,$F4 ;Input: A=$63 Y=$8B P=$74 M=$A4  Output: A=$C7 X=$74 Y=$8B P=$F4 M=$A4
        dta $B3,$E5,$06,$30 ;Input: A=$9E Y=$E5 P=$B2 M=$98  Output: A=$06 X=$1A Y=$E5 P=$30 M=$98

        ; $5E  LSR abs,x
        dta $E0
        dta $E3,$5E,$56,$00,$88,$35,$78 ;Input: A=$4A Y=$88 P=$36 M=$F1  Output: A=$4A X=$77 Y=$88 P=$35 M=$78
        dta $41,$C4,$F6,$2E ;Input: A=$81 Y=$F6 P=$75 M=$5D  Output: A=$81 X=$09 Y=$F6 P=$75 M=$2E
        dta $43,$8B,$BD,$35,$7C ;Input: A=$89 Y=$BD P=$B4 M=$F9  Output: A=$89 X=$42 Y=$BD P=$35 M=$7C
        dta $B9,$EB,$30,$7D ;Input: A=$3D Y=$EB P=$B0 M=$FA  Output: A=$3D X=$14 Y=$EB P=$30 M=$7D
        dta $86,$B8,$31,$1F ;Input: A=$0B Y=$B8 P=$32 M=$3F  Output: A=$0B X=$47 Y=$B8 P=$31 M=$1F
        dta $AF,$E1,$34,$3E ;Input: A=$3C Y=$E1 P=$35 M=$7C  Output: A=$3C X=$1E Y=$E1 P=$34 M=$3E
        dta $5D,$8F,$34,$12 ;Input: A=$B8 Y=$8F P=$B5 M=$24  Output: A=$B8 X=$70 Y=$8F P=$34 M=$12
        dta $8A,$BC,$31,$2E ;Input: A=$F2 Y=$BC P=$33 M=$5D  Output: A=$F2 X=$43 Y=$BC P=$31 M=$2E

        ; $61  ADC (zp,x)
        dta $C0
        dta $D2,$61,$B7,$F4,$3C,$75 ;Input: A=$81 Y=$F4 P=$B6 M=$BB  Output: A=$3C X=$0B Y=$F4 P=$75 M=$BB
        dta $52,$25,$62,$F7,$B4 ;Input: A=$0D Y=$62 P=$76 M=$EA  Output: A=$F7 X=$9D Y=$62 P=$B4 M=$EA
        dta $4D,$8A,$B6,$B1 ;Input: A=$F8 Y=$8A P=$72 M=$BE  Output: A=$B6 X=$75 Y=$8A P=$B1 M=$BE
        dta $EA,$27,$23,$31 ;Input: A=$C2 Y=$27 P=$70 M=$61  Output: A=$23 X=$D8 Y=$27 P=$31 M=$61
        dta $BA,$F7,$E7,$B0 ;Input: A=$9E Y=$F7 P=$30 M=$49  Output: A=$E7 X=$08 Y=$F7 P=$B0 M=$49
        dta $4A,$87,$9A,$B1 ;Input: A=$BC Y=$87 P=$72 M=$DE  Output: A=$9A X=$78 Y=$87 P=$B1 M=$DE
        dta $FE,$3B,$F3,$B4 ;Input: A=$D2 Y=$3B P=$34 M=$21  Output: A=$F3 X=$C4 Y=$3B P=$B4 M=$21
        dta $5E,$9B,$10,$75 ;Input: A=$87 Y=$9B P=$F4 M=$89  Output: A=$10 X=$64 Y=$9B P=$75 M=$89

        ; $65  ADC zp
        dta $C0
        dta $D2,$65,$CD,$40,$87,$B5 ;Input: A=$FB Y=$40 P=$35 M=$8B  Output: A=$87 X=$BF Y=$40 P=$B5 M=$8B
        dta $12,$C6,$67,$31 ;Input: A=$72 Y=$C6 P=$B1 M=$F4  Output: A=$67 X=$39 Y=$C6 P=$31 M=$F4
        dta $B1,$BF,$B0 ;Input: A=$06 Y=$B1 P=$F1 M=$B8  Output: A=$BF X=$4E Y=$B1 P=$B0 M=$B8
        dta $30,$DD,$F0 ;Input: A=$5F Y=$30 P=$B1 M=$7D  Output: A=$DD X=$CF Y=$30 P=$F0 M=$7D
        dta $10,$90,$B5 ;Input: A=$97 Y=$10 P=$77 M=$F8  Output: A=$90 X=$EF Y=$10 P=$B5 M=$F8
        dta $75,$37,$31 ;Input: A=$D1 Y=$75 P=$33 M=$65  Output: A=$37 X=$8A Y=$75 P=$31 M=$65
        dta $03,$6C,$34 ;Input: A=$62 Y=$03 P=$75 M=$09  Output: A=$6C X=$FC Y=$03 P=$34 M=$09
        dta $9E,$DB,$B4 ;Input: A=$B1 Y=$9E P=$B5 M=$29  Output: A=$DB X=$61 Y=$9E P=$B4 M=$29

        ; $66  ROR zp
        dta $C6
        dta $C3,$66,$CD,$EF,$31,$02 ;Input: A=$ED Y=$EF P=$B0 M=$05  Output: A=$ED X=$10 Y=$EF P=$31 M=$02
        dta $03,$50,$35,$6A ;Input: A=$0D Y=$50 P=$34 M=$D5  Output: A=$0D X=$AF Y=$50 P=$35 M=$6A
        dta $C8,$75,$57 ;Input: A=$6A Y=$C8 P=$76 M=$AF  Output: A=$6A X=$37 Y=$C8 P=$75 M=$57
        dta $43,$F4,$A4 ;Input: A=$1E Y=$43 P=$F5 M=$48  Output: A=$1E X=$BC Y=$43 P=$F4 M=$A4
        dta $28,$F4,$A3 ;Input: A=$43 Y=$28 P=$F7 M=$46  Output: A=$43 X=$D7 Y=$28 P=$F4 M=$A3
        dta $01,$F6,$70 ;Input: A=$11 Y=$F6 P=$30 M=$E0  Output: A=$11 X=$09 Y=$F6 P=$30 M=$70
        dta $03,$9E,$B4,$A0 ;Input: A=$70 Y=$9E P=$35 M=$40  Output: A=$70 X=$61 Y=$9E P=$B4 M=$A0
        dta $A1,$F0,$C8 ;Input: A=$FA Y=$A1 P=$73 M=$90  Output: A=$FA X=$5E Y=$A1 P=$F0 M=$C8

        ; $69  ADC #imm
        dta $D8
        dta $D2,$69,$31,$48,$FE,$B4 ;Input: A=$CD Y=$48 P=$F4 M=$26  Output: A=$FE X=$B7 Y=$48 P=$B4 M=$26
        dta $52,$CD,$6C,$26,$35 ;Input: A=$59 Y=$6C P=$B4 M=$81  Output: A=$26 X=$93 Y=$6C P=$35 M=$81
        dta $9E,$A4,$57,$75 ;Input: A=$B9 Y=$A4 P=$36 M=$F4  Output: A=$57 X=$5B Y=$A4 P=$75 M=$F4
        dta $50,$81,$DE,$F5 ;Input: A=$74 Y=$DE P=$B0 M=$03  Output: A=$F5 X=$21 Y=$DE P=$B0 M=$03
        dta $52,$8C,$A3,$7F,$71 ;Input: A=$F3 Y=$A3 P=$B0 M=$41  Output: A=$7F X=$5C Y=$A3 P=$71 M=$41
        dta $10,$98,$FA,$B4 ;Input: A=$E9 Y=$98 P=$35 M=$47  Output: A=$FA X=$67 Y=$98 P=$B4 M=$47
        dta $9A,$FF,$01,$35 ;Input: A=$66 Y=$FF P=$37 M=$60  Output: A=$01 X=$00 Y=$FF P=$35 M=$60
        dta $CD,$43,$50,$75 ;Input: A=$83 Y=$43 P=$F4 M=$58  Output: A=$50 X=$BC Y=$43 P=$75 M=$58

        ; $6A  ROR
        dta $C0
        dta $92,$6A,$CF,$59,$31 ;Input: A=$B3 Y=$CF P=$B0 M=$11  Output: A=$59 X=$30 Y=$CF P=$31 M=$11
        dta $12,$C7,$12,$75 ;Input: A=$25 Y=$C7 P=$74 M=$A7  Output: A=$12 X=$38 Y=$C7 P=$75 M=$A7
        dta $59,$45,$75 ;Input: A=$8B Y=$59 P=$74 M=$64  Output: A=$45 X=$A6 Y=$59 P=$75 M=$64
        dta $BD,$3B,$31 ;Input: A=$77 Y=$BD P=$32 M=$0C  Output: A=$3B X=$42 Y=$BD P=$31 M=$0C
        dta $27,$C7,$F4 ;Input: A=$8E Y=$27 P=$77 M=$16  Output: A=$C7 X=$D8 Y=$27 P=$F4 M=$16
        dta $28,$55,$75 ;Input: A=$AB Y=$28 P=$F4 M=$A3  Output: A=$55 X=$D7 Y=$28 P=$75 M=$A3
        dta $5D,$14,$31 ;Input: A=$29 Y=$5D P=$B2 M=$CB  Output: A=$14 X=$A2 Y=$5D P=$31 M=$CB
        dta $5F,$9F,$B4 ;Input: A=$3E Y=$5F P=$B7 M=$D1  Output: A=$9F X=$A0 Y=$5F P=$B4 M=$D1

        ; $6D  ADC abs
        dta $E0
        dta $F2,$6D,$CD,$00,$49,$1D,$31 ;Input: A=$FF Y=$49 P=$B3 M=$1D  Output: A=$1D X=$B6 Y=$49 P=$31 M=$1D
        dta $10,$4E,$B7 ;Input: A=$25 Y=$4E P=$B0 M=$92  Output: A=$B7 X=$B1 Y=$4E P=$B0 M=$92
        dta $12,$3C,$F1,$B4 ;Input: A=$84 Y=$3C P=$F4 M=$6D  Output: A=$F1 X=$C3 Y=$3C P=$B4 M=$6D
        dta $DE,$74,$75 ;Input: A=$AD Y=$DE P=$76 M=$C7  Output: A=$74 X=$21 Y=$DE P=$75 M=$C7
        dta $98,$00,$33 ;Input: A=$D9 Y=$98 P=$71 M=$26  Output: A=$00 X=$67 Y=$98 P=$33 M=$26
        dta $B2,$8F,$F0 ;Input: A=$62 Y=$B2 P=$F3 M=$2C  Output: A=$8F X=$4D Y=$B2 P=$F0 M=$2C
        dta $2B,$1F,$31 ;Input: A=$E3 Y=$2B P=$F1 M=$3B  Output: A=$1F X=$D4 Y=$2B P=$31 M=$3B
        dta $14,$57,$75 ;Input: A=$AD Y=$14 P=$37 M=$A9  Output: A=$57 X=$EB Y=$14 P=$75 M=$A9

        ; $6E  ROR abs
        dta $D0
        dta $E3,$6E,$CD,$00,$BB,$B4,$D7 ;Input: A=$9B Y=$BB P=$37 M=$AE  Output: A=$9B X=$44 Y=$BB P=$B4 M=$D7
        dta $01,$2F,$31 ;Input: A=$1A Y=$2F P=$74 M=$62  Output: A=$1A X=$D0 Y=$2F P=$74 M=$31
        dta $E2,$5D ;Input: A=$5A Y=$E2 P=$74 M=$BA  Output: A=$5A X=$1D Y=$E2 P=$74 M=$5D
        dta $03,$7E,$F4,$BA ;Input: A=$27 Y=$7E P=$75 M=$74  Output: A=$27 X=$81 Y=$7E P=$F4 M=$BA
        dta $1D,$71,$28 ;Input: A=$85 Y=$1D P=$70 M=$51  Output: A=$85 X=$E2 Y=$1D P=$71 M=$28
        dta $84,$F5,$F1 ;Input: A=$36 Y=$84 P=$77 M=$E3  Output: A=$36 X=$7B Y=$84 P=$F5 M=$F1
        dta $1F,$F1,$EE ;Input: A=$70 Y=$1F P=$71 M=$DD  Output: A=$70 X=$E0 Y=$1F P=$F1 M=$EE
        dta $BE,$F0,$FE ;Input: A=$F9 Y=$BE P=$73 M=$FC  Output: A=$F9 X=$41 Y=$BE P=$F0 M=$FE

        ; $71  ADC (zp),y
        dta $D8
        dta $D2,$71,$C4,$01,$03,$35 ;Input: A=$BE Y=$01 P=$F7 M=$44  Output: A=$03 X=$FE Y=$01 P=$35 M=$44
        dta $52,$C2,$00,$37,$31 ;Input: A=$C5 Y=$00 P=$B0 M=$72  Output: A=$37 X=$FF Y=$00 P=$31 M=$72
        dta $C4,$01,$A5,$B4 ;Input: A=$8A Y=$01 P=$B6 M=$1B  Output: A=$A5 X=$FE Y=$01 P=$B4 M=$1B
        dta $12,$01,$FE,$B4 ;Input: A=$D3 Y=$01 P=$77 M=$2A  Output: A=$FE X=$FE Y=$01 P=$B4 M=$2A
        dta $52,$C2,$00,$6D,$34 ;Input: A=$37 Y=$00 P=$F7 M=$35  Output: A=$6D X=$FF Y=$00 P=$34 M=$35
        dta $C4,$01,$74,$30 ;Input: A=$52 Y=$01 P=$B0 M=$22  Output: A=$74 X=$FE Y=$01 P=$30 M=$22
        dta $C2,$00,$22,$35 ;Input: A=$54 Y=$00 P=$B7 M=$CD  Output: A=$22 X=$FF Y=$00 P=$35 M=$CD
        dta $C4,$01,$49,$31 ;Input: A=$D2 Y=$01 P=$32 M=$77  Output: A=$49 X=$FE Y=$01 P=$31 M=$77

        ; $75  ADC zp,x
        dta $C0
        dta $D2,$75,$AA,$DC,$5C,$71 ;Input: A=$90 Y=$DC P=$F0 M=$CC  Output: A=$5C X=$23 Y=$DC P=$71 M=$CC
        dta $52,$BE,$F0,$5C,$34 ;Input: A=$0B Y=$F0 P=$37 M=$50  Output: A=$5C X=$0F Y=$F0 P=$34 M=$50
        dta $DC,$0E,$EE,$B0 ;Input: A=$07 Y=$0E P=$F3 M=$E6  Output: A=$EE X=$F1 Y=$0E P=$B0 M=$E6
        dta $3B,$6D,$D2,$B4 ;Input: A=$97 Y=$6D P=$34 M=$3B  Output: A=$D2 X=$92 Y=$6D P=$B4 M=$3B
        dta $FF,$31,$B1,$F0 ;Input: A=$6F Y=$31 P=$F1 M=$41  Output: A=$B1 X=$CE Y=$31 P=$F0 M=$41
        dta $42,$74,$9B,$B0 ;Input: A=$97 Y=$74 P=$33 M=$03  Output: A=$9B X=$8B Y=$74 P=$B0 M=$03
        dta $5B,$8D,$C4,$B1 ;Input: A=$E2 Y=$8D P=$B3 M=$E1  Output: A=$C4 X=$72 Y=$8D P=$B1 M=$E1
        dta $72,$A4,$5F,$71 ;Input: A=$CF Y=$A4 P=$B2 M=$90  Output: A=$5F X=$5B Y=$A4 P=$71 M=$90

        ; $76  ROR zp,x
        dta $C6
        dta $C3,$76,$A3,$D5,$71,$60 ;Input: A=$B9 Y=$D5 P=$72 M=$C1  Output: A=$B9 X=$2A Y=$D5 P=$71 M=$60
        dta $43,$38,$6A,$31,$54 ;Input: A=$61 Y=$6A P=$30 M=$A9  Output: A=$61 X=$95 Y=$6A P=$31 M=$54
        dta $CA,$FC,$70,$40 ;Input: A=$F5 Y=$FC P=$F0 M=$80  Output: A=$F5 X=$03 Y=$FC P=$70 M=$40
        dta $44,$76,$30,$68 ;Input: A=$FE Y=$76 P=$B2 M=$D0  Output: A=$FE X=$89 Y=$76 P=$30 M=$68
        dta $19,$4B,$F5,$BB ;Input: A=$27 Y=$4B P=$F7 M=$77  Output: A=$27 X=$B4 Y=$4B P=$F5 M=$BB
        dta $41,$39,$6B,$0C ;Input: A=$F4 Y=$6B P=$34 M=$18  Output: A=$F4 X=$94 Y=$6B P=$34 M=$0C
        dta $43,$7F,$B1,$B1,$BB ;Input: A=$53 Y=$B1 P=$33 M=$77  Output: A=$53 X=$4E Y=$B1 P=$B1 M=$BB
        dta $A9,$DB,$75,$6C ;Input: A=$34 Y=$DB P=$74 M=$D9  Output: A=$34 X=$24 Y=$DB P=$75 M=$6C

        ; $78  SEI
        dta $E7
        dta $82,$78,$5A,$B5 ;Input: A=$D3 Y=$5A P=$B1 M=$C8  Output: A=$D3 X=$A5 Y=$5A P=$B5 M=$C8
        dta $02,$72,$F7 ;Input: A=$C7 Y=$72 P=$F3 M=$06  Output: A=$C7 X=$8D Y=$72 P=$F7 M=$06
        dta $00,$82 ;Input: A=$AA Y=$82 P=$B6 M=$65  Output: A=$AA X=$7D Y=$82 P=$B6 M=$65
        dta $71 ;Input: A=$7B Y=$71 P=$B7 M=$8C  Output: A=$7B X=$8E Y=$71 P=$B7 M=$8C
        dta $8C ;Input: A=$A7 Y=$8C P=$B6 M=$8D  Output: A=$A7 X=$73 Y=$8C P=$B6 M=$8D
        dta $02,$5E,$B5 ;Input: A=$BE Y=$5E P=$B1 M=$F6  Output: A=$BE X=$A1 Y=$5E P=$B5 M=$F6
        dta $00,$4E ;Input: A=$A9 Y=$4E P=$74 M=$20  Output: A=$A9 X=$B1 Y=$4E P=$74 M=$20
        dta $02,$FA,$34 ;Input: A=$7F Y=$FA P=$30 M=$94  Output: A=$7F X=$05 Y=$FA P=$34 M=$94

        ; $79  ADC abs,y
        dta $C0
        dta $F0,$79,$9D,$00,$30,$51 ;Input: A=$EA Y=$30 P=$31 M=$66  Output: A=$51 X=$CF Y=$30 P=$31 M=$66
        dta $52,$59,$74,$37,$35 ;Input: A=$4E Y=$74 P=$34 M=$E9  Output: A=$37 X=$8B Y=$74 P=$35 M=$E9
        dta $58,$75,$69,$34 ;Input: A=$5A Y=$75 P=$F4 M=$0F  Output: A=$69 X=$8A Y=$75 P=$34 M=$0F
        dta $71,$5C,$CB,$B4 ;Input: A=$8D Y=$5C P=$35 M=$3D  Output: A=$CB X=$A3 Y=$5C P=$B4 M=$3D
        dta $75,$58,$41,$31 ;Input: A=$62 Y=$58 P=$B3 M=$DE  Output: A=$41 X=$A7 Y=$58 P=$31 M=$DE
        dta $73,$5A,$EF,$B1 ;Input: A=$FE Y=$5A P=$F1 M=$F0  Output: A=$EF X=$A5 Y=$5A P=$B1 M=$F0
        dta $72,$5B,$37,$35 ;Input: A=$6F Y=$5B P=$75 M=$C7  Output: A=$37 X=$A4 Y=$5B P=$35 M=$C7
        dta $AD,$20,$9C,$F0 ;Input: A=$6C Y=$20 P=$73 M=$2F  Output: A=$9C X=$DF Y=$20 P=$F0 M=$2F

        ; $7D  ADC abs,x
        dta $C2
        dta $F2,$7D,$AA,$00,$DC,$EE,$B4 ;Input: A=$9C Y=$DC P=$F4 M=$52  Output: A=$EE X=$23 Y=$DC P=$B4 M=$52
        dta $52,$6B,$9D,$FD,$B0 ;Input: A=$6C Y=$9D P=$30 M=$91  Output: A=$FD X=$62 Y=$9D P=$B0 M=$91
        dta $B7,$E9,$61,$34 ;Input: A=$2F Y=$E9 P=$36 M=$32  Output: A=$61 X=$16 Y=$E9 P=$34 M=$32
        dta $C2,$F4,$D3,$B0 ;Input: A=$94 Y=$F4 P=$F0 M=$3F  Output: A=$D3 X=$0B Y=$F4 P=$B0 M=$3F
        dta $A3,$D5,$EA,$B0 ;Input: A=$46 Y=$D5 P=$72 M=$A4  Output: A=$EA X=$2A Y=$D5 P=$B0 M=$A4
        dta $7A,$AC,$C3,$B0 ;Input: A=$24 Y=$AC P=$32 M=$9F  Output: A=$C3 X=$53 Y=$AC P=$B0 M=$9F
        dta $50,$59,$8B,$0D ;Input: A=$56 Y=$8B P=$31 M=$B6  Output: A=$0D X=$74 Y=$8B P=$31 M=$B6
        dta $77,$A9,$D8 ;Input: A=$CF Y=$A9 P=$B4 M=$09  Output: A=$D8 X=$56 Y=$A9 P=$B4 M=$09

        ; $7E  ROR abs,x
        dta $C0
        dta $E3,$7E,$71,$00,$A3,$34,$7C ;Input: A=$FA Y=$A3 P=$36 M=$F8  Output: A=$FA X=$5C Y=$A3 P=$34 M=$7C
        dta $43,$A9,$DB,$31,$1F ;Input: A=$97 Y=$DB P=$B2 M=$3F  Output: A=$97 X=$24 Y=$DB P=$31 M=$1F
        dta $81,$B3,$F5,$FB ;Input: A=$76 Y=$B3 P=$75 M=$F7  Output: A=$76 X=$4C Y=$B3 P=$F5 M=$FB
        dta $83,$B5,$75,$5C ;Input: A=$84 Y=$B5 P=$F4 M=$B9  Output: A=$84 X=$4A Y=$B5 P=$75 M=$5C
        dta $B6,$E8,$75,$04 ;Input: A=$D0 Y=$E8 P=$F6 M=$09  Output: A=$D0 X=$17 Y=$E8 P=$75 M=$04
        dta $AF,$E1,$71,$5A ;Input: A=$74 Y=$E1 P=$72 M=$B5  Output: A=$74 X=$1E Y=$E1 P=$71 M=$5A
        dta $A3,$D5,$35,$42 ;Input: A=$33 Y=$D5 P=$B4 M=$85  Output: A=$33 X=$2A Y=$D5 P=$35 M=$42
        dta $80,$B2,$F4,$E8 ;Input: A=$F1 Y=$B2 P=$F7 M=$D0  Output: A=$F1 X=$4D Y=$B2 P=$F4 M=$E8

        ; $81  STA (zp,x)
        dta $C0
        dta $C1,$81,$CD,$0A,$C1 ;Input: A=$C1 Y=$0A P=$75 M=$3C  Output: A=$C1 X=$F5 Y=$0A P=$75 M=$C1
        dta $41,$1D,$5A,$20 ;Input: A=$20 Y=$5A P=$75 M=$C3  Output: A=$20 X=$A5 Y=$5A P=$75 M=$20
        dta $AC,$E9,$F3 ;Input: A=$F3 Y=$E9 P=$76 M=$29  Output: A=$F3 X=$16 Y=$E9 P=$76 M=$F3
        dta $20,$5D,$1B ;Input: A=$1B Y=$5D P=$31 M=$C8  Output: A=$1B X=$A2 Y=$5D P=$31 M=$1B
        dta $78,$B5,$B9 ;Input: A=$B9 Y=$B5 P=$33 M=$AF  Output: A=$B9 X=$4A Y=$B5 P=$33 M=$B9
        dta $88,$C5,$2E ;Input: A=$2E Y=$C5 P=$76 M=$2B  Output: A=$2E X=$3A Y=$C5 P=$76 M=$2E
        dta $2F,$6C,$E3 ;Input: A=$E3 Y=$6C P=$F1 M=$4E  Output: A=$E3 X=$93 Y=$6C P=$F1 M=$E3
        dta $5B,$98,$AB ;Input: A=$AB Y=$98 P=$37 M=$D7  Output: A=$AB X=$67 Y=$98 P=$37 M=$AB

        ; $84  STY zp
        dta $C0
        dta $C1,$84,$CD,$57,$57 ;Input: A=$66 Y=$57 P=$30 M=$2A  Output: A=$66 X=$A8 Y=$57 P=$30 M=$57
        dta $01,$87,$87 ;Input: A=$34 Y=$87 P=$76 M=$7C  Output: A=$34 X=$78 Y=$87 P=$76 M=$87
        dta $DF,$DF ;Input: A=$98 Y=$DF P=$B5 M=$68  Output: A=$98 X=$20 Y=$DF P=$B5 M=$DF
        dta $68,$68 ;Input: A=$32 Y=$68 P=$73 M=$E1  Output: A=$32 X=$97 Y=$68 P=$73 M=$68
        dta $A2,$A2 ;Input: A=$5F Y=$A2 P=$F4 M=$68  Output: A=$5F X=$5D Y=$A2 P=$F4 M=$A2
        dta $E6,$E6 ;Input: A=$D9 Y=$E6 P=$B1 M=$93  Output: A=$D9 X=$19 Y=$E6 P=$B1 M=$E6
        dta $C6,$C6 ;Input: A=$E5 Y=$C6 P=$33 M=$34  Output: A=$E5 X=$39 Y=$C6 P=$33 M=$C6
        dta $77,$77 ;Input: A=$AA Y=$77 P=$34 M=$B1  Output: A=$AA X=$88 Y=$77 P=$34 M=$77

        ; $85  STA zp
        dta $C0
        dta $C1,$85,$CD,$8D,$45 ;Input: A=$45 Y=$8D P=$73 M=$CB  Output: A=$45 X=$72 Y=$8D P=$73 M=$45
        dta $01,$8F,$40 ;Input: A=$40 Y=$8F P=$77 M=$36  Output: A=$40 X=$70 Y=$8F P=$77 M=$40
        dta $2A,$7B ;Input: A=$7B Y=$2A P=$33 M=$69  Output: A=$7B X=$D5 Y=$2A P=$33 M=$7B
        dta $14,$E1 ;Input: A=$E1 Y=$14 P=$36 M=$09  Output: A=$E1 X=$EB Y=$14 P=$36 M=$E1
        dta $D4,$74 ;Input: A=$74 Y=$D4 P=$77 M=$7B  Output: A=$74 X=$2B Y=$D4 P=$77 M=$74
        dta $11,$33 ;Input: A=$33 Y=$11 P=$B2 M=$BB  Output: A=$33 X=$EE Y=$11 P=$B2 M=$33
        dta $18,$0E ;Input: A=$0E Y=$18 P=$73 M=$D9  Output: A=$0E X=$E7 Y=$18 P=$73 M=$0E
        dta $AE,$32 ;Input: A=$32 Y=$AE P=$77 M=$C4  Output: A=$32 X=$51 Y=$AE P=$77 M=$32

        ; $86  STX zp
        dta $C0
        dta $C1,$86,$CD,$73,$8C ;Input: A=$C1 Y=$73 P=$74 M=$CF  Output: A=$C1 X=$8C Y=$73 P=$74 M=$8C
        dta $01,$60,$9F ;Input: A=$31 Y=$60 P=$B3 M=$14  Output: A=$31 X=$9F Y=$60 P=$B3 M=$9F
        dta $20,$DF ;Input: A=$18 Y=$20 P=$B7 M=$C6  Output: A=$18 X=$DF Y=$20 P=$B7 M=$DF
        dta $5D,$A2 ;Input: A=$41 Y=$5D P=$34 M=$99  Output: A=$41 X=$A2 Y=$5D P=$34 M=$A2
        dta $2F,$D0 ;Input: A=$33 Y=$2F P=$70 M=$19  Output: A=$33 X=$D0 Y=$2F P=$70 M=$D0
        dta $53,$AC ;Input: A=$D6 Y=$53 P=$37 M=$71  Output: A=$D6 X=$AC Y=$53 P=$37 M=$AC
        dta $D7,$28 ;Input: A=$6B Y=$D7 P=$F3 M=$42  Output: A=$6B X=$28 Y=$D7 P=$F3 M=$28
        dta $59,$A6 ;Input: A=$D6 Y=$59 P=$32 M=$FD  Output: A=$D6 X=$A6 Y=$59 P=$32 M=$A6

        ; $88  DEY
        dta $E0
        dta $86,$88,$0D,$0C,$75 ;Input: A=$27 Y=$0D P=$F5 M=$7A  Output: A=$27 X=$F2 Y=$0C P=$75 M=$7A
        dta $04,$26,$25 ;Input: A=$D2 Y=$26 P=$70 M=$5C  Output: A=$D2 X=$D9 Y=$25 P=$70 M=$5C
        dta $06,$55,$54,$34 ;Input: A=$09 Y=$55 P=$B4 M=$B1  Output: A=$09 X=$AA Y=$54 P=$34 M=$B1
        dta $80,$7F,$71 ;Input: A=$3C Y=$80 P=$F3 M=$68  Output: A=$3C X=$7F Y=$7F P=$71 M=$68
        dta $D9,$D8,$B1 ;Input: A=$16 Y=$D9 P=$31 M=$95  Output: A=$16 X=$26 Y=$D8 P=$B1 M=$95
        dta $F8,$F7,$B5 ;Input: A=$87 Y=$F8 P=$35 M=$21  Output: A=$87 X=$07 Y=$F7 P=$B5 M=$21
        dta $B8,$B7,$F1 ;Input: A=$61 Y=$B8 P=$F3 M=$82  Output: A=$61 X=$47 Y=$B7 P=$F1 M=$82
        dta $D5,$D4,$B0 ;Input: A=$2F Y=$D5 P=$B2 M=$4B  Output: A=$2F X=$2A Y=$D4 P=$B0 M=$4B

        ; $8A  TXA
        dta $D4
        dta $90,$8A,$A0,$5F ;Input: A=$83 Y=$A0 P=$70 M=$F3  Output: A=$5F X=$5F Y=$A0 P=$70 M=$F3
        dta $10,$4E,$B1 ;Input: A=$92 Y=$4E P=$F0 M=$99  Output: A=$B1 X=$B1 Y=$4E P=$F0 M=$99
        dta $9F,$60 ;Input: A=$21 Y=$9F P=$75 M=$F0  Output: A=$60 X=$60 Y=$9F P=$75 M=$F0
        dta $12,$FC,$03,$35 ;Input: A=$3A Y=$FC P=$B7 M=$61  Output: A=$03 X=$03 Y=$FC P=$35 M=$61
        dta $36,$C9,$B4 ;Input: A=$D2 Y=$36 P=$B6 M=$B7  Output: A=$C9 X=$C9 Y=$36 P=$B4 M=$B7
        dta $10,$8E,$71 ;Input: A=$14 Y=$8E P=$70 M=$D6  Output: A=$71 X=$71 Y=$8E P=$70 M=$D6
        dta $B0,$4F ;Input: A=$E1 Y=$B0 P=$31 M=$19  Output: A=$4F X=$4F Y=$B0 P=$31 M=$19
        dta $BF,$40 ;Input: A=$E7 Y=$BF P=$35 M=$2B  Output: A=$40 X=$40 Y=$BF P=$35 M=$2B

        ; $8C  STY abs
        dta $C0
        dta $E1,$8C,$CD,$00,$B1,$B1 ;Input: A=$8D Y=$B1 P=$32 M=$08  Output: A=$8D X=$4E Y=$B1 P=$32 M=$B1
        dta $01,$85,$85 ;Input: A=$4F Y=$85 P=$71 M=$26  Output: A=$4F X=$7A Y=$85 P=$71 M=$85
        dta $12,$12 ;Input: A=$68 Y=$12 P=$B3 M=$04  Output: A=$68 X=$ED Y=$12 P=$B3 M=$12
        dta $8B,$8B ;Input: A=$5F Y=$8B P=$F1 M=$A6  Output: A=$5F X=$74 Y=$8B P=$F1 M=$8B
        dta $EC,$EC ;Input: A=$07 Y=$EC P=$77 M=$85  Output: A=$07 X=$13 Y=$EC P=$77 M=$EC
        dta $E2,$E2 ;Input: A=$5E Y=$E2 P=$F4 M=$3F  Output: A=$5E X=$1D Y=$E2 P=$F4 M=$E2
        dta $F3,$F3 ;Input: A=$E3 Y=$F3 P=$F0 M=$B5  Output: A=$E3 X=$0C Y=$F3 P=$F0 M=$F3
        dta $F4,$F4 ;Input: A=$BA Y=$F4 P=$31 M=$18  Output: A=$BA X=$0B Y=$F4 P=$31 M=$F4

        ; $8D  STA abs
        dta $C0
        dta $E1,$8D,$CD,$00,$06,$87 ;Input: A=$87 Y=$06 P=$71 M=$67  Output: A=$87 X=$F9 Y=$06 P=$71 M=$87
        dta $01,$B9,$CC ;Input: A=$CC Y=$B9 P=$73 M=$C2  Output: A=$CC X=$46 Y=$B9 P=$73 M=$CC
        dta $04,$D7 ;Input: A=$D7 Y=$04 P=$77 M=$76  Output: A=$D7 X=$FB Y=$04 P=$77 M=$D7
        dta $2D,$C2 ;Input: A=$C2 Y=$2D P=$32 M=$DF  Output: A=$C2 X=$D2 Y=$2D P=$32 M=$C2
        dta $E0,$66 ;Input: A=$66 Y=$E0 P=$34 M=$14  Output: A=$66 X=$1F Y=$E0 P=$34 M=$66
        dta $01,$3E ;Input: A=$3E Y=$01 P=$36 M=$63  Output: A=$3E X=$FE Y=$01 P=$36 M=$3E
        dta $F8,$15 ;Input: A=$15 Y=$F8 P=$35 M=$B1  Output: A=$15 X=$07 Y=$F8 P=$35 M=$15
        dta $90,$44 ;Input: A=$44 Y=$90 P=$76 M=$E2  Output: A=$44 X=$6F Y=$90 P=$76 M=$44

        ; $8E  STX abs
        dta $C0
        dta $E1,$8E,$CD,$00,$A2,$5D ;Input: A=$06 Y=$A2 P=$70 M=$90  Output: A=$06 X=$5D Y=$A2 P=$70 M=$5D
        dta $01,$16,$E9 ;Input: A=$4C Y=$16 P=$37 M=$0D  Output: A=$4C X=$E9 Y=$16 P=$37 M=$E9
        dta $08,$F7 ;Input: A=$1F Y=$08 P=$74 M=$40  Output: A=$1F X=$F7 Y=$08 P=$74 M=$F7
        dta $1C,$E3 ;Input: A=$8C Y=$1C P=$B0 M=$DE  Output: A=$8C X=$E3 Y=$1C P=$B0 M=$E3
        dta $37,$C8 ;Input: A=$79 Y=$37 P=$34 M=$DB  Output: A=$79 X=$C8 Y=$37 P=$34 M=$C8
        dta $37,$C8 ;Input: A=$BD Y=$37 P=$76 M=$15  Output: A=$BD X=$C8 Y=$37 P=$76 M=$C8
        dta $72,$8D ;Input: A=$21 Y=$72 P=$B5 M=$62  Output: A=$21 X=$8D Y=$72 P=$B5 M=$8D
        dta $F2,$0D ;Input: A=$96 Y=$F2 P=$F7 M=$57  Output: A=$96 X=$0D Y=$F2 P=$F7 M=$0D

        ; $91  STA (zp),y
        dta $D8
        dta $C1,$91,$C2,$00,$8F ;Input: A=$8F Y=$00 P=$B2 M=$D2  Output: A=$8F X=$FF Y=$00 P=$B2 M=$8F
        dta $41,$C4,$01,$A1 ;Input: A=$A1 Y=$01 P=$33 M=$CD  Output: A=$A1 X=$FE Y=$01 P=$33 M=$A1
        dta $C2,$00,$62 ;Input: A=$62 Y=$00 P=$B0 M=$C5  Output: A=$62 X=$FF Y=$00 P=$B0 M=$62
        dta $01,$00,$30 ;Input: A=$30 Y=$00 P=$73 M=$7D  Output: A=$30 X=$FF Y=$00 P=$73 M=$30
        dta $41,$C4,$01,$56 ;Input: A=$56 Y=$01 P=$F6 M=$2A  Output: A=$56 X=$FE Y=$01 P=$F6 M=$56
        dta $C2,$00,$17 ;Input: A=$17 Y=$00 P=$B6 M=$A1  Output: A=$17 X=$FF Y=$00 P=$B6 M=$17
        dta $C4,$01,$92 ;Input: A=$92 Y=$01 P=$F6 M=$36  Output: A=$92 X=$FE Y=$01 P=$F6 M=$92
        dta $C2,$00,$66 ;Input: A=$66 Y=$00 P=$71 M=$E9  Output: A=$66 X=$FF Y=$00 P=$71 M=$66

        ; $94  STY zp,x
        dta $C0
        dta $C1,$94,$D6,$08,$08 ;Input: A=$EF Y=$08 P=$70 M=$4E  Output: A=$EF X=$F7 Y=$08 P=$70 M=$08
        dta $41,$0D,$3F,$3F ;Input: A=$73 Y=$3F P=$F6 M=$43  Output: A=$73 X=$C0 Y=$3F P=$F6 M=$3F
        dta $AE,$E0,$E0 ;Input: A=$B6 Y=$E0 P=$72 M=$BF  Output: A=$B6 X=$1F Y=$E0 P=$72 M=$E0
        dta $EA,$1C,$1C ;Input: A=$66 Y=$1C P=$34 M=$27  Output: A=$66 X=$E3 Y=$1C P=$34 M=$1C
        dta $AD,$DF,$DF ;Input: A=$47 Y=$DF P=$76 M=$17  Output: A=$47 X=$20 Y=$DF P=$76 M=$DF
        dta $2D,$5F,$5F ;Input: A=$7C Y=$5F P=$B3 M=$CC  Output: A=$7C X=$A0 Y=$5F P=$B3 M=$5F
        dta $AB,$DD,$DD ;Input: A=$1A Y=$DD P=$B0 M=$08  Output: A=$1A X=$22 Y=$DD P=$B0 M=$DD
        dta $87,$B9,$B9 ;Input: A=$21 Y=$B9 P=$F4 M=$B1  Output: A=$21 X=$46 Y=$B9 P=$F4 M=$B9

        ; $95  STA zp,x
        dta $C0
        dta $C1,$95,$E9,$1B,$8C ;Input: A=$8C Y=$1B P=$71 M=$33  Output: A=$8C X=$E4 Y=$1B P=$71 M=$8C
        dta $41,$74,$A6,$B6 ;Input: A=$B6 Y=$A6 P=$72 M=$3A  Output: A=$B6 X=$59 Y=$A6 P=$72 M=$B6
        dta $EB,$1D,$0B ;Input: A=$0B Y=$1D P=$74 M=$6D  Output: A=$0B X=$E2 Y=$1D P=$74 M=$0B
        dta $B3,$E5,$CE ;Input: A=$CE Y=$E5 P=$71 M=$52  Output: A=$CE X=$1A Y=$E5 P=$71 M=$CE
        dta $8A,$BC,$53 ;Input: A=$53 Y=$BC P=$71 M=$28  Output: A=$53 X=$43 Y=$BC P=$71 M=$53
        dta $F9,$2B,$6D ;Input: A=$6D Y=$2B P=$F0 M=$BE  Output: A=$6D X=$D4 Y=$2B P=$F0 M=$6D
        dta $45,$77,$0C ;Input: A=$0C Y=$77 P=$B7 M=$E2  Output: A=$0C X=$88 Y=$77 P=$B7 M=$0C
        dta $E6,$18,$BA ;Input: A=$BA Y=$18 P=$B5 M=$7D  Output: A=$BA X=$E7 Y=$18 P=$B5 M=$BA

        ; $96  STX zp,y
        dta $C0
        dta $C1,$96,$C3,$0A,$F5 ;Input: A=$C4 Y=$0A P=$71 M=$DF  Output: A=$C4 X=$F5 Y=$0A P=$71 M=$F5
        dta $41,$63,$6A,$95 ;Input: A=$FB Y=$6A P=$71 M=$4C  Output: A=$FB X=$95 Y=$6A P=$71 M=$95
        dta $8B,$42,$BD ;Input: A=$30 Y=$42 P=$F1 M=$D0  Output: A=$30 X=$BD Y=$42 P=$F1 M=$BD
        dta $5C,$71,$8E ;Input: A=$FE Y=$71 P=$F2 M=$13  Output: A=$FE X=$8E Y=$71 P=$F2 M=$8E
        dta $BE,$0F,$F0 ;Input: A=$62 Y=$0F P=$77 M=$6C  Output: A=$62 X=$F0 Y=$0F P=$77 M=$F0
        dta $0A,$C3,$3C ;Input: A=$DB Y=$C3 P=$71 M=$21  Output: A=$DB X=$3C Y=$C3 P=$71 M=$3C
        dta $F2,$DB,$24 ;Input: A=$6C Y=$DB P=$B1 M=$E0  Output: A=$6C X=$24 Y=$DB P=$B1 M=$24
        dta $9D,$30,$CF ;Input: A=$41 Y=$30 P=$B0 M=$D4  Output: A=$41 X=$CF Y=$30 P=$B0 M=$CF

        ; $98  TYA
        dta $F7
        dta $92,$98,$1A,$1A,$30 ;Input: A=$1C Y=$1A P=$B0 M=$73  Output: A=$1A X=$E5 Y=$1A P=$30 M=$73
        dta $12,$F8,$F8,$F1 ;Input: A=$AB Y=$F8 P=$73 M=$5D  Output: A=$F8 X=$07 Y=$F8 P=$F1 M=$5D
        dta $10,$22,$22 ;Input: A=$54 Y=$22 P=$70 M=$EA  Output: A=$22 X=$DD Y=$22 P=$70 M=$EA
        dta $12,$3C,$3C,$30 ;Input: A=$B3 Y=$3C P=$B0 M=$B3  Output: A=$3C X=$C3 Y=$3C P=$30 M=$B3
        dta $3D,$3D,$74 ;Input: A=$B8 Y=$3D P=$F4 M=$DD  Output: A=$3D X=$C2 Y=$3D P=$74 M=$DD
        dta $10,$B3,$B3 ;Input: A=$0C Y=$B3 P=$B5 M=$4F  Output: A=$B3 X=$4C Y=$B3 P=$B5 M=$4F
        dta $12,$FD,$FD,$B1 ;Input: A=$C0 Y=$FD P=$31 M=$DB  Output: A=$FD X=$02 Y=$FD P=$B1 M=$DB
        dta $10,$9E,$9E ;Input: A=$DD Y=$9E P=$B4 M=$1B  Output: A=$9E X=$61 Y=$9E P=$B4 M=$1B

        ; $99  STA abs,y
        dta $C1
        dta $E1,$99,$7D,$00,$50,$61 ;Input: A=$61 Y=$50 P=$70 M=$00  Output: A=$61 X=$AF Y=$50 P=$70 M=$61
        dta $41,$4F,$7E,$1B ;Input: A=$1B Y=$7E P=$B0 M=$45  Output: A=$1B X=$81 Y=$7E P=$B0 M=$1B
        dta $79,$54,$9A ;Input: A=$9A Y=$54 P=$75 M=$63  Output: A=$9A X=$AB Y=$54 P=$75 M=$9A
        dta $C9,$04,$89 ;Input: A=$89 Y=$04 P=$71 M=$D5  Output: A=$89 X=$FB Y=$04 P=$71 M=$89
        dta $60,$6D,$0C ;Input: A=$0C Y=$6D P=$36 M=$FA  Output: A=$0C X=$92 Y=$6D P=$36 M=$0C
        dta $84,$49,$CA ;Input: A=$CA Y=$49 P=$B3 M=$91  Output: A=$CA X=$B6 Y=$49 P=$B3 M=$CA
        dta $B9,$14,$FA ;Input: A=$FA Y=$14 P=$F0 M=$A4  Output: A=$FA X=$EB Y=$14 P=$F0 M=$FA
        dta $40,$8E,$3F ;Input: A=$2A Y=$3F P=$76 M=$2A  Output: A=$2A X=$C0 Y=$3F P=$76 M=$2A

        ; $9D  STA abs,x
        dta $C0
        dta $E1,$9D,$C0,$00,$F2,$F2 ;Input: A=$F2 Y=$F2 P=$34 M=$2A  Output: A=$F2 X=$0D Y=$F2 P=$34 M=$F2
        dta $41,$B6,$E8,$8C ;Input: A=$8C Y=$E8 P=$74 M=$45  Output: A=$8C X=$17 Y=$E8 P=$74 M=$8C
        dta $7B,$AD,$08 ;Input: A=$08 Y=$AD P=$B4 M=$B6  Output: A=$08 X=$52 Y=$AD P=$B4 M=$08
        dta $93,$C5,$65 ;Input: A=$65 Y=$C5 P=$72 M=$86  Output: A=$65 X=$3A Y=$C5 P=$72 M=$65
        dta $50,$82,$86 ;Input: A=$86 Y=$82 P=$F1 M=$FB  Output: A=$86 X=$7D Y=$82 P=$F1 M=$86
        dta $60,$92,$30 ;Input: A=$30 Y=$92 P=$32 M=$3D  Output: A=$30 X=$6D Y=$92 P=$32 M=$30
        dta $62,$94,$67 ;Input: A=$67 Y=$94 P=$B0 M=$CC  Output: A=$67 X=$6B Y=$94 P=$B0 M=$67
        dta $7E,$B0,$A0 ;Input: A=$A0 Y=$B0 P=$F7 M=$37  Output: A=$A0 X=$4F Y=$B0 P=$F7 M=$A0

        ; $A0  LDY #imm
        dta $DA
        dta $C4,$A0,$AF,$83,$AF ;Input: A=$3D Y=$83 P=$B1 M=$98  Output: A=$3D X=$7C Y=$AF P=$B1 M=$98
        dta $46,$95,$63,$95,$F4 ;Input: A=$F4 Y=$63 P=$74 M=$F8  Output: A=$F4 X=$9C Y=$95 P=$F4 M=$F8
        dta $E3,$41,$E3,$F1 ;Input: A=$85 Y=$41 P=$73 M=$44  Output: A=$85 X=$BE Y=$E3 P=$F1 M=$44
        dta $44,$DD,$BD,$DD ;Input: A=$9A Y=$BD P=$B5 M=$DE  Output: A=$9A X=$42 Y=$DD P=$B5 M=$DE
        dta $46,$58,$A1,$58,$71 ;Input: A=$88 Y=$A1 P=$F1 M=$E9  Output: A=$88 X=$5E Y=$58 P=$71 M=$E9
        dta $4F,$DA,$4F,$35 ;Input: A=$DB Y=$DA P=$37 M=$D1  Output: A=$DB X=$25 Y=$4F P=$35 M=$D1
        dta $44,$38,$31,$38 ;Input: A=$CB Y=$31 P=$35 M=$AA  Output: A=$CB X=$CE Y=$38 P=$35 M=$AA
        dta $FD,$CE,$FD ;Input: A=$9D Y=$CE P=$B5 M=$72  Output: A=$9D X=$31 Y=$FD P=$B5 M=$72

        ; $A1  LDA (zp,x)
        dta $CC
        dta $D2,$A1,$8D,$CA,$1F,$35 ;Input: A=$8D Y=$CA P=$B7 M=$1F  Output: A=$1F X=$35 Y=$CA P=$35 M=$1F
        dta $52,$1F,$5C,$27,$75 ;Input: A=$01 Y=$5C P=$77 M=$27  Output: A=$27 X=$A3 Y=$5C P=$75 M=$27
        dta $28,$65,$73,$71 ;Input: A=$5E Y=$65 P=$F3 M=$73  Output: A=$73 X=$9A Y=$65 P=$71 M=$73
        dta $2C,$69,$1B,$30 ;Input: A=$6A Y=$69 P=$32 M=$1B  Output: A=$1B X=$96 Y=$69 P=$30 M=$1B
        dta $50,$F3,$30,$98 ;Input: A=$92 Y=$30 P=$F5 M=$98  Output: A=$98 X=$CF Y=$30 P=$F5 M=$98
        dta $52,$72,$AF,$90,$F1 ;Input: A=$F1 Y=$AF P=$F3 M=$90  Output: A=$90 X=$50 Y=$AF P=$F1 M=$90
        dta $A5,$E2,$A7,$B5 ;Input: A=$DB Y=$E2 P=$35 M=$A7  Output: A=$A7 X=$1D Y=$E2 P=$B5 M=$A7
        dta $79,$B6,$02,$71 ;Input: A=$AD Y=$B6 P=$F1 M=$02  Output: A=$02 X=$49 Y=$B6 P=$71 M=$02

        ; $A2  LDX #imm
        dta $C3
        dta $CA,$A2,$BE,$4B,$BE,$B1 ;Input: A=$23 Y=$4B P=$33 M=$6B  Output: A=$23 X=$BE Y=$4B P=$B1 M=$6B
        dta $48,$56,$00,$56 ;Input: A=$08 Y=$00 P=$31 M=$91  Output: A=$08 X=$56 Y=$00 P=$31 M=$91
        dta $87,$2F,$87 ;Input: A=$40 Y=$2F P=$B4 M=$0F  Output: A=$40 X=$87 Y=$2F P=$B4 M=$0F
        dta $6A,$84,$6A ;Input: A=$36 Y=$84 P=$71 M=$4E  Output: A=$36 X=$6A Y=$84 P=$71 M=$4E
        dta $36,$54,$36 ;Input: A=$38 Y=$54 P=$75 M=$42  Output: A=$38 X=$36 Y=$54 P=$75 M=$42
        dta $6C,$5F,$6C ;Input: A=$CC Y=$5F P=$30 M=$7B  Output: A=$CC X=$6C Y=$5F P=$30 M=$7B
        dta $4A,$3C,$DA,$3C,$35 ;Input: A=$FB Y=$DA P=$B5 M=$C4  Output: A=$FB X=$3C Y=$DA P=$35 M=$C4
        dta $48,$0A,$C5,$0A ;Input: A=$BE Y=$C5 P=$71 M=$1D  Output: A=$BE X=$0A Y=$C5 P=$71 M=$1D

        ; $A4  LDY zp
        dta $F7
        dta $C6,$A4,$CD,$CB,$7C,$34 ;Input: A=$F3 Y=$CB P=$B4 M=$7C  Output: A=$F3 X=$34 Y=$7C P=$34 M=$7C
        dta $06,$3D,$86,$B5 ;Input: A=$F0 Y=$3D P=$35 M=$86  Output: A=$F0 X=$C2 Y=$86 P=$B5 M=$86
        dta $04,$F3,$84 ;Input: A=$42 Y=$F3 P=$F0 M=$84  Output: A=$42 X=$0C Y=$84 P=$F0 M=$84
        dta $06,$0E,$D6,$F1 ;Input: A=$A8 Y=$0E P=$73 M=$D6  Output: A=$A8 X=$F1 Y=$D6 P=$F1 M=$D6
        dta $F2,$1A,$71 ;Input: A=$56 Y=$F2 P=$73 M=$1A  Output: A=$56 X=$0D Y=$1A P=$71 M=$1A
        dta $04,$F4,$AC ;Input: A=$95 Y=$F4 P=$F0 M=$AC  Output: A=$95 X=$0B Y=$AC P=$F0 M=$AC
        dta $06,$8F,$2C,$31 ;Input: A=$22 Y=$8F P=$B3 M=$2C  Output: A=$22 X=$70 Y=$2C P=$31 M=$2C
        dta $04,$18,$60 ;Input: A=$C8 Y=$18 P=$30 M=$60  Output: A=$C8 X=$E7 Y=$60 P=$30 M=$60

        ; $A5  LDA zp
        dta $D6
        dta $D0,$A5,$CD,$4F,$68 ;Input: A=$97 Y=$4F P=$30 M=$68  Output: A=$68 X=$B0 Y=$4F P=$30 M=$68
        dta $10,$3F,$46 ;Input: A=$CB Y=$3F P=$31 M=$46  Output: A=$46 X=$C0 Y=$3F P=$31 M=$46
        dta $47,$3D ;Input: A=$09 Y=$47 P=$75 M=$3D  Output: A=$3D X=$B8 Y=$47 P=$75 M=$3D
        dta $12,$42,$AD,$F5 ;Input: A=$20 Y=$42 P=$75 M=$AD  Output: A=$AD X=$BD Y=$42 P=$F5 M=$AD
        dta $2E,$FB,$F4 ;Input: A=$F2 Y=$2E P=$F6 M=$FB  Output: A=$FB X=$D1 Y=$2E P=$F4 M=$FB
        dta $10,$BE,$13 ;Input: A=$29 Y=$BE P=$70 M=$13  Output: A=$13 X=$41 Y=$BE P=$70 M=$13
        dta $12,$B6,$CD,$B1 ;Input: A=$71 Y=$B6 P=$33 M=$CD  Output: A=$CD X=$49 Y=$B6 P=$B1 M=$CD
        dta $0C,$D1,$B4 ;Input: A=$71 Y=$0C P=$36 M=$D1  Output: A=$D1 X=$F3 Y=$0C P=$B4 M=$D1

        ; $A6  LDX zp
        dta $CF
        dta $C8,$A6,$CD,$1C,$A1 ;Input: A=$9A Y=$1C P=$F0 M=$A1  Output: A=$9A X=$A1 Y=$1C P=$F0 M=$A1
        dta $0A,$74,$42,$35 ;Input: A=$3B Y=$74 P=$B7 M=$42  Output: A=$3B X=$42 Y=$74 P=$35 M=$42
        dta $FE,$8B,$B5 ;Input: A=$D0 Y=$FE P=$37 M=$8B  Output: A=$D0 X=$8B Y=$FE P=$B5 M=$8B
        dta $9D,$E8,$F4 ;Input: A=$B3 Y=$9D P=$74 M=$E8  Output: A=$B3 X=$E8 Y=$9D P=$F4 M=$E8
        dta $08,$6E,$30 ;Input: A=$C0 Y=$6E P=$74 M=$30  Output: A=$C0 X=$30 Y=$6E P=$74 M=$30
        dta $0A,$48,$DC,$B0 ;Input: A=$50 Y=$48 P=$B2 M=$DC  Output: A=$50 X=$DC Y=$48 P=$B0 M=$DC
        dta $08,$44,$4B ;Input: A=$DC Y=$44 P=$31 M=$4B  Output: A=$DC X=$4B Y=$44 P=$31 M=$4B
        dta $0A,$3E,$4D,$35 ;Input: A=$21 Y=$3E P=$37 M=$4D  Output: A=$21 X=$4D Y=$3E P=$35 M=$4D

        ; $A8  TAY
        dta $E3
        dta $86,$A8,$B8,$DE,$F1 ;Input: A=$DE Y=$B8 P=$73 M=$DE  Output: A=$DE X=$47 Y=$DE P=$F1 M=$DE
        dta $04,$87,$1B ;Input: A=$1B Y=$87 P=$71 M=$34  Output: A=$1B X=$78 Y=$1B P=$71 M=$34
        dta $06,$30,$71,$31 ;Input: A=$71 Y=$30 P=$B3 M=$FC  Output: A=$71 X=$CF Y=$71 P=$31 M=$FC
        dta $DD,$5F,$34 ;Input: A=$5F Y=$DD P=$36 M=$9E  Output: A=$5F X=$22 Y=$5F P=$34 M=$9E
        dta $56,$C3,$F5 ;Input: A=$C3 Y=$56 P=$75 M=$FF  Output: A=$C3 X=$A9 Y=$C3 P=$F5 M=$FF
        dta $61,$F0,$B5 ;Input: A=$F0 Y=$61 P=$B7 M=$A1  Output: A=$F0 X=$9E Y=$F0 P=$B5 M=$A1
        dta $04,$6E,$E2 ;Input: A=$E2 Y=$6E P=$B4 M=$15  Output: A=$E2 X=$91 Y=$E2 P=$B4 M=$15
        dta $06,$8C,$46,$70 ;Input: A=$46 Y=$8C P=$F0 M=$92  Output: A=$46 X=$73 Y=$46 P=$70 M=$92

        ; $A9  LDA #imm
        dta $E6
        dta $D2,$A9,$C4,$E7,$C4,$F4 ;Input: A=$04 Y=$E7 P=$76 M=$22  Output: A=$C4 X=$18 Y=$E7 P=$F4 M=$22
        dta $50,$BC,$67,$BC ;Input: A=$AD Y=$67 P=$B1 M=$26  Output: A=$BC X=$98 Y=$67 P=$B1 M=$26
        dta $52,$F2,$2F,$F2,$F1 ;Input: A=$84 Y=$2F P=$73 M=$CA  Output: A=$F2 X=$D0 Y=$2F P=$F1 M=$CA
        dta $C9,$FF,$C9,$B4 ;Input: A=$37 Y=$FF P=$34 M=$46  Output: A=$C9 X=$00 Y=$FF P=$B4 M=$46
        dta $0F,$CC,$0F,$70 ;Input: A=$88 Y=$CC P=$F2 M=$33  Output: A=$0F X=$33 Y=$CC P=$70 M=$33
        dta $50,$90,$13,$90 ;Input: A=$66 Y=$13 P=$F5 M=$8E  Output: A=$90 X=$EC Y=$13 P=$F5 M=$8E
        dta $52,$24,$A9,$24,$30 ;Input: A=$EB Y=$A9 P=$B0 M=$52  Output: A=$24 X=$56 Y=$A9 P=$30 M=$52
        dta $1E,$11,$1E,$31 ;Input: A=$14 Y=$11 P=$33 M=$E7  Output: A=$1E X=$EE Y=$11 P=$31 M=$E7

        ; $AA  TAX
        dta $C3
        dta $8A,$AA,$AD,$3F,$31 ;Input: A=$3F Y=$AD P=$B3 M=$85  Output: A=$3F X=$3F Y=$AD P=$31 M=$85
        dta $0A,$41,$9E,$F0 ;Input: A=$9E Y=$41 P=$72 M=$C8  Output: A=$9E X=$9E Y=$41 P=$F0 M=$C8
        dta $92,$63,$74 ;Input: A=$63 Y=$92 P=$76 M=$C1  Output: A=$63 X=$63 Y=$92 P=$74 M=$C1
        dta $3F,$72,$74 ;Input: A=$72 Y=$3F P=$76 M=$90  Output: A=$72 X=$72 Y=$3F P=$74 M=$90
        dta $F5,$5F,$31 ;Input: A=$5F Y=$F5 P=$B3 M=$14  Output: A=$5F X=$5F Y=$F5 P=$31 M=$14
        dta $AE,$B5,$B1 ;Input: A=$B5 Y=$AE P=$B3 M=$D2  Output: A=$B5 X=$B5 Y=$AE P=$B1 M=$D2
        dta $08,$A9,$EF ;Input: A=$EF Y=$A9 P=$F1 M=$F0  Output: A=$EF X=$EF Y=$A9 P=$F1 M=$F0
        dta $0A,$2B,$46,$30 ;Input: A=$46 Y=$2B P=$B0 M=$EF  Output: A=$46 X=$46 Y=$2B P=$30 M=$EF

        ; $AC  LDY abs
        dta $D4
        dta $E6,$AC,$CD,$00,$51,$3D,$74 ;Input: A=$DA Y=$51 P=$F6 M=$3D  Output: A=$DA X=$AE Y=$3D P=$74 M=$3D
        dta $06,$80,$07,$70 ;Input: A=$8A Y=$80 P=$72 M=$07  Output: A=$8A X=$7F Y=$07 P=$70 M=$07
        dta $2E,$9F,$F5 ;Input: A=$8E Y=$2E P=$F7 M=$9F  Output: A=$8E X=$D1 Y=$9F P=$F5 M=$9F
        dta $04,$1A,$4A ;Input: A=$A2 Y=$1A P=$74 M=$4A  Output: A=$A2 X=$E5 Y=$4A P=$74 M=$4A
        dta $35,$76 ;Input: A=$F7 Y=$35 P=$71 M=$76  Output: A=$F7 X=$CA Y=$76 P=$71 M=$76
        dta $06,$BE,$E4,$B4 ;Input: A=$AB Y=$BE P=$36 M=$E4  Output: A=$AB X=$41 Y=$E4 P=$B4 M=$E4
        dta $65,$55,$34 ;Input: A=$50 Y=$65 P=$36 M=$55  Output: A=$50 X=$9A Y=$55 P=$34 M=$55
        dta $77,$91,$B1 ;Input: A=$3F Y=$77 P=$31 M=$91  Output: A=$3F X=$88 Y=$91 P=$B1 M=$91

        ; $AD  LDA abs
        dta $F0
        dta $F2,$AD,$CD,$00,$57,$33,$34 ;Input: A=$03 Y=$57 P=$36 M=$33  Output: A=$33 X=$A8 Y=$57 P=$34 M=$33
        dta $12,$D3,$79,$75 ;Input: A=$7C Y=$D3 P=$F7 M=$79  Output: A=$79 X=$2C Y=$D3 P=$75 M=$79
        dta $10,$17,$9E ;Input: A=$37 Y=$17 P=$B0 M=$9E  Output: A=$9E X=$E8 Y=$17 P=$B0 M=$9E
        dta $12,$4E,$73,$70 ;Input: A=$14 Y=$4E P=$72 M=$73  Output: A=$73 X=$B1 Y=$4E P=$70 M=$73
        dta $32,$A7,$F5 ;Input: A=$C2 Y=$32 P=$75 M=$A7  Output: A=$A7 X=$CD Y=$32 P=$F5 M=$A7
        dta $0F,$4D,$34 ;Input: A=$62 Y=$0F P=$36 M=$4D  Output: A=$4D X=$F0 Y=$0F P=$34 M=$4D
        dta $10,$F7,$B5 ;Input: A=$41 Y=$10 P=$B7 M=$F7  Output: A=$F7 X=$EF Y=$10 P=$B5 M=$F7
        dta $D2,$61,$30 ;Input: A=$99 Y=$D2 P=$32 M=$61  Output: A=$61 X=$2D Y=$D2 P=$30 M=$61

        ; $AE  LDX abs
        dta $FA
        dta $E8,$AE,$CD,$00,$84,$10 ;Input: A=$DE Y=$84 P=$70 M=$10  Output: A=$DE X=$10 Y=$84 P=$70 M=$10
        dta $0A,$26,$25,$31 ;Input: A=$3C Y=$26 P=$33 M=$25  Output: A=$3C X=$25 Y=$26 P=$31 M=$25
        dta $08,$A2,$97 ;Input: A=$9C Y=$A2 P=$F1 M=$97  Output: A=$9C X=$97 Y=$A2 P=$F1 M=$97
        dta $0A,$D8,$4B,$74 ;Input: A=$64 Y=$D8 P=$76 M=$4B  Output: A=$64 X=$4B Y=$D8 P=$74 M=$4B
        dta $08,$D1,$26 ;Input: A=$E9 Y=$D1 P=$70 M=$26  Output: A=$E9 X=$26 Y=$D1 P=$70 M=$26
        dta $AF,$1B ;Input: A=$6E Y=$AF P=$31 M=$1B  Output: A=$6E X=$1B Y=$AF P=$31 M=$1B
        dta $0A,$21,$9B,$B0 ;Input: A=$CB Y=$21 P=$32 M=$9B  Output: A=$CB X=$9B Y=$21 P=$B0 M=$9B
        dta $59,$A4,$F5 ;Input: A=$67 Y=$59 P=$F7 M=$A4  Output: A=$67 X=$A4 Y=$59 P=$F5 M=$A4

        ; $B1  LDA (zp),y
        dta $FF
        dta $D0,$B1,$C2,$00,$38 ;Input: A=$2C Y=$00 P=$75 M=$38  Output: A=$38 X=$FF Y=$00 P=$75 M=$38
        dta $10,$00,$CF ;Input: A=$66 Y=$00 P=$F1 M=$CF  Output: A=$CF X=$FF Y=$00 P=$F1 M=$CF
        dta $50,$C4,$01,$9B ;Input: A=$E2 Y=$01 P=$B0 M=$9B  Output: A=$9B X=$FE Y=$01 P=$B0 M=$9B
        dta $52,$C2,$00,$33,$70 ;Input: A=$32 Y=$00 P=$F0 M=$33  Output: A=$33 X=$FF Y=$00 P=$70 M=$33
        dta $12,$00,$BA,$F4 ;Input: A=$92 Y=$00 P=$76 M=$BA  Output: A=$BA X=$FF Y=$00 P=$F4 M=$BA
        dta $52,$C4,$01,$40,$31 ;Input: A=$D3 Y=$01 P=$B1 M=$40  Output: A=$40 X=$FE Y=$01 P=$31 M=$40
        dta $12,$01,$36,$71 ;Input: A=$BD Y=$01 P=$F3 M=$36  Output: A=$36 X=$FE Y=$01 P=$71 M=$36
        dta $52,$C2,$00,$C5,$F1 ;Input: A=$9F Y=$00 P=$F3 M=$C5  Output: A=$C5 X=$FF Y=$00 P=$F1 M=$C5

        ; $B4  LDY zp,x
        dta $D8
        dta $C6,$B4,$76,$A8,$31,$31 ;Input: A=$B9 Y=$A8 P=$B1 M=$31  Output: A=$B9 X=$57 Y=$31 P=$31 M=$31
        dta $46,$8E,$C0,$6C,$30 ;Input: A=$20 Y=$C0 P=$32 M=$6C  Output: A=$20 X=$3F Y=$6C P=$30 M=$6C
        dta $02,$34,$7E,$74 ;Input: A=$FD Y=$34 P=$F4 M=$7E  Output: A=$FD X=$CB Y=$7E P=$74 M=$7E
        dta $44,$EE,$20,$73 ;Input: A=$38 Y=$20 P=$70 M=$73  Output: A=$38 X=$DF Y=$73 P=$70 M=$73
        dta $46,$95,$C7,$A5,$F4 ;Input: A=$F0 Y=$C7 P=$F6 M=$A5  Output: A=$F0 X=$38 Y=$A5 P=$F4 M=$A5
        dta $1D,$4F,$43,$30 ;Input: A=$4E Y=$4F P=$32 M=$43  Output: A=$4E X=$B0 Y=$43 P=$30 M=$43
        dta $52,$84,$56,$30 ;Input: A=$AB Y=$84 P=$B0 M=$56  Output: A=$AB X=$7B Y=$56 P=$30 M=$56
        dta $6E,$A0,$D7,$B0 ;Input: A=$68 Y=$A0 P=$B2 M=$D7  Output: A=$68 X=$5F Y=$D7 P=$B0 M=$D7

        ; $B5  LDA zp,x
        dta $EA
        dta $D2,$B5,$37,$69,$93,$F5 ;Input: A=$32 Y=$69 P=$F7 M=$93  Output: A=$93 X=$96 Y=$69 P=$F5 M=$93
        dta $52,$F0,$22,$68,$74 ;Input: A=$CC Y=$22 P=$F4 M=$68  Output: A=$68 X=$DD Y=$22 P=$74 M=$68
        dta $50,$F2,$24,$B1 ;Input: A=$1C Y=$24 P=$F5 M=$B1  Output: A=$B1 X=$DB Y=$24 P=$F5 M=$B1
        dta $E6,$18,$2C ;Input: A=$3B Y=$18 P=$75 M=$2C  Output: A=$2C X=$E7 Y=$18 P=$75 M=$2C
        dta $52,$F6,$28,$95,$B1 ;Input: A=$C4 Y=$28 P=$33 M=$95  Output: A=$95 X=$D7 Y=$28 P=$B1 M=$95
        dta $77,$A9,$1F,$75 ;Input: A=$BF Y=$A9 P=$F5 M=$1F  Output: A=$1F X=$56 Y=$A9 P=$75 M=$1F
        dta $50,$CF,$01,$7E ;Input: A=$F1 Y=$01 P=$75 M=$7E  Output: A=$7E X=$FE Y=$01 P=$75 M=$7E
        dta $A7,$D9,$E7 ;Input: A=$7E Y=$D9 P=$B1 M=$E7  Output: A=$E7 X=$26 Y=$D9 P=$B1 M=$E7

        ; $B6  LDX zp,y
        dta $E6
        dta $CA,$B6,$E9,$E4,$30,$34 ;Input: A=$E4 Y=$E4 P=$36 M=$30  Output: A=$E4 X=$30 Y=$E4 P=$34 M=$30
        dta $4A,$15,$B8,$8F,$B1 ;Input: A=$84 Y=$B8 P=$B3 M=$8F  Output: A=$84 X=$8F Y=$B8 P=$B1 M=$8F
        dta $48,$42,$8B,$48 ;Input: A=$24 Y=$8B P=$74 M=$48  Output: A=$24 X=$48 Y=$8B P=$74 M=$48
        dta $F4,$D9,$FA ;Input: A=$77 Y=$D9 P=$F5 M=$FA  Output: A=$77 X=$FA Y=$D9 P=$F5 M=$FA
        dta $8D,$40,$42 ;Input: A=$A9 Y=$40 P=$75 M=$42  Output: A=$A9 X=$42 Y=$40 P=$75 M=$42
        dta $4A,$C7,$06,$65,$74 ;Input: A=$70 Y=$06 P=$F6 M=$65  Output: A=$70 X=$65 Y=$06 P=$74 M=$65
        dta $48,$96,$37,$DD ;Input: A=$73 Y=$37 P=$F1 M=$DD  Output: A=$73 X=$DD Y=$37 P=$F1 M=$DD
        dta $9B,$32,$5F ;Input: A=$97 Y=$32 P=$70 M=$5F  Output: A=$97 X=$5F Y=$32 P=$70 M=$5F

        ; $B8  CLV
        dta $D1
        dta $82,$B8,$20,$31 ;Input: A=$5A Y=$20 P=$71 M=$4D  Output: A=$5A X=$DF Y=$20 P=$31 M=$4D
        dta $02,$AF,$33 ;Input: A=$15 Y=$AF P=$73 M=$96  Output: A=$15 X=$50 Y=$AF P=$33 M=$96
        dta $FD,$32 ;Input: A=$61 Y=$FD P=$72 M=$AC  Output: A=$61 X=$02 Y=$FD P=$32 M=$AC
        dta $00,$A2 ;Input: A=$B6 Y=$A2 P=$B4 M=$99  Output: A=$B6 X=$5D Y=$A2 P=$B4 M=$99
        dta $31 ;Input: A=$B0 Y=$31 P=$B0 M=$B0  Output: A=$B0 X=$CE Y=$31 P=$B0 M=$B0
        dta $B3 ;Input: A=$1C Y=$B3 P=$B7 M=$03  Output: A=$1C X=$4C Y=$B3 P=$B7 M=$03
        dta $DA ;Input: A=$D7 Y=$DA P=$B1 M=$71  Output: A=$D7 X=$25 Y=$DA P=$B1 M=$71
        dta $02,$09,$B6 ;Input: A=$31 Y=$09 P=$F6 M=$DA  Output: A=$31 X=$F6 Y=$09 P=$B6 M=$DA

        ; $B9  LDA abs,y
        dta $EA
        dta $F0,$B9,$6C,$00,$61,$95 ;Input: A=$8A Y=$61 P=$B1 M=$95  Output: A=$95 X=$9E Y=$61 P=$B1 M=$95
        dta $50,$58,$75,$96 ;Input: A=$52 Y=$75 P=$F5 M=$96  Output: A=$96 X=$8A Y=$75 P=$F5 M=$96
        dta $52,$7D,$50,$1C,$71 ;Input: A=$DF Y=$50 P=$F1 M=$1C  Output: A=$1C X=$AF Y=$50 P=$71 M=$1C
        dta $60,$6D,$B1,$B4 ;Input: A=$B2 Y=$6D P=$34 M=$B1  Output: A=$B1 X=$92 Y=$6D P=$B4 M=$B1
        dta $50,$6A,$63,$FD ;Input: A=$37 Y=$63 P=$F5 M=$FD  Output: A=$FD X=$9C Y=$63 P=$F5 M=$FD
        dta $90,$3D,$AC ;Input: A=$EC Y=$3D P=$B4 M=$AC  Output: A=$AC X=$C2 Y=$3D P=$B4 M=$AC
        dta $52,$6C,$61,$4E,$75 ;Input: A=$A8 Y=$61 P=$F5 M=$4E  Output: A=$4E X=$9E Y=$61 P=$75 M=$4E
        dta $54,$79,$D8,$F5 ;Input: A=$74 Y=$79 P=$77 M=$D8  Output: A=$D8 X=$86 Y=$79 P=$F5 M=$D8

        ; $BC  LDY abs,x
        dta $EE
        dta $E6,$BC,$7A,$00,$AC,$A5,$F0 ;Input: A=$64 Y=$AC P=$F2 M=$A5  Output: A=$64 X=$53 Y=$A5 P=$F0 M=$A5
        dta $44,$84,$B6,$64 ;Input: A=$B4 Y=$B6 P=$70 M=$64  Output: A=$B4 X=$49 Y=$64 P=$70 M=$64
        dta $46,$65,$97,$09,$30 ;Input: A=$37 Y=$97 P=$32 M=$09  Output: A=$37 X=$68 Y=$09 P=$30 M=$09
        dta $C7,$F9,$6A,$74 ;Input: A=$0E Y=$F9 P=$F6 M=$6A  Output: A=$0E X=$06 Y=$6A P=$74 M=$6A
        dta $06,$F9,$21,$71 ;Input: A=$AF Y=$F9 P=$F1 M=$21  Output: A=$AF X=$06 Y=$21 P=$71 M=$21
        dta $44,$BE,$F0,$6B ;Input: A=$53 Y=$F0 P=$31 M=$6B  Output: A=$53 X=$0F Y=$6B P=$31 M=$6B
        dta $46,$4E,$80,$AF,$F0 ;Input: A=$03 Y=$80 P=$70 M=$AF  Output: A=$03 X=$7F Y=$AF P=$F0 M=$AF
        dta $AE,$E0,$5B,$71 ;Input: A=$BA Y=$E0 P=$73 M=$5B  Output: A=$BA X=$1F Y=$5B P=$71 M=$5B

        ; $BD  LDA abs,x
        dta $E0
        dta $F2,$BD,$95,$00,$C7,$48,$75 ;Input: A=$9B Y=$C7 P=$F5 M=$48  Output: A=$48 X=$38 Y=$C7 P=$75 M=$48
        dta $50,$AD,$DF,$58 ;Input: A=$D6 Y=$DF P=$30 M=$58  Output: A=$58 X=$20 Y=$DF P=$30 M=$58
        dta $52,$78,$AA,$EC,$F1 ;Input: A=$2F Y=$AA P=$71 M=$EC  Output: A=$EC X=$55 Y=$AA P=$F1 M=$EC
        dta $91,$C3,$38,$70 ;Input: A=$6D Y=$C3 P=$F2 M=$38  Output: A=$38 X=$3C Y=$C3 P=$70 M=$38
        dta $A7,$D9,$48,$31 ;Input: A=$FB Y=$D9 P=$B3 M=$48  Output: A=$48 X=$26 Y=$D9 P=$31 M=$48
        dta $A8,$DA,$26,$35 ;Input: A=$EC Y=$DA P=$B5 M=$26  Output: A=$26 X=$25 Y=$DA P=$35 M=$26
        dta $79,$AB,$ED,$F1 ;Input: A=$33 Y=$AB P=$F3 M=$ED  Output: A=$ED X=$54 Y=$AB P=$F1 M=$ED
        dta $AA,$DC,$C1,$B5 ;Input: A=$45 Y=$DC P=$35 M=$C1  Output: A=$C1 X=$23 Y=$DC P=$B5 M=$C1

        ; $BE  LDX abs,y
        dta $F0
        dta $EA,$BE,$98,$00,$35,$EA,$F1 ;Input: A=$0A Y=$35 P=$F3 M=$EA  Output: A=$0A X=$EA Y=$35 P=$F1 M=$EA
        dta $4A,$B0,$1D,$07,$70 ;Input: A=$4F Y=$1D P=$72 M=$07  Output: A=$4F X=$07 Y=$1D P=$70 M=$07
        dta $48,$B1,$1C,$BD ;Input: A=$B0 Y=$1C P=$F1 M=$BD  Output: A=$B0 X=$BD Y=$1C P=$F1 M=$BD
        dta $4A,$C6,$07,$01,$71 ;Input: A=$A6 Y=$07 P=$F1 M=$01  Output: A=$A6 X=$01 Y=$07 P=$71 M=$01
        dta $A6,$27,$83,$F0 ;Input: A=$C5 Y=$27 P=$F2 M=$83  Output: A=$C5 X=$83 Y=$27 P=$F0 M=$83
        dta $85,$48,$24,$30 ;Input: A=$DF Y=$48 P=$B2 M=$24  Output: A=$DF X=$24 Y=$48 P=$30 M=$24
        dta $B5,$18,$5B,$34 ;Input: A=$A2 Y=$18 P=$B6 M=$5B  Output: A=$A2 X=$5B Y=$18 P=$34 M=$5B
        dta $85,$48,$AA,$F5 ;Input: A=$61 Y=$48 P=$75 M=$AA  Output: A=$61 X=$AA Y=$48 P=$F5 M=$AA

        ; $C0  CPY #imm
        dta $C3
        dta $C2,$C0,$6E,$BD,$35 ;Input: A=$4A Y=$BD P=$36 M=$1F  Output: A=$4A X=$42 Y=$BD P=$35 M=$1F
        dta $42,$F1,$DE,$F4 ;Input: A=$BF Y=$DE P=$75 M=$B9  Output: A=$BF X=$21 Y=$DE P=$F4 M=$B9
        dta $A1,$C2,$35 ;Input: A=$5A Y=$C2 P=$B5 M=$A1  Output: A=$5A X=$3D Y=$C2 P=$35 M=$A1
        dta $0E,$C3,$B1 ;Input: A=$C8 Y=$C3 P=$B3 M=$17  Output: A=$C8 X=$3C Y=$C3 P=$B1 M=$17
        dta $B5,$49,$B0 ;Input: A=$3A Y=$49 P=$32 M=$12  Output: A=$3A X=$B6 Y=$49 P=$B0 M=$12
        dta $80,$C7,$31 ;Input: A=$0E Y=$C7 P=$30 M=$D2  Output: A=$0E X=$38 Y=$C7 P=$31 M=$D2
        dta $40,$3E,$BC ;Input: A=$B2 Y=$BC P=$75 M=$72  Output: A=$B2 X=$43 Y=$BC P=$75 M=$72
        dta $42,$0B,$C2,$B1 ;Input: A=$C8 Y=$C2 P=$33 M=$E7  Output: A=$C8 X=$3D Y=$C2 P=$B1 M=$E7

        ; $C1  CMP (zp,x)
        dta $C0
        dta $C2,$C1,$A5,$E2,$71 ;Input: A=$DF Y=$E2 P=$72 M=$85  Output: A=$DF X=$1D Y=$E2 P=$71 M=$85
        dta $42,$91,$CE,$B5 ;Input: A=$DE Y=$CE P=$B6 M=$19  Output: A=$DE X=$31 Y=$CE P=$B5 M=$19
        dta $74,$B1,$75 ;Input: A=$E3 Y=$B1 P=$77 M=$B2  Output: A=$E3 X=$4E Y=$B1 P=$75 M=$B2
        dta $76,$B3,$31 ;Input: A=$95 Y=$B3 P=$33 M=$4D  Output: A=$95 X=$4C Y=$B3 P=$31 M=$4D
        dta $27,$64,$B0 ;Input: A=$A3 Y=$64 P=$31 M=$F3  Output: A=$A3 X=$9B Y=$64 P=$B0 M=$F3
        dta $66,$A3,$B4 ;Input: A=$55 Y=$A3 P=$36 M=$C9  Output: A=$55 X=$5C Y=$A3 P=$B4 M=$C9
        dta $90,$CD,$F4 ;Input: A=$73 Y=$CD P=$75 M=$C7  Output: A=$73 X=$32 Y=$CD P=$F4 M=$C7
        dta $EB,$28,$31 ;Input: A=$13 Y=$28 P=$B0 M=$07  Output: A=$13 X=$D7 Y=$28 P=$31 M=$07

        ; $C4  CPY zp
        dta $CD
        dta $C2,$C4,$CD,$EB,$B5 ;Input: A=$5E Y=$EB P=$B7 M=$4D  Output: A=$5E X=$14 Y=$EB P=$B5 M=$4D
        dta $02,$5A,$B0 ;Input: A=$42 Y=$5A P=$32 M=$77  Output: A=$42 X=$A5 Y=$5A P=$B0 M=$77
        dta $BF,$F4 ;Input: A=$53 Y=$BF P=$77 M=$C6  Output: A=$53 X=$40 Y=$BF P=$F4 M=$C6
        dta $55,$35 ;Input: A=$41 Y=$55 P=$34 M=$49  Output: A=$41 X=$AA Y=$55 P=$35 M=$49
        dta $00,$5C ;Input: A=$33 Y=$5C P=$B0 M=$DB  Output: A=$33 X=$A3 Y=$5C P=$B0 M=$DB
        dta $02,$EA,$35 ;Input: A=$30 Y=$EA P=$37 M=$B3  Output: A=$30 X=$15 Y=$EA P=$35 M=$B3
        dta $3E,$F4 ;Input: A=$4F Y=$3E P=$F6 M=$66  Output: A=$4F X=$C1 Y=$3E P=$F4 M=$66
        dta $00,$95 ;Input: A=$FE Y=$95 P=$31 M=$3F  Output: A=$FE X=$6A Y=$95 P=$31 M=$3F

        ; $C5  CMP zp
        dta $C5
        dta $C0,$C5,$CD,$DC ;Input: A=$FA Y=$DC P=$F5 M=$5D  Output: A=$FA X=$23 Y=$DC P=$F5 M=$5D
        dta $02,$AF,$71 ;Input: A=$AC Y=$AF P=$70 M=$6C  Output: A=$AC X=$50 Y=$AF P=$71 M=$6C
        dta $80,$B5 ;Input: A=$A3 Y=$80 P=$B4 M=$0F  Output: A=$A3 X=$7F Y=$80 P=$B5 M=$0F
        dta $E5,$71 ;Input: A=$7C Y=$E5 P=$F0 M=$50  Output: A=$7C X=$1A Y=$E5 P=$71 M=$50
        dta $57,$F0 ;Input: A=$04 Y=$57 P=$72 M=$2E  Output: A=$04 X=$A8 Y=$57 P=$F0 M=$2E
        dta $00,$E9 ;Input: A=$5D Y=$E9 P=$31 M=$37  Output: A=$5D X=$16 Y=$E9 P=$31 M=$37
        dta $C4 ;Input: A=$AB Y=$C4 P=$35 M=$79  Output: A=$AB X=$3B Y=$C4 P=$35 M=$79
        dta $02,$67,$F4 ;Input: A=$00 Y=$67 P=$74 M=$64  Output: A=$00 X=$98 Y=$67 P=$F4 M=$64

        ; $C6  DEC zp
        dta $CC
        dta $C3,$C6,$CD,$FA,$30,$05 ;Input: A=$66 Y=$FA P=$B0 M=$06  Output: A=$66 X=$05 Y=$FA P=$30 M=$05
        dta $03,$3B,$34,$14 ;Input: A=$A8 Y=$3B P=$B6 M=$15  Output: A=$A8 X=$C4 Y=$3B P=$34 M=$14
        dta $CA,$B5,$8F ;Input: A=$F7 Y=$CA P=$35 M=$90  Output: A=$F7 X=$35 Y=$CA P=$B5 M=$8F
        dta $E1,$74,$4B ;Input: A=$E2 Y=$E1 P=$76 M=$4C  Output: A=$E2 X=$1E Y=$E1 P=$74 M=$4B
        dta $01,$B9,$E9 ;Input: A=$7B Y=$B9 P=$B4 M=$EA  Output: A=$7B X=$46 Y=$B9 P=$B4 M=$E9
        dta $03,$2D,$34,$77 ;Input: A=$84 Y=$2D P=$B4 M=$78  Output: A=$84 X=$D2 Y=$2D P=$34 M=$77
        dta $56,$B4,$BB ;Input: A=$CA Y=$56 P=$B6 M=$BC  Output: A=$CA X=$A9 Y=$56 P=$B4 M=$BB
        dta $3C,$74,$25 ;Input: A=$5B Y=$3C P=$76 M=$26  Output: A=$5B X=$C3 Y=$3C P=$74 M=$25

        ; $C8  INY
        dta $C7
        dta $86,$C8,$CB,$CC,$B0 ;Input: A=$8A Y=$CB P=$30 M=$1C  Output: A=$8A X=$34 Y=$CC P=$B0 M=$1C
        dta $06,$AA,$AB,$B1 ;Input: A=$93 Y=$AA P=$B3 M=$42  Output: A=$93 X=$55 Y=$AB P=$B1 M=$42
        dta $B1,$B2,$B4 ;Input: A=$B1 Y=$B1 P=$36 M=$B2  Output: A=$B1 X=$4E Y=$B2 P=$B4 M=$B2
        dta $05,$06,$70 ;Input: A=$B5 Y=$05 P=$F2 M=$09  Output: A=$B5 X=$FA Y=$06 P=$70 M=$09
        dta $29,$2A,$35 ;Input: A=$63 Y=$29 P=$37 M=$B0  Output: A=$63 X=$D6 Y=$2A P=$35 M=$B0
        dta $04,$82,$83 ;Input: A=$81 Y=$82 P=$B0 M=$A6  Output: A=$81 X=$7D Y=$83 P=$B0 M=$A6
        dta $06,$28,$29,$70 ;Input: A=$D9 Y=$28 P=$72 M=$D5  Output: A=$D9 X=$D7 Y=$29 P=$70 M=$D5
        dta $04,$EF,$F0 ;Input: A=$1A Y=$EF P=$B1 M=$F9  Output: A=$1A X=$10 Y=$F0 P=$B1 M=$F9

        ; $C9  CMP #imm
        dta $C0
        dta $C2,$C9,$42,$12,$F0 ;Input: A=$3E Y=$12 P=$72 M=$F4  Output: A=$3E X=$ED Y=$12 P=$F0 M=$F4
        dta $42,$3F,$E9,$F5 ;Input: A=$C1 Y=$E9 P=$75 M=$75  Output: A=$C1 X=$16 Y=$E9 P=$F5 M=$75
        dta $31,$8B,$F4 ;Input: A=$0D Y=$8B P=$F5 M=$09  Output: A=$0D X=$74 Y=$8B P=$F4 M=$09
        dta $B1,$4F,$30 ;Input: A=$07 Y=$4F P=$B0 M=$9E  Output: A=$07 X=$B0 Y=$4F P=$30 M=$9E
        dta $77,$8A,$35 ;Input: A=$D9 Y=$8A P=$36 M=$67  Output: A=$D9 X=$75 Y=$8A P=$35 M=$67
        dta $90,$F7,$F0 ;Input: A=$2C Y=$F7 P=$71 M=$95  Output: A=$2C X=$08 Y=$F7 P=$F0 M=$95
        dta $04,$AE,$F1 ;Input: A=$97 Y=$AE P=$71 M=$58  Output: A=$97 X=$51 Y=$AE P=$F1 M=$58
        dta $ED,$9F,$74 ;Input: A=$01 Y=$9F P=$F7 M=$BC  Output: A=$01 X=$60 Y=$9F P=$74 M=$BC

        ; $CA  DEX
        dta $C0
        dta $8A,$CA,$D6,$28,$31 ;Input: A=$09 Y=$D6 P=$33 M=$68  Output: A=$09 X=$28 Y=$D6 P=$31 M=$68
        dta $0A,$26,$D8,$F1 ;Input: A=$E6 Y=$26 P=$71 M=$4F  Output: A=$E6 X=$D8 Y=$26 P=$F1 M=$4F
        dta $EC,$12,$71 ;Input: A=$A7 Y=$EC P=$F3 M=$1A  Output: A=$A7 X=$12 Y=$EC P=$71 M=$1A
        dta $FC,$02,$35 ;Input: A=$4F Y=$FC P=$B7 M=$33  Output: A=$4F X=$02 Y=$FC P=$35 M=$33
        dta $01,$FD,$F5 ;Input: A=$7E Y=$01 P=$F7 M=$DB  Output: A=$7E X=$FD Y=$01 P=$F5 M=$DB
        dta $D7,$27,$70 ;Input: A=$BB Y=$D7 P=$F2 M=$CC  Output: A=$BB X=$27 Y=$D7 P=$70 M=$CC
        dta $9F,$5F,$30 ;Input: A=$B5 Y=$9F P=$B2 M=$58  Output: A=$B5 X=$5F Y=$9F P=$30 M=$58
        dta $A7,$57,$75 ;Input: A=$03 Y=$A7 P=$77 M=$6F  Output: A=$03 X=$57 Y=$A7 P=$75 M=$6F

        ; $CC  CPY abs
        dta $CA
        dta $E2,$CC,$CD,$00,$5A,$70 ;Input: A=$85 Y=$5A P=$71 M=$F5  Output: A=$85 X=$A5 Y=$5A P=$70 M=$F5
        dta $02,$CD,$B1 ;Input: A=$2B Y=$CD P=$31 M=$3A  Output: A=$2B X=$32 Y=$CD P=$B1 M=$3A
        dta $A9,$F5 ;Input: A=$F0 Y=$A9 P=$77 M=$1D  Output: A=$F0 X=$56 Y=$A9 P=$F5 M=$1D
        dta $87,$75 ;Input: A=$14 Y=$87 P=$74 M=$71  Output: A=$14 X=$78 Y=$87 P=$75 M=$71
        dta $00,$F4 ;Input: A=$62 Y=$F4 P=$F1 M=$2C  Output: A=$62 X=$0B Y=$F4 P=$F1 M=$2C
        dta $AF ;Input: A=$A0 Y=$AF P=$F5 M=$05  Output: A=$A0 X=$50 Y=$AF P=$F5 M=$05
        dta $02,$EE,$35 ;Input: A=$E7 Y=$EE P=$B5 M=$8A  Output: A=$E7 X=$11 Y=$EE P=$35 M=$8A
        dta $A6,$71 ;Input: A=$96 Y=$A6 P=$72 M=$33  Output: A=$96 X=$59 Y=$A6 P=$71 M=$33

        ; $CD  CMP abs
        dta $C0
        dta $E2,$CD,$CD,$00,$25,$35 ;Input: A=$5F Y=$25 P=$B4 M=$39  Output: A=$5F X=$DA Y=$25 P=$35 M=$39
        dta $02,$8B,$F0 ;Input: A=$3D Y=$8B P=$F1 M=$46  Output: A=$3D X=$74 Y=$8B P=$F0 M=$46
        dta $D8,$75 ;Input: A=$F9 Y=$D8 P=$F4 M=$E7  Output: A=$F9 X=$27 Y=$D8 P=$75 M=$E7
        dta $B0,$F4 ;Input: A=$00 Y=$B0 P=$F5 M=$59  Output: A=$00 X=$4F Y=$B0 P=$F4 M=$59
        dta $08,$F5 ;Input: A=$DC Y=$08 P=$76 M=$56  Output: A=$DC X=$F7 Y=$08 P=$F5 M=$56
        dta $4E,$75 ;Input: A=$86 Y=$4E P=$F5 M=$28  Output: A=$86 X=$B1 Y=$4E P=$75 M=$28
        dta $D1,$F1 ;Input: A=$D6 Y=$D1 P=$72 M=$0C  Output: A=$D6 X=$2E Y=$D1 P=$F1 M=$0C
        dta $77,$F4 ;Input: A=$56 Y=$77 P=$F5 M=$71  Output: A=$56 X=$88 Y=$77 P=$F4 M=$71

        ; $CE  DEC abs
        dta $F0
        dta $E1,$CE,$CD,$00,$FB,$8D ;Input: A=$7B Y=$FB P=$F4 M=$8E  Output: A=$7B X=$04 Y=$FB P=$F4 M=$8D
        dta $03,$4D,$74,$58 ;Input: A=$B6 Y=$4D P=$F4 M=$59  Output: A=$B6 X=$B2 Y=$4D P=$74 M=$58
        dta $01,$CD,$6A ;Input: A=$9A Y=$CD P=$30 M=$6B  Output: A=$9A X=$32 Y=$CD P=$30 M=$6A
        dta $03,$72,$B5,$85 ;Input: A=$92 Y=$72 P=$37 M=$86  Output: A=$92 X=$8D Y=$72 P=$B5 M=$85
        dta $21,$B5,$8B ;Input: A=$9C Y=$21 P=$37 M=$8C  Output: A=$9C X=$DE Y=$21 P=$B5 M=$8B
        dta $BF,$70,$11 ;Input: A=$EC Y=$BF P=$F2 M=$12  Output: A=$EC X=$40 Y=$BF P=$70 M=$11
        dta $BE,$71,$15 ;Input: A=$C4 Y=$BE P=$F1 M=$16  Output: A=$C4 X=$41 Y=$BE P=$71 M=$15
        dta $39,$71,$24 ;Input: A=$8E Y=$39 P=$F1 M=$25  Output: A=$8E X=$C6 Y=$39 P=$71 M=$24

        ; $D1  CMP (zp),y
        dta $C3
        dta $C2,$D1,$C4,$01,$71 ;Input: A=$B7 Y=$01 P=$70 M=$6D  Output: A=$B7 X=$FE Y=$01 P=$71 M=$6D
        dta $42,$C2,$00,$71 ;Input: A=$5E Y=$00 P=$F1 M=$5D  Output: A=$5E X=$FF Y=$00 P=$71 M=$5D
        dta $C4,$01,$F4 ;Input: A=$33 Y=$01 P=$76 M=$5F  Output: A=$33 X=$FE Y=$01 P=$F4 M=$5F
        dta $C2,$00,$B0 ;Input: A=$21 Y=$00 P=$B3 M=$62  Output: A=$21 X=$FF Y=$00 P=$B0 M=$62
        dta $C4,$01,$71 ;Input: A=$CF Y=$01 P=$73 M=$5A  Output: A=$CF X=$FE Y=$01 P=$71 M=$5A
        dta $C2,$00,$B4 ;Input: A=$19 Y=$00 P=$34 M=$32  Output: A=$19 X=$FF Y=$00 P=$B4 M=$32
        dta $02,$00,$75 ;Input: A=$5B Y=$00 P=$77 M=$3D  Output: A=$5B X=$FF Y=$00 P=$75 M=$3D
        dta $42,$C4,$01,$74 ;Input: A=$24 Y=$01 P=$F6 M=$B1  Output: A=$24 X=$FE Y=$01 P=$74 M=$B1

        ; $D5  CMP zp,x
        dta $C0
        dta $C2,$D5,$EB,$1D,$F1 ;Input: A=$EA Y=$1D P=$71 M=$0D  Output: A=$EA X=$E2 Y=$1D P=$F1 M=$0D
        dta $42,$A0,$D2,$75 ;Input: A=$62 Y=$D2 P=$F4 M=$1C  Output: A=$62 X=$2D Y=$D2 P=$75 M=$1C
        dta $6C,$9E,$31 ;Input: A=$C2 Y=$9E P=$33 M=$97  Output: A=$C2 X=$61 Y=$9E P=$31 M=$97
        dta $C4,$F6,$70 ;Input: A=$17 Y=$F6 P=$F2 M=$99  Output: A=$17 X=$09 Y=$F6 P=$70 M=$99
        dta $07,$39,$34 ;Input: A=$03 Y=$39 P=$B6 M=$D0  Output: A=$03 X=$C6 Y=$39 P=$34 M=$D0
        dta $5C,$8E,$35 ;Input: A=$76 Y=$8E P=$B7 M=$4E  Output: A=$76 X=$71 Y=$8E P=$35 M=$4E
        dta $92,$C4,$F0 ;Input: A=$52 Y=$C4 P=$70 M=$A9  Output: A=$52 X=$3B Y=$C4 P=$F0 M=$A9
        dta $0E,$40,$74 ;Input: A=$30 Y=$40 P=$77 M=$D1  Output: A=$30 X=$BF Y=$40 P=$74 M=$D1

        ; $D6  DEC zp,x
        dta $CC
        dta $C3,$D6,$00,$32,$B4,$E4 ;Input: A=$20 Y=$32 P=$36 M=$E5  Output: A=$20 X=$CD Y=$32 P=$B4 M=$E4
        dta $43,$7D,$AF,$34,$12 ;Input: A=$27 Y=$AF P=$B4 M=$13  Output: A=$27 X=$50 Y=$AF P=$34 M=$12
        dta $32,$64,$34,$05 ;Input: A=$CC Y=$64 P=$36 M=$06  Output: A=$CC X=$9B Y=$64 P=$34 M=$05
        dta $C0,$F2,$F1,$92 ;Input: A=$BF Y=$F2 P=$71 M=$93  Output: A=$BF X=$0D Y=$F2 P=$F1 M=$92
        dta $41,$0F,$41,$D7 ;Input: A=$45 Y=$41 P=$B5 M=$D8  Output: A=$45 X=$BE Y=$41 P=$B5 M=$D7
        dta $43,$14,$46,$71,$5E ;Input: A=$6A Y=$46 P=$F3 M=$5F  Output: A=$6A X=$B9 Y=$46 P=$71 M=$5E
        dta $D7,$09,$B1,$F8 ;Input: A=$10 Y=$09 P=$33 M=$F9  Output: A=$10 X=$F6 Y=$09 P=$B1 M=$F8
        dta $CD,$FF,$74,$5E ;Input: A=$29 Y=$FF P=$F6 M=$5F  Output: A=$29 X=$00 Y=$FF P=$74 M=$5E

        ; $D8  CLD
        dta $C0
        dta $80,$D8,$E6 ;Input: A=$73 Y=$E6 P=$F7 M=$D5  Output: A=$73 X=$19 Y=$E6 P=$F7 M=$D5
        dta $00,$C5 ;Input: A=$A3 Y=$C5 P=$34 M=$4A  Output: A=$A3 X=$3A Y=$C5 P=$34 M=$4A
        dta $C5 ;Input: A=$E8 Y=$C5 P=$30 M=$0F  Output: A=$E8 X=$3A Y=$C5 P=$30 M=$0F
        dta $FC ;Input: A=$29 Y=$FC P=$F0 M=$E2  Output: A=$29 X=$03 Y=$FC P=$F0 M=$E2
        dta $63 ;Input: A=$BA Y=$63 P=$F3 M=$2A  Output: A=$BA X=$9C Y=$63 P=$F3 M=$2A
        dta $84 ;Input: A=$C4 Y=$84 P=$F5 M=$8D  Output: A=$C4 X=$7B Y=$84 P=$F5 M=$8D
        dta $AB ;Input: A=$B8 Y=$AB P=$71 M=$86  Output: A=$B8 X=$54 Y=$AB P=$71 M=$86
        dta $A3 ;Input: A=$3D Y=$A3 P=$F3 M=$F2  Output: A=$3D X=$5C Y=$A3 P=$F3 M=$F2

        ; $D9  CMP abs,y
        dta $E0
        dta $E2,$D9,$8E,$00,$3F,$71 ;Input: A=$6A Y=$3F P=$70 M=$5D  Output: A=$6A X=$C0 Y=$3F P=$71 M=$5D
        dta $02,$3F,$70 ;Input: A=$30 Y=$3F P=$71 M=$E9  Output: A=$30 X=$C0 Y=$3F P=$70 M=$E9
        dta $42,$78,$55,$B1 ;Input: A=$EB Y=$55 P=$32 M=$4E  Output: A=$EB X=$AA Y=$55 P=$B1 M=$4E
        dta $7C,$51,$75 ;Input: A=$6B Y=$51 P=$F7 M=$10  Output: A=$6B X=$AE Y=$51 P=$75 M=$10
        dta $89,$44,$31 ;Input: A=$E6 Y=$44 P=$32 M=$9E  Output: A=$E6 X=$BB Y=$44 P=$31 M=$9E
        dta $B7,$16,$B0 ;Input: A=$C0 Y=$16 P=$31 M=$D2  Output: A=$C0 X=$E9 Y=$16 P=$B0 M=$D2
        dta $6F,$5E,$B4 ;Input: A=$3A Y=$5E P=$34 M=$51  Output: A=$3A X=$A1 Y=$5E P=$B4 M=$51
        dta $C5,$08,$75 ;Input: A=$F8 Y=$08 P=$74 M=$B7  Output: A=$F8 X=$F7 Y=$08 P=$75 M=$B7

        ; $DD  CMP abs,x
        dta $CC
        dta $E2,$DD,$4F,$00,$81,$75 ;Input: A=$54 Y=$81 P=$74 M=$2C  Output: A=$54 X=$7E Y=$81 P=$75 M=$2C
        dta $42,$5A,$8C,$B0 ;Input: A=$61 Y=$8C P=$30 M=$9E  Output: A=$61 X=$73 Y=$8C P=$B0 M=$9E
        dta $B7,$E9,$30 ;Input: A=$21 Y=$E9 P=$B0 M=$E6  Output: A=$21 X=$16 Y=$E9 P=$30 M=$E6
        dta $BB,$ED,$B0 ;Input: A=$3C Y=$ED P=$31 M=$7D  Output: A=$3C X=$12 Y=$ED P=$B0 M=$7D
        dta $40,$96,$C8 ;Input: A=$55 Y=$C8 P=$35 M=$51  Output: A=$55 X=$37 Y=$C8 P=$35 M=$51
        dta $42,$57,$89,$75 ;Input: A=$DD Y=$89 P=$F7 M=$8A  Output: A=$DD X=$76 Y=$89 P=$75 M=$8A
        dta $67,$99,$71 ;Input: A=$CA Y=$99 P=$F2 M=$56  Output: A=$CA X=$66 Y=$99 P=$71 M=$56
        dta $98,$CA,$B4 ;Input: A=$97 Y=$CA P=$36 M=$A6  Output: A=$97 X=$35 Y=$CA P=$B4 M=$A6

        ; $DE  DEC abs,x
        dta $E3
        dta $E1,$DE,$7F,$00,$B1,$52 ;Input: A=$F0 Y=$B1 P=$35 M=$53  Output: A=$F0 X=$4E Y=$B1 P=$35 M=$52
        dta $41,$A6,$D8,$05 ;Input: A=$AC Y=$D8 P=$70 M=$06  Output: A=$AC X=$27 Y=$D8 P=$70 M=$05
        dta $43,$65,$97,$F4,$D0 ;Input: A=$AE Y=$97 P=$74 M=$D1  Output: A=$AE X=$68 Y=$97 P=$F4 M=$D0
        dta $5B,$8D,$F4,$D9 ;Input: A=$63 Y=$8D P=$F6 M=$DA  Output: A=$63 X=$72 Y=$8D P=$F4 M=$D9
        dta $63,$95,$B4,$B6 ;Input: A=$6D Y=$95 P=$36 M=$B7  Output: A=$6D X=$6A Y=$95 P=$B4 M=$B6
        dta $78,$AA,$71,$42 ;Input: A=$C0 Y=$AA P=$73 M=$43  Output: A=$C0 X=$55 Y=$AA P=$71 M=$42
        dta $41,$5D,$8F,$23 ;Input: A=$19 Y=$8F P=$30 M=$24  Output: A=$19 X=$70 Y=$8F P=$30 M=$23
        dta $43,$96,$C8,$F5,$C5 ;Input: A=$44 Y=$C8 P=$77 M=$C6  Output: A=$44 X=$37 Y=$C8 P=$F5 M=$C5

        ; $E0  CPX #imm
        dta $C3
        dta $C2,$E0,$A3,$14,$35 ;Input: A=$B5 Y=$14 P=$36 M=$49  Output: A=$B5 X=$EB Y=$14 P=$35 M=$49
        dta $42,$E8,$91,$B4 ;Input: A=$0B Y=$91 P=$B7 M=$CD  Output: A=$0B X=$6E Y=$91 P=$B4 M=$CD
        dta $50,$B3,$B0 ;Input: A=$A0 Y=$B3 P=$33 M=$69  Output: A=$A0 X=$4C Y=$B3 P=$B0 M=$69
        dta $60,$2F,$31 ;Input: A=$3C Y=$2F P=$B1 M=$17  Output: A=$3C X=$D0 Y=$2F P=$31 M=$17
        dta $2B,$90,$75 ;Input: A=$FA Y=$90 P=$F5 M=$2C  Output: A=$FA X=$6F Y=$90 P=$75 M=$2C
        dta $E3,$85,$F0 ;Input: A=$8E Y=$85 P=$70 M=$0A  Output: A=$8E X=$7A Y=$85 P=$F0 M=$0A
        dta $02,$B7,$34 ;Input: A=$83 Y=$B7 P=$B6 M=$DB  Output: A=$83 X=$48 Y=$B7 P=$34 M=$DB
        dta $42,$20,$1D,$B5 ;Input: A=$C2 Y=$1D P=$B4 M=$88  Output: A=$C2 X=$E2 Y=$1D P=$B5 M=$88

        ; $E1  SBC (zp,x)
        dta $C0
        dta $D2,$E1,$F2,$2F,$24,$31 ;Input: A=$E3 Y=$2F P=$30 M=$BE  Output: A=$24 X=$D0 Y=$2F P=$31 M=$BE
        dta $52,$25,$62,$FD,$B0 ;Input: A=$23 Y=$62 P=$31 M=$26  Output: A=$FD X=$9D Y=$62 P=$B0 M=$26
        dta $96,$D3,$21,$75 ;Input: A=$98 Y=$D3 P=$B5 M=$77  Output: A=$21 X=$2C Y=$D3 P=$75 M=$77
        dta $D3,$10,$34,$31 ;Input: A=$7F Y=$10 P=$B3 M=$4B  Output: A=$34 X=$EF Y=$10 P=$31 M=$4B
        dta $20,$5D,$C3,$B0 ;Input: A=$A9 Y=$5D P=$33 M=$E6  Output: A=$C3 X=$A2 Y=$5D P=$B0 M=$E6
        dta $AB,$E8,$10,$31 ;Input: A=$EE Y=$E8 P=$72 M=$DD  Output: A=$10 X=$17 Y=$E8 P=$31 M=$DD
        dta $87,$C4,$4E,$30 ;Input: A=$39 Y=$C4 P=$73 M=$EB  Output: A=$4E X=$3B Y=$C4 P=$30 M=$EB
        dta $6B,$A8,$5C,$34 ;Input: A=$3E Y=$A8 P=$75 M=$E2  Output: A=$5C X=$57 Y=$A8 P=$34 M=$E2

        ; $E4  CPX zp
        dta $C6
        dta $C2,$E4,$CD,$C8,$F4 ;Input: A=$D7 Y=$C8 P=$F7 M=$93  Output: A=$D7 X=$37 Y=$C8 P=$F4 M=$93
        dta $02,$3A,$F1 ;Input: A=$EA Y=$3A P=$F2 M=$40  Output: A=$EA X=$C5 Y=$3A P=$F1 M=$40
        dta $CA,$B4 ;Input: A=$48 Y=$CA P=$36 M=$58  Output: A=$48 X=$35 Y=$CA P=$B4 M=$58
        dta $2C,$35 ;Input: A=$E1 Y=$2C P=$B7 M=$C9  Output: A=$E1 X=$D3 Y=$2C P=$35 M=$C9
        dta $E2,$F0 ;Input: A=$49 Y=$E2 P=$71 M=$4D  Output: A=$49 X=$1D Y=$E2 P=$F0 M=$4D
        dta $00,$60 ;Input: A=$0F Y=$60 P=$F4 M=$A0  Output: A=$0F X=$9F Y=$60 P=$F4 M=$A0
        dta $02,$2C,$B5 ;Input: A=$BD Y=$2C P=$B4 M=$3D  Output: A=$BD X=$D3 Y=$2C P=$B5 M=$3D
        dta $05,$F1 ;Input: A=$45 Y=$05 P=$71 M=$40  Output: A=$45 X=$FA Y=$05 P=$F1 M=$40

        ; $E5  SBC zp
        dta $EB
        dta $D2,$E5,$CD,$62,$12,$31 ;Input: A=$EC Y=$62 P=$B3 M=$DA  Output: A=$12 X=$9D Y=$62 P=$31 M=$DA
        dta $12,$C5,$65,$34 ;Input: A=$39 Y=$C5 P=$F7 M=$D4  Output: A=$65 X=$3A Y=$C5 P=$34 M=$D4
        dta $10,$B0,$26 ;Input: A=$38 Y=$B0 P=$35 M=$12  Output: A=$26 X=$4F Y=$B0 P=$35 M=$12
        dta $40,$9E ;Input: A=$73 Y=$40 P=$F0 M=$D4  Output: A=$9E X=$BF Y=$40 P=$F0 M=$D4
        dta $12,$C3,$FB,$B5 ;Input: A=$FC Y=$C3 P=$F6 M=$00  Output: A=$FB X=$3C Y=$C3 P=$B5 M=$00
        dta $BA,$00,$37 ;Input: A=$EB Y=$BA P=$B5 M=$EB  Output: A=$00 X=$45 Y=$BA P=$37 M=$EB
        dta $10,$24,$9D ;Input: A=$BA Y=$24 P=$B5 M=$1D  Output: A=$9D X=$DB Y=$24 P=$B5 M=$1D
        dta $12,$10,$2C,$31 ;Input: A=$CF Y=$10 P=$F1 M=$A3  Output: A=$2C X=$EF Y=$10 P=$31 M=$A3

        ; $E6  INC zp
        dta $D8
        dta $C1,$E6,$CD,$C4,$D0 ;Input: A=$78 Y=$C4 P=$B0 M=$CF  Output: A=$78 X=$3B Y=$C4 P=$B0 M=$D0
        dta $03,$F1,$B5,$E4 ;Input: A=$98 Y=$F1 P=$B7 M=$E3  Output: A=$98 X=$0E Y=$F1 P=$B5 M=$E4
        dta $D8,$35,$3C ;Input: A=$AE Y=$D8 P=$37 M=$3B  Output: A=$AE X=$27 Y=$D8 P=$35 M=$3C
        dta $01,$4A,$57 ;Input: A=$BD Y=$4A P=$74 M=$56  Output: A=$BD X=$B5 Y=$4A P=$74 M=$57
        dta $03,$E6,$71,$09 ;Input: A=$FB Y=$E6 P=$F1 M=$08  Output: A=$FB X=$19 Y=$E6 P=$71 M=$09
        dta $02,$B1,$CA ;Input: A=$98 Y=$02 P=$31 M=$C9  Output: A=$98 X=$FD Y=$02 P=$B1 M=$CA
        dta $20,$F1,$CC ;Input: A=$5A Y=$20 P=$73 M=$CB  Output: A=$5A X=$DF Y=$20 P=$F1 M=$CC
        dta $E8,$75,$37 ;Input: A=$A5 Y=$E8 P=$77 M=$36  Output: A=$A5 X=$17 Y=$E8 P=$75 M=$37

        ; $E8  INX
        dta $C3
        dta $8A,$E8,$F2,$0E,$35 ;Input: A=$65 Y=$F2 P=$B5 M=$77  Output: A=$65 X=$0E Y=$F2 P=$35 M=$77
        dta $0A,$EA,$16,$31 ;Input: A=$25 Y=$EA P=$B3 M=$63  Output: A=$25 X=$16 Y=$EA P=$31 M=$63
        dta $D7,$29,$74 ;Input: A=$F5 Y=$D7 P=$76 M=$16  Output: A=$F5 X=$29 Y=$D7 P=$74 M=$16
        dta $87,$79,$74 ;Input: A=$04 Y=$87 P=$F6 M=$A2  Output: A=$04 X=$79 Y=$87 P=$74 M=$A2
        dta $70,$90,$F1 ;Input: A=$0B Y=$70 P=$71 M=$A7  Output: A=$0B X=$90 Y=$70 P=$F1 M=$A7
        dta $87,$79,$75 ;Input: A=$73 Y=$87 P=$F7 M=$42  Output: A=$73 X=$79 Y=$87 P=$75 M=$42
        dta $08,$D7,$29 ;Input: A=$67 Y=$D7 P=$34 M=$F2  Output: A=$67 X=$29 Y=$D7 P=$34 M=$F2
        dta $0A,$E9,$17,$75 ;Input: A=$C1 Y=$E9 P=$77 M=$A3  Output: A=$C1 X=$17 Y=$E9 P=$75 M=$A3

        ; $E9  SBC #imm
        dta $F0
        dta $D2,$E9,$E1,$31,$29,$30 ;Input: A=$0A Y=$31 P=$F1 M=$49  Output: A=$29 X=$CE Y=$31 P=$30 M=$49
        dta $52,$73,$1D,$C0,$B4 ;Input: A=$34 Y=$1D P=$F6 M=$05  Output: A=$C0 X=$E2 Y=$1D P=$B4 M=$05
        dta $50,$8D,$82,$55 ;Input: A=$E2 Y=$82 P=$35 M=$A1  Output: A=$55 X=$7D Y=$82 P=$35 M=$A1
        dta $52,$9C,$73,$2B,$35 ;Input: A=$C8 Y=$73 P=$F6 M=$9C  Output: A=$2B X=$8C Y=$73 P=$35 M=$9C
        dta $CE,$C6,$6B,$34 ;Input: A=$3A Y=$C6 P=$B4 M=$A9  Output: A=$6B X=$39 Y=$C6 P=$34 M=$A9
        dta $57,$D4,$4C,$75 ;Input: A=$A4 Y=$D4 P=$34 M=$1A  Output: A=$4C X=$2B Y=$D4 P=$75 M=$1A
        dta $AC,$3D,$AB,$F4 ;Input: A=$58 Y=$3D P=$F6 M=$C7  Output: A=$AB X=$C2 Y=$3D P=$F4 M=$C7
        dta $A1,$B0,$2B,$31 ;Input: A=$CC Y=$B0 P=$73 M=$A7  Output: A=$2B X=$4F Y=$B0 P=$31 M=$A7

        ; $EA  NOP
        dta $C0
        dta $80,$EA,$06 ;Input: A=$5C Y=$06 P=$F7 M=$5E  Output: A=$5C X=$F9 Y=$06 P=$F7 M=$5E
        dta $00,$37 ;Input: A=$45 Y=$37 P=$F0 M=$98  Output: A=$45 X=$C8 Y=$37 P=$F0 M=$98
        dta $EF ;Input: A=$96 Y=$EF P=$B5 M=$BC  Output: A=$96 X=$10 Y=$EF P=$B5 M=$BC
        dta $CC ;Input: A=$49 Y=$CC P=$F6 M=$2F  Output: A=$49 X=$33 Y=$CC P=$F6 M=$2F
        dta $8C ;Input: A=$66 Y=$8C P=$B6 M=$F5  Output: A=$66 X=$73 Y=$8C P=$B6 M=$F5
        dta $B6 ;Input: A=$F0 Y=$B6 P=$72 M=$EC  Output: A=$F0 X=$49 Y=$B6 P=$72 M=$EC
        dta $86 ;Input: A=$EA Y=$86 P=$72 M=$0D  Output: A=$EA X=$79 Y=$86 P=$72 M=$0D
        dta $19 ;Input: A=$29 Y=$19 P=$B6 M=$29  Output: A=$29 X=$E6 Y=$19 P=$B6 M=$29

        ; $EC  CPX abs
        dta $CC
        dta $E2,$EC,$CD,$00,$36,$B5 ;Input: A=$B8 Y=$36 P=$37 M=$24  Output: A=$B8 X=$C9 Y=$36 P=$B5 M=$24
        dta $02,$25,$B4 ;Input: A=$B3 Y=$25 P=$B7 M=$F6  Output: A=$B3 X=$DA Y=$25 P=$B4 M=$F6
        dta $72,$75 ;Input: A=$47 Y=$72 P=$76 M=$11  Output: A=$47 X=$8D Y=$72 P=$75 M=$11
        dta $8E,$F0 ;Input: A=$9B Y=$8E P=$73 M=$7A  Output: A=$9B X=$71 Y=$8E P=$F0 M=$7A
        dta $00,$A7 ;Input: A=$8F Y=$A7 P=$74 M=$F2  Output: A=$8F X=$58 Y=$A7 P=$74 M=$F2
        dta $02,$3C,$F4 ;Input: A=$87 Y=$3C P=$77 M=$EC  Output: A=$87 X=$C3 Y=$3C P=$F4 M=$EC
        dta $28,$B5 ;Input: A=$1E Y=$28 P=$B7 M=$2B  Output: A=$1E X=$D7 Y=$28 P=$B5 M=$2B
        dta $43,$31 ;Input: A=$11 Y=$43 P=$B3 M=$8B  Output: A=$11 X=$BC Y=$43 P=$31 M=$8B

        ; $ED  SBC abs
        dta $D8
        dta $F2,$ED,$CD,$00,$C6,$8F,$B4 ;Input: A=$07 Y=$C6 P=$37 M=$78  Output: A=$8F X=$39 Y=$C6 P=$B4 M=$78
        dta $12,$15,$43,$35 ;Input: A=$78 Y=$15 P=$34 M=$34  Output: A=$43 X=$EA Y=$15 P=$35 M=$34
        dta $A1,$E2,$F4 ;Input: A=$77 Y=$A1 P=$B7 M=$95  Output: A=$E2 X=$5E Y=$A1 P=$F4 M=$95
        dta $10,$F7,$1E ;Input: A=$CB Y=$F7 P=$31 M=$AD  Output: A=$1E X=$08 Y=$F7 P=$31 M=$AD
        dta $12,$3E,$3C,$35 ;Input: A=$C9 Y=$3E P=$75 M=$8D  Output: A=$3C X=$C1 Y=$3E P=$35 M=$8D
        dta $B1,$42,$35 ;Input: A=$73 Y=$B1 P=$37 M=$31  Output: A=$42 X=$4E Y=$B1 P=$35 M=$31
        dta $D9,$52,$34 ;Input: A=$47 Y=$D9 P=$F4 M=$F4  Output: A=$52 X=$26 Y=$D9 P=$34 M=$F4
        dta $8A,$9E,$F4 ;Input: A=$6F Y=$8A P=$B7 M=$D1  Output: A=$9E X=$75 Y=$8A P=$F4 M=$D1

        ; $EE  INC abs
        dta $E0
        dta $E3,$EE,$CD,$00,$FC,$75,$1E ;Input: A=$69 Y=$FC P=$F7 M=$1D  Output: A=$69 X=$03 Y=$FC P=$75 M=$1E
        dta $01,$73,$C7 ;Input: A=$D1 Y=$73 P=$B0 M=$C6  Output: A=$D1 X=$8C Y=$73 P=$B0 M=$C7
        dta $03,$50,$F5,$B8 ;Input: A=$BC Y=$50 P=$F7 M=$B7  Output: A=$BC X=$AF Y=$50 P=$F5 M=$B8
        dta $83,$B0,$AF ;Input: A=$7D Y=$83 P=$32 M=$AE  Output: A=$7D X=$7C Y=$83 P=$B0 M=$AF
        dta $B4,$75,$67 ;Input: A=$D7 Y=$B4 P=$F7 M=$66  Output: A=$D7 X=$4B Y=$B4 P=$75 M=$67
        dta $A3,$70,$03 ;Input: A=$B4 Y=$A3 P=$72 M=$02  Output: A=$B4 X=$5C Y=$A3 P=$70 M=$03
        dta $8C,$B4,$E0 ;Input: A=$C6 Y=$8C P=$B6 M=$DF  Output: A=$C6 X=$73 Y=$8C P=$B4 M=$E0
        dta $8B,$B1,$B3 ;Input: A=$16 Y=$8B P=$31 M=$B2  Output: A=$16 X=$74 Y=$8B P=$B1 M=$B3

        ; $F1  SBC (zp),y
        dta $CB
        dta $D2,$F1,$C4,$01,$F1,$B4 ;Input: A=$1E Y=$01 P=$75 M=$2D  Output: A=$F1 X=$FE Y=$01 P=$B4 M=$2D
        dta $12,$01,$6A,$34 ;Input: A=$59 Y=$01 P=$B7 M=$EF  Output: A=$6A X=$FE Y=$01 P=$34 M=$EF
        dta $01,$80,$B5 ;Input: A=$B0 Y=$01 P=$74 M=$2F  Output: A=$80 X=$FE Y=$01 P=$B5 M=$2F
        dta $01,$0E,$35 ;Input: A=$76 Y=$01 P=$77 M=$68  Output: A=$0E X=$FE Y=$01 P=$35 M=$68
        dta $52,$C2,$00,$5C,$71 ;Input: A=$90 Y=$00 P=$70 M=$33  Output: A=$5C X=$FF Y=$00 P=$71 M=$33
        dta $C4,$01,$EE,$B4 ;Input: A=$13 Y=$01 P=$77 M=$25  Output: A=$EE X=$FE Y=$01 P=$B4 M=$25
        dta $12,$01,$B4,$B4 ;Input: A=$95 Y=$01 P=$B5 M=$E1  Output: A=$B4 X=$FE Y=$01 P=$B4 M=$E1
        dta $52,$C2,$00,$71,$34 ;Input: A=$12 Y=$00 P=$F5 M=$A1  Output: A=$71 X=$FF Y=$00 P=$34 M=$A1

        ; $F5  SBC zp,x
        dta $C3
        dta $D2,$F5,$78,$AA,$E4,$B0 ;Input: A=$37 Y=$AA P=$30 M=$52  Output: A=$E4 X=$55 Y=$AA P=$B0 M=$52
        dta $52,$3C,$6E,$9D,$B1 ;Input: A=$F8 Y=$6E P=$B2 M=$5A  Output: A=$9D X=$91 Y=$6E P=$B1 M=$5A
        dta $91,$C3,$77,$30 ;Input: A=$57 Y=$C3 P=$B0 M=$DF  Output: A=$77 X=$3C Y=$C3 P=$30 M=$DF
        dta $08,$3A,$E8,$F0 ;Input: A=$7F Y=$3A P=$F3 M=$97  Output: A=$E8 X=$C5 Y=$3A P=$F0 M=$97
        dta $E0,$12,$D1,$B0 ;Input: A=$28 Y=$12 P=$73 M=$57  Output: A=$D1 X=$ED Y=$12 P=$B0 M=$57
        dta $40,$72,$D6,$B0 ;Input: A=$41 Y=$72 P=$70 M=$6A  Output: A=$D6 X=$8D Y=$72 P=$B0 M=$6A
        dta $50,$DD,$0F,$D1 ;Input: A=$85 Y=$0F P=$B0 M=$B3  Output: A=$D1 X=$F0 Y=$0F P=$B0 M=$B3
        dta $52,$3C,$6E,$66,$34 ;Input: A=$2F Y=$6E P=$B7 M=$C9  Output: A=$66 X=$91 Y=$6E P=$34 M=$C9

        ; $F6  INC zp,x
        dta $E8
        dta $C3,$F6,$BB,$ED,$34,$7C ;Input: A=$BE Y=$ED P=$36 M=$7B  Output: A=$BE X=$12 Y=$ED P=$34 M=$7C
        dta $43,$F3,$25,$B4,$E2 ;Input: A=$FD Y=$25 P=$B6 M=$E1  Output: A=$FD X=$DA Y=$25 P=$B4 M=$E2
        dta $41,$40,$72,$8B ;Input: A=$2A Y=$72 P=$B4 M=$8A  Output: A=$2A X=$8D Y=$72 P=$B4 M=$8B
        dta $6C,$9E,$0F ;Input: A=$A7 Y=$9E P=$70 M=$0E  Output: A=$A7 X=$61 Y=$9E P=$70 M=$0F
        dta $43,$D3,$05,$F5,$EC ;Input: A=$B9 Y=$05 P=$75 M=$EB  Output: A=$B9 X=$FA Y=$05 P=$F5 M=$EC
        dta $76,$A8,$B0,$F2 ;Input: A=$DB Y=$A8 P=$32 M=$F1  Output: A=$DB X=$57 Y=$A8 P=$B0 M=$F2
        dta $D7,$09,$31,$7B ;Input: A=$C9 Y=$09 P=$33 M=$7A  Output: A=$C9 X=$F6 Y=$09 P=$31 M=$7B
        dta $99,$CB,$B1,$A4 ;Input: A=$39 Y=$CB P=$B3 M=$A3  Output: A=$39 X=$34 Y=$CB P=$B1 M=$A4

        ; $F8  SED
        dta $C0
        dta $82,$F8,$69,$BD ;Input: A=$B3 Y=$69 P=$B5 M=$BE  Output: A=$B3 X=$96 Y=$69 P=$BD M=$BE
        dta $02,$7E,$BA ;Input: A=$E2 Y=$7E P=$B2 M=$BD  Output: A=$E2 X=$81 Y=$7E P=$BA M=$BD
        dta $74,$3C ;Input: A=$5E Y=$74 P=$34 M=$7B  Output: A=$5E X=$8B Y=$74 P=$3C M=$7B
        dta $9A,$F8 ;Input: A=$D5 Y=$9A P=$F0 M=$E3  Output: A=$D5 X=$65 Y=$9A P=$F8 M=$E3
        dta $EC,$FA ;Input: A=$DA Y=$EC P=$F2 M=$44  Output: A=$DA X=$13 Y=$EC P=$FA M=$44
        dta $0F,$FA ;Input: A=$85 Y=$0F P=$F2 M=$68  Output: A=$85 X=$F0 Y=$0F P=$FA M=$68
        dta $59,$7B ;Input: A=$0A Y=$59 P=$73 M=$A8  Output: A=$0A X=$A6 Y=$59 P=$7B M=$A8
        dta $D6,$BA ;Input: A=$CF Y=$D6 P=$B2 M=$80  Output: A=$CF X=$29 Y=$D6 P=$BA M=$80

        ; $F9  SBC abs,y
        dta $CC
        dta $F2,$F9,$AA,$00,$23,$45,$31 ;Input: A=$E3 Y=$23 P=$F2 M=$9D  Output: A=$45 X=$DC Y=$23 P=$31 M=$9D
        dta $52,$A6,$27,$3F,$34 ;Input: A=$29 Y=$27 P=$B5 M=$EA  Output: A=$3F X=$D8 Y=$27 P=$34 M=$EA
        dta $6D,$60,$A8,$B4 ;Input: A=$0F Y=$60 P=$F5 M=$67  Output: A=$A8 X=$9F Y=$60 P=$B4 M=$67
        dta $62,$6B,$B5,$B0 ;Input: A=$30 Y=$6B P=$B2 M=$7A  Output: A=$B5 X=$94 Y=$6B P=$B0 M=$7A
        dta $12,$6B,$A1,$B1 ;Input: A=$DF Y=$6B P=$B0 M=$3D  Output: A=$A1 X=$94 Y=$6B P=$B1 M=$3D
        dta $52,$59,$74,$15,$31 ;Input: A=$67 Y=$74 P=$72 M=$51  Output: A=$15 X=$8B Y=$74 P=$31 M=$51
        dta $97,$36,$A1,$B0 ;Input: A=$A0 Y=$36 P=$73 M=$FF  Output: A=$A1 X=$C9 Y=$36 P=$B0 M=$FF
        dta $61,$6C,$F4,$B0 ;Input: A=$84 Y=$6C P=$F1 M=$90  Output: A=$F4 X=$93 Y=$6C P=$B0 M=$90

        ; $FD  SBC abs,x
        dta $F0
        dta $F2,$FD,$AF,$00,$E1,$46,$31 ;Input: A=$DA Y=$E1 P=$70 M=$93  Output: A=$46 X=$1E Y=$E1 P=$31 M=$93
        dta $52,$59,$8B,$17,$35 ;Input: A=$EB Y=$8B P=$B6 M=$D3  Output: A=$17 X=$74 Y=$8B P=$35 M=$D3
        dta $12,$8B,$62,$34 ;Input: A=$0E Y=$8B P=$B7 M=$AC  Output: A=$62 X=$74 Y=$8B P=$34 M=$AC
        dta $52,$5B,$8D,$D0,$B4 ;Input: A=$41 Y=$8D P=$77 M=$71  Output: A=$D0 X=$72 Y=$8D P=$B4 M=$71
        dta $B6,$E8,$3A,$31 ;Input: A=$42 Y=$E8 P=$B2 M=$07  Output: A=$3A X=$17 Y=$E8 P=$31 M=$07
        dta $B2,$E4,$F9,$B4 ;Input: A=$60 Y=$E4 P=$B7 M=$67  Output: A=$F9 X=$1B Y=$E4 P=$B4 M=$67
        dta $61,$93,$45,$31 ;Input: A=$46 Y=$93 P=$33 M=$01  Output: A=$45 X=$6C Y=$93 P=$31 M=$01
        dta $78,$AA,$F7,$B4 ;Input: A=$6B Y=$AA P=$34 M=$73  Output: A=$F7 X=$55 Y=$AA P=$B4 M=$73

        ; $FE  INC abs,x
        dta $C6
        dta $E3,$FE,$74,$00,$A6,$F0,$E6 ;Input: A=$BB Y=$A6 P=$70 M=$E5  Output: A=$BB X=$59 Y=$A6 P=$F0 M=$E6
        dta $43,$A7,$D9,$74,$52 ;Input: A=$34 Y=$D9 P=$76 M=$51  Output: A=$34 X=$26 Y=$D9 P=$74 M=$52
        dta $57,$89,$B5,$F1 ;Input: A=$66 Y=$89 P=$35 M=$F0  Output: A=$66 X=$76 Y=$89 P=$B5 M=$F1
        dta $8C,$BE,$70,$07 ;Input: A=$90 Y=$BE P=$F0 M=$06  Output: A=$90 X=$41 Y=$BE P=$70 M=$07
        dta $55,$87,$75,$73 ;Input: A=$AE Y=$87 P=$77 M=$72  Output: A=$AE X=$78 Y=$87 P=$75 M=$73
        dta $41,$79,$AB,$F0 ;Input: A=$63 Y=$AB P=$B4 M=$EF  Output: A=$63 X=$54 Y=$AB P=$B4 M=$F0
        dta $43,$75,$A7,$70,$39 ;Input: A=$E5 Y=$A7 P=$F2 M=$38  Output: A=$E5 X=$58 Y=$A7 P=$70 M=$39
        dta $BB,$ED,$70,$61 ;Input: A=$C8 Y=$ED P=$F2 M=$60  Output: A=$C8 X=$12 Y=$ED P=$70 M=$61
test_end:

.ifndef ATARI
    org $fffa
    dta a(0),a(main),a(0)
.else
    run main
.endif
