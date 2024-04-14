;==================================================================
; (C) 2002  Bird Computer
; All rights reserved.
;
; 6502 Processor Test Routine
;
;   The basic format is to test the things needed for further
; testing first, like branches and compare, then move onto other
; instructions.
;==================================================================

; Modified for Mad-Assembler by Ivo van Poorten
; On error, it hangs with jmp *. Use debugger to see what failed.

; mads -l -o:bird6502.bin bird6502.asm
;
; or
;
; mads -d:ATARI=1 -o:bird6502.xex bird6502.asm

.ifndef ATARI
    opt h-
    opt f+

    org $0

    .byte 0
.endif

data_ptr        equ     $08

    org $8000

run:

; If the program gets here then we know at least the boot strap
; worked okay, which is in itself a good test of processor health.

    nop

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; First thing to test is branches. If you can't branch reliably
; then the validity of the remaining tests are in question.
; Test branches and also simultaneously some other simple
; instructions.
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    sei

    sec
    bcs     bcsok

    jmp *
    .byte "BCS:FAILED"

bcsok
    clc
    bcc     bccok

    jmp *
    .byte "BCC:FAILED"

bccok
    lda     #$00
    beq     beqok

    jmp *
    .byte "BEQ:FAILED"
beqok
    lda     #$80
    bne     bneok

    jmp *
    .byte "BNE:FAILED"
bneok
    ora     #$00
    bmi     bmiok

    jmp *
    .byte "BMI:FAILED"
bmiok
    eor     #$80
    bpl     bplok

    jmp *
    .byte "BPL:FAILED"
bplok
    lda     #$7f
    clc
    adc     #$10        ; should give signed overflow
    bvs     bvsok

    jmp *
    .byte "BVS:FAILED"
bvsok
    clv
    bvc     bvcok

    jmp *
    .byte "BVC:FAILED"
bvcok

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Compare Instructions
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    lda     #27         ; bit 7 = 0
    clc
    cmp     #27
    bcc     cmperr
    bne     cmperr
    bmi     cmperr
    lda     #$A1
    cmp     #20
    bpl     cmperr      ; should be neg.
    sec
    lda     #10
    cmp     #20         ; should be a borrow here
    bcs     cmperr
    clv
    lda     #$80        ; -128 - 32 = -160 should overflow
    cmp     #$20        ; compare doesn't affect overflow
    bvs     cmperr
    bvc     cmpok

cmperr
    jmp *
    .byte "CMP:FAILED"

cmpok
    ldx     #27
    clc
    cpx     #27
    bcc     cpxerr
    bne     cpxerr
    bmi     cpxerr
    ldx     #$A1
    cpx     #20
    bpl     cpxerr
    ldx     #10
    cpx     #20         ; should be a borrow here
    bcs     cpxerr
    clv
    ldx     #$80        ; -128 - 32 = -160 should overflow
    cpx     #$20        ; but cpx shouldn't change overflow
    bvs     cpxerr      
    bvc     cpxok

cpxerr
    jmp *
    .byte "CPX:FAILED"

cpxok
    ldy     #27
    clc
    cpy     #27
    bcc     cpyerr
    bne     cpyerr
    bmi     cpyerr
    ldy     #$B0
    cpy     #20
    bpl     cpyerr
    ldy     #10
    cpy     #20         ; should be a borrow here
    bcs     cpyerr
    clv
    ldy     #$80        ; -128 - 32 = -160 should overflow
    cpy     #$20        ; but cpy shouldn't change overflow
    bvs     cpyerr      
    bvc     cpyok

cpyerr
    jmp *
    .byte "CPY:FAILED"

cpyok


;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Load
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; lda

    clc
    lda     #0
    bne     ldaerr
    bmi     ldaerr
    bcs     ldaerr
    lda     #$80
    beq     ldaerr
    bpl     ldaerr

    lda     #$00
    sta     $800
    bne     ldaerr
    bmi     ldaerr
    bcs     ldaerr
    
    lda     #$ff
    lda     $800
    bne     ldaerr
    bmi     ldaerr
    bcs     ldaerr
    
    cmp     #0
    bne     ldaerr
    
    sec
    lda     #$ff
    sta     $800
    beq     ldaerr
    bpl     ldaerr
    bcc     ldaerr
    
    lda     #0
    lda     $800
    beq     ldaerr
    bpl     ldaerr
    bcc     ldaerr
    
    cmp     #$ff
    beq     ldaok

ldaerr
    jmp *
    .byte "LDA:FAILED"

ldaok


; ldx

    clc
    lda     #$80        ; z = 0, n = 1
    ldx     #0
    bcs     ldxerr
    bne     ldxerr
    bmi     ldxerr

    stx     $800
    bne     ldxerr
    bmi     ldxerr
    bcs     ldxerr
    
    ldx     #$ff
    ldx     $800
    bne     ldxerr
    bmi     ldxerr
    bcs     ldxerr
    
    cpx     #0
    bne     ldxerr
    
    sec
    ldx     #$ff
    stx     $800
    beq     ldxerr
    bpl     ldxerr
    bcc     ldxerr
    
    ldx     #0
    ldx     $800
    beq     ldxerr
    bpl     ldxerr
    bcc     ldxerr
    
    cpx     #$ff
    beq     ldxok

ldxerr
    jmp *
    .byte "LDX:FAILED"


; ldy

ldxok
    clc
    lda     #$80        ; z = 0, n = 1
    ldy     #0
    bcs     ldyerr
    bne     ldyerr
    bmi     ldyerr

    sty     $800
    bne     ldyerr
    bmi     ldyerr
    bcs     ldyerr
    
    ldy     #$ff
    ldy     $800
    bne     ldyerr
    bmi     ldyerr
    bcs     ldyerr
    
    cpy     #0
    bne     ldyerr
    
    sec
    ldy     #$ff
    sty     $800
    beq     ldyerr
    bpl     ldyerr
    bcc     ldyerr
    
    ldy     #0
    ldy     $800
    beq     ldyerr
    bpl     ldyerr
    bcc     ldyerr
    
    cpy     #$ff
    beq     ldyok

ldyerr
    jmp *
    .byte "LDY:FAILED"

ldyok

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Test register transfers
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; tax

    clc
    lda     #0
    ldx     #$80            ; z = 0, n = 1
    tax
    bcs     taxerr
    bmi     taxerr
    bne     taxerr
    
    txa
    bne     taxerr

    lda     #$ff
    sec
    ldx     #0
    tax
    bcc     taxerr
    bpl     taxerr
    beq     taxerr
    
    txa
    cmp     #$ff
    beq     taxok

taxerr
    jmp *
    .byte "TAX:FAILED"
taxok

; tay

    clc
    lda     #0
    ldy     #$80            ; z = 0, n = 1
    tay
    bcs     tayerr
    bmi     tayerr
    bne     tayerr
    
    tya
    bne     tayerr

    lda     #$ff
    sec
    ldy     #0
    tay
    bcc     tayerr
    bpl     tayerr
    beq     tayerr
    
    tya
    cmp     #$ff
    beq     tayok

tayerr
    jmp *
    .byte "TAY:FAILED"

tayok

; txs

    ldx     #15
    txs
    ldx     #87
    tsx
    cpx     #15
    beq     txsok

    jmp *
    .byte "TSX:FAILED"

txsok
    ldx     #87
    txa
    cmp     #87
    beq     txaok

    jmp *
    .byte "TXA:FAILED"

txaok
    tay
    cpy     #87
    beq     tayok1

    jmp *
    .byte "TAY:FAILED"

tayok1
    tya
    beq     tyaerr
    bmi     tyaerr
    cmp     #87
    beq     tyaok

tyaerr
    jmp *
    .byte "TYA:FAILED"

tyaok

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Increment / Decrement
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    ldx     #$FE
    clc
    lda     #0
    inx
    bcs     inxerr
    beq     inxerr
    bpl     inxerr
    
    cpx     #$ff
    bne     inxerr
    
    sec
    lda     #$80
    inx
    bcc     inxerr
    bne     inxerr
    bmi     inxerr
    
    cpx     #0
    bne     inxerr
    
    clc
inxl1               ; test loop
    inx
    bcs     inxerr
    bne     inxl1
    
    sec
inxl2
    inx
    bcc     inxerr
    bne     inxl2
    
    beq     inxok
    
inxerr
    jmp *
    .byte "INX:FAILED"

inxok

;   dex

    ldx     #2
    clc
    lda     #0
    dex
    bcs     dexerr
    beq     dexerr
    bmi     dexerr
    
    cpx     #1
    bne     dexerr
    
    sec
    lda     #$80
    dex
    bcc     dexerr
    bne     dexerr
    bmi     dexerr
    
    cpx     #0
    bne     dexerr
    
    lda     #0
    dex
    beq     dexerr
    bpl     dexerr
    
    cpx     #$ff
    bne     dexerr
    
    clc
dexl1
    dex
    bcs     dexerr
    bne     dexl1
    
    sec
dexl2
    dex
    bcc     dexerr
    bne     dexl2
    
    beq     dexok
    
dexerr
    jmp *
    .byte "DEX:FAILED"
    
dexok

; iny

    ldy     #$FE
    clc
    adc     #0
    iny
    bcs     inyerr
    beq     inyerr
    bpl     inyerr
    
    cpy     #$ff
    bne     inyerr
    
    sec
    lda     #$80
    iny
    bcc     inyerr
    bne     inyerr
    bmi     inyerr
    
    cpy     #0
    bne     inyerr
    
    clc
inyl1               ; test loop
    iny
    bcs     inyerr
    bne     inyl1
    
    sec
inyl2
    iny
    bcc     inyerr
    bne     inyl2
    
    beq     inyok
    
inyerr
    jmp *
    .byte "INY:FAILED"


;   dey

inyok

    ldy     #2
    clc
    lda     #0
    dey
    bcs     deyerr
    beq     deyerr
    bmi     deyerr
    
    cpy     #1
    bne     deyerr
    
    sec
    lda     #$80
    dey
    bcc     deyerr
    bne     deyerr
    bmi     deyerr
    
    cpy     #0
    bne     deyerr
    
    lda     #0
    dey
    beq     deyerr
    bpl     deyerr
    
    cpy     #$ff
    bne     deyerr
    
    clc
deyl1
    dey
    bcs     deyerr
    bne     deyl1
    
    sec
deyl2
    dey
    bcc     deyerr
    bne     deyl2
    
    beq     deyok
    
deyerr
    jmp *
    .byte "DEY:FAILED"
    
deyok


;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Stores
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; sta

    lda     #0
    clc
    sta     $00
    bne     staerr
    bmi     staerr
    bcs     staerr
    
    lda     $00
    bne     staerr
    
    lda     #$ff
    sec
    sta     $00
    beq     staerr
    bpl     staerr
    bcc     staerr
    bcs     staok

staerr
    jmp *
    .byte "STA:FAILED"
staok

; stx

    ldx     #0
    clc
    stx     $00
    bne     stxerr
    bmi     stxerr
    bcs     stxerr
    
    ldx     $00
    bne     stxerr
    
    ldx     #$ff
    sec
    stx     $00
    beq     stxerr
    bpl     stxerr
    bcc     stxerr
    bcs     stxok

stxerr
    jmp *
    .byte "STX:FAILED"
stxok

; sty

    ldy     #0
    clc
    sty     $00
    bne     styerr
    bmi     styerr
    bcs     styerr
    
    ldy     $00
    bne     styerr
    
    ldy     #$ff
    sec
    sty     $00
    beq     styerr
    bpl     styerr
    bcc     styerr
    bcs     styok

styerr
    jmp *
    .byte "STY:FAILED"
styok

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Test addressing mode
;   Note that addressing modes are handled independently of the
; actual operation performed by the processor. This means that
; if a mode works with one instruction, it should work properly
; with all instructions that use that mode.
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    lda     #$AA
    eor     #$55
    cmp     #$ff
    beq     imm_ok

    jmp *
    .byte "IMM:FAILED"

imm_ok
    lda     n55
    eor     nAA
    clc
    adc     #1
    beq     abs_ok

    jmp *
    .byte "ABS:FAILED"

abs_ok
    ldx     #2
    lda     n55,x
    cmp     #12
    beq     absx_ok

    jmp *
    .byte "ABS,X:FAILED"

absx_ok
    ldy     #3
    lda     n55,y
    cmp     #34
    beq     absy_ok

    jmp *
    .byte "ABS,Y:FAILED"

absy_ok
    lda     #33
    sta     data_ptr
    ldx     #0
    ldx     data_ptr
    cpx     #33
    beq     zp_ok

    jmp *
    .byte "ZP:FAILED"

zp_ok
    lda     #44
    sta     data_ptr+33
    lda     data_ptr,x
    cmp     #44
    beq     zpx_ok

    jmp *
    .byte "ZP,X:FAILED"

zpx_ok
    lda     #$12
    sta     $201
    lda     #$01
    sta     data_ptr
    lda     #$2
    sta     data_ptr+1
    ldx     #5
    lda     (data_ptr-5,x)
    cmp     #$12
    beq     zpix_ok

    jmp *
    .byte "(ZP,X):FAILED"

zpix_ok
    lda     #$fe
    sta     data_ptr
    lda     #$01
    sta     data_ptr+1
    ldy     #3
    lda     (data_ptr),y
    cmp     #$12
    beq     zpiy_ok

    jmp *
    .byte "(ZP),y:FAILED"

zpiy_ok
    ldy     #15
    ldx     data_ptr-15,y
    cpx     #$fe
    beq     zpy_ok

    jmp *
    jmp *
    .byte "ZP,Y:FAILED"

zpy_ok


;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    ldy     #$ff
    iny
    cpy     #0  
    beq     iny_ok

    jmp *
    .byte "INY:FAILED"

iny_ok
    ldy     #10
    dey
    cpy     #9
    beq     dey_ok

    jmp *
    .byte "DEY:FAILED"

dey_ok
    ldx     #$80
    inx
    cpx     #$81
    beq     inx_ok

    jmp *
    .byte "INX:FAILED"

inx_ok
    ldx     #$00
    dex
    cpx     #$ff
    beq     dex_ok

    jmp *
    .byte "DEX:FAILED"

dex_ok

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Shift ops
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; asl a

    lda     #$01
    asl
    asl
    asl
    asl
    asl
    asl
    asl
    bpl     aslaerr
    beq     aslaerr
    bcs     aslaerr
    cmp     #$80
    beq     aslaok

aslaerr
    jmp *
    .byte "ASLA:FAILED"
    
aslaok

; lsr a

    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    bmi     lsraerr
    beq     lsraerr
    bcs     lsraerr
    bcc     lsraok

lsraerr
    jmp *
    .byte "LSRA:FAILED"
    
lsraok

; rol a

    clc
    lda     #$01
    rol
    rol
    rol
    rol
    rol
    rol
    rol
    bpl     rolaerr
    beq     rolaerr
    bcs     rolaerr
    cmp     #$80        ; this will set the carry !!!
    bne     rolaerr
    clc
    rol
    bcc     rolaerr
    bne     rolaerr
    bmi     rolaerr
    rol
    bcs     rolaerr
    bmi     rolaerr
    beq     rolaerr
    cmp     #1
    beq     rolaok

rolaerr
    jmp *
    .byte "ROLA:FAILED"

rolaok

; ror a

    clc
    lda     #$80
    ror
    ror
    ror
    ror
    ror
    ror
    ror
    bmi     roraerr
    beq     roraerr
    bcs     roraerr
    cmp     #$01        ; this will set the carry !!!
    bne     roraerr
    clc
    ror
    bcc     roraerr
    bne     roraerr
    bmi     roraerr
    ror
    bcs     roraerr
    bpl     roraerr
    beq     roraerr
    cmp     #$80
    beq     roraok
    bne     roraerr

roraerr
    jmp *
    .byte "RORA:FAILED"
    
roraok

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; pha / pla
    lda     #$ee
    pha
    lda     #00
    clc
    pla                 ; doesn't affect flags
;    bmi     plaerr
    bpl     plaerr
;    bne     plaerr
    beq     plaerr
    bcs     *
    bcc     plaok

plaerr
    jmp *
    .byte "PLA:FAILED"

plaok
    jmp *
    .byte "SUCCESS!"

n55
    .byte $55
nAA
    .byte $AA, 12, 34

.ifndef ATARI
    org $fffa
    .word 0, run, 0
.endif
