
; tests from Visual6502wiki/6502DecimalMode
; program by Ivo van Poorten
; assembler: mads by tebe

    .macro testN0
        bmi *
    .endm
    .macro testN1
        bpl *
    .endm
    .macro testV0
        bvs *
    .endm
    .macro testV1
        bvc *
    .endm
    .macro testZ0
        beq *
    .endm
    .macro testZ1
        bne *
    .endm
    .macro testC0
        bcs *
    .endm
    .macro testC1
        bcc *
    .endm
    .macro testA value
        cmp #:value
        bne *
    .endm
    .macro testADC init value carry result Nflag Vflag Zflag Cflag
        clv
        .if :carry = 1
            sec
        .else
            clc
        .endif
        lda #:init
        adc #:value
        .if :Nflag = 1
            testN1
        .else
            testN0
        .endif
        .if :Vflag = 1
            testV1
        .else
            testV0
        .endif
        .if :Zflag = 1
            testZ1
        .else
            testZ0
        .endif
        .if :Cflag = 1
            testC1
        .else
            testC0
        .endif
        testA :result
    .endm
    .macro testSBC init value carry result Nflag Vflag Zflag Cflag
        clv
        .if :carry = 1
            sec
        .else
            clc
        .endif
        lda #:init
        sbc #:value
        .if :Nflag = 1
            testN1
        .else
            testN0
        .endif
        .if :Vflag = 1
            testV1
        .else
            testV0
        .endif
        .if :Zflag = 1
            testZ1
        .else
            testZ0
        .endif
        .if :Cflag = 1
            testC1
        .else
            testC0
        .endif
        testA :result
    .endm

.ifndef ATARI
    opt h-
    opt f+

    org $0000
    dta 0
.endif

    org $8000

main:
    sed

    testADC $00 $00 0 $00 0 0 1 0
    testADC $79 $00 1 $80 1 1 0 0
    testADC $24 $56 0 $80 1 1 0 0
    testADC $93 $82 0 $75 0 1 0 1
    testADC $89 $76 0 $65 0 0 0 1
    testADC $89 $76 1 $66 0 0 1 1
    testADC $80 $f0 0 $d0 0 1 0 1
    testADC $80 $fa 0 $e0 1 0 0 1
    testADC $2f $4f 0 $74 0 0 0 0
    testADC $6f $00 1 $76 0 0 0 0

    testSBC $00 $00 0 $99 1 0 0 0
    testSBC $00 $00 1 $00 0 0 1 1
    testSBC $00 $01 1 $99 1 0 0 0
    testSBC $0a $00 1 $0a 0 0 0 1
    testSBC $0b $00 0 $0a 0 0 0 1
    testSBC $9a $00 1 $9a 1 0 0 1
    testSBC $9b $00 0 $9a 1 0 0 1

    jmp *

.ifndef ATARI
    org $fffa
    dta a(0), a(main), a(0)
.else
    run main
.endif
