;
; Test number of cycles per instruction
; by Ivo van Poorten
;

.ifndef ATARI
    opt h-
    opt f+
    org 0
    dta 0
.endif

    org $80

    dta $12, $80, $56, $78

    org $90
    dta a($1ff0)

    org $2000

main:

    ; INIT

.ifdef ATARI
    sei
    mva #0 $d20e
    sta $d40e
    mva #$fe $d301
    mwa #nmi $fffa
    mwa #irq $fffe
.endif
    ldx #0      ; init to zero
    ldy #0
    cld
    clv
    jmp timing

nmi:
irq:
    rti         ; $40 [6]

endless:
    jmp *       ;     [3]

subrout:
    rts         ;     [6];

tramp:
    dta a(land_here)

    org $3000

timing:
                ; $00-$0f, documented opcodes --------------------------------

    dta 0,0     ; $00 [7] brk is a two byte instruction!
                ;     [6] rti
    ora ($80,x) ; $01 [6]
    ora $80     ; $05 [3]
    asl $80     ; $06 [5]
    php         ; $08 [3]
    ora #$ff    ; $09 [2] ...
    asl         ; $0a [2]
    ora $1000   ; $0d [4]
    asl $1000   ; $0e [6]

                ; $10-$1f, documented opcodes --------------------------------

    ldx #-1     ;     [2] init for branch
    bpl @+      ; $10 [2] not taken
@:
    ldx #0      ;     [2]
    bpl @+      ; $10 [3] taken
    .align $3090,$ea
@:
    bpl @+      ; $10 [4] taken, page cross
    .align $3100,$ea
@:
    ldy #0      ;     [2]
    ora ($90),y ; $11 [5]
    dey         ;     [2]
    ora ($90),y ; $11 [6]
    ora $80,x   ; $15 [4]
    asl $80,x   ; $16 [6]
    clc         ; $18 [2]
    ora $1000,y ; $19 [4]
    ora $1080,y ; $19 [5]
    dex         ;     [2]
    ora $1000,x ; $1d [4]
    ora $1080,x ; $1d [5]
    asl $1000,x ; $1e [7]

                ; $20-$2f, documented opcodes --------------------------------

    jsr subrout ; $20 [6]
                ;     [6] rts
    and ($80,x) ; $21 [6]
    bit $80     ; $24 [3]
    and $80     ; $25 [3]
    rol $80     ; $26 [5]
    plp         ; $28 [4]
    and #$ff    ; $29 [2]
    rol         ; $2a [2]
    bit $1000   ; $2c [4]
    and $1000   ; $2d [4]
    rol $1000   ; $2e [6]

                ; $30-$3f, documented opcodes --------------------------------

    ldx #0      ;     [2]
    bmi @+      ; $30 [2] not taken
@:
    ldx #-1     ;     [2]
    bmi @+      ;     [3] taken
    .align $3190,$ea
@:
    bmi @+      ;     [4] taken, page cross
    .align $3200,$ea
@:
    ldy #0      ;     [2]
    and ($90),y ; $31 [5]
    dey         ;     [2]
    and ($90),y ;     [6]
    and $80,x   ; $35 [4]
    rol $80,x   ; $36 [6]
    sec         ; $38 [2]
    and $1000,y ; $39 [4]
    and $1080,y ; $39 [5]
    and $1000,x ; $3a [4]
    and $1080,x ; $3d [5]
    rol $1000,x ; $3e [7]

                ; $40-$4f, documented opcodes --------------------------------

    ;rti        ; $40       skipped, already tested by brk
    eor ($80,x) ; $41 [6]
    eor $80     ; $45 [3]
    lsr $80     ; $46 [5]
    pha         ; $48 [3]
    eor #$ff    ; $49 [2]
    lsr         ; $4a [2]
    jmp next    ; $4c [3]
next:
    eor $1000   ; $4d [4]
    lsr $1000   ; $4e [6]

                ; $50-$5f, documented opcodes --------------------------------

    lda #$40    ;     [2] set V
    pha         ;     [3]
    plp         ;     [4]
    bvc @+      ; $50 [2] not taken
@:
    clv         ;     [2]
    bvc @+      ; $50 [3] taken
    .align $3290, $ea
@:
    bvc @+      ; $50 [4] taken, page cross
    .align $3300, $ea
@:
    ldy #0      ;     [2]
    eor ($90),y ; $51 [5]
    dey         ;     [2]
    eor ($90),y ; $51 [6]
    eor $80,x   ; $55 [4]
    lsr $80,x   ; $56 [6]
    cli         ; $58 [2]
    eor $1000,y ; $59 [4]
    eor $1080,y ; $59 [5]
    eor $1000,x ; $5d [4]
    eor $1080,x ; $5d [5]
    lsr $1000,x ; $5e [7]

                ; $60-$6f, documented opcodes --------------------------------

    ;rts        ; $60     skipped, already tested by jsr
    cld         ;     [2]
    adc ($80,x) ; $61 [6]
    adc $80     ; $65 [3]
    adc #$ff    ; $69 [2]
    adc $1000   ; $6d [4]

    sed         ;     [2]   adc +1 cycle in decimal mode
    adc ($80,x) ; $61 [7]
    adc $80     ; $65 [4]
    adc #$ff    ; $69 [3]
    adc $1000   ; $6d [5]

    ror $80     ; $66 [5]
    pla         ; $68 [4]
    ror         ; $6a [2]
    jmp (tramp) ; $6c [5]
land_here:
    ror $1000   ; $6e [6]

                ; $70-$7f, documented opcodes --------------------------------

    clv         ;     [2]
    bvs @+      ; $70 [2] not taken
@:
    lda #$40    ;     [2] set V
    pha         ;     [3]
    plp         ;     [4]
    bvs @+      ; $70 [3] taken
    .align $3390,$ea
@:
    bvs @+      ; $70 [4] taken, page cross
    .align $3400,$ea
@:
    cld         ;     [2]
    ldy #0      ;     [2]
    adc ($90),y ; $71 [5]
    dey         ;     [2]
    adc ($90),y ; $71 [6]
    adc $80,x   ; $75 [4]
    adc $1000,y ; $79 [4]
    adc $1080,y ; $79 [5]
    adc $1000,x ; $7d [4]
    adc $1080,x ; $7d [5]

    sed         ;     [2]   decimal mode, same as above, +1
    ldy #0      ;     [2]
    adc ($90),y ; $71 [6]
    dey         ;     [2]
    adc ($90),y ; $71 [7]
    adc $80,x   ; $75 [5]
    adc $1000,y ; $79 [5]
    adc $1080,y ; $79 [6]
    adc $1000,x ; $7d [5]
    adc $1080,x ; $7d [6]

    ror $80,x   ; $76 [6]
    sei         ; $78 [2]
    ror $1000,x ; $7e [7]

                ; $80-$8f, documented opcodes --------------------------------

    ldx #0      ;     [2]
    sta ($80,x) ; $81 [6]
    sty $a0     ; $84 [3]
    sta $a0     ; $85 [3]
    stx $a0     ; $86 [3]
    dey         ; $88 [2]
    txa         ; $8a [2]
    sty $1000   ; $8c [4]
    sta $1000   ; $8d [4]
    stx $1000   ; $8e [4]

                ; $90-$9f, documented opcodes --------------------------------

    sec         ;     [2]
    bcc @+      ; $90 [2] not taken
@:
    clc         ;     [2]
    bcc @+      ; $90 [3] taken
    .align $3490,$ea
@:
    bcc @+      ; $90 [4] taken, page cross
    .align $3500,$ea
@:
    ldy #0      ;     [2]
    sta ($80),y ; $91 [6] no page cross
    dey         ;     [2]
    sty $80     ;     [3]
    sta ($80),y ; $91 [6] page cross, but still 6(!)
    ldx #0      ;     [2]
    sty $80,x   ; $94 [4]
    sta $80,x   ; $95 [4]
    stx $80,y   ; $96 [4]
    tya         ; $98 [2]
    sta $1000,y ; $99 [5]
    sta $10ff,y ; $99 [5]
    txs         ; $9a [2]
    sta $1000,x ; $9d [5]
    sta $10ff,x ; $9d [5]

                ; $a0-$af, documented opcodes --------------------------------

    ldy #0      ; $a0 [2]
    lda ($80,x) ; $a1 [6]
    ldx #0      ; $a2 [2]
    ldy $80     ; $a4 [3]
    lda $80     ; $a5 [3]
    ldx $80     ; $a6 [3]
    tay         ; $a8 [2]
    lda #0      ; $a9 [2]
    tax         ; $aa [2]
    ldy $1000   ; $ac [4]
    lda $1000   ; $ad [4]
    ldx $1000   ; $ae [4]

                ; $b0-$bf, documented opcodes --------------------------------

    clc         ;     [2]
    bcs @+      ; $b0 [2] not taken
@:
    sec         ;     [2]
    bcs @+      ; $b0 [3] taken
    .align $3590,$ea
@:
    bcs @+      ; $b0 [4] taken, page cross
    .align $3600,$ea
@:
    ldy #0      ;     [2]
    lda ($90),y ; $b1 [5]
    dey         ;     [2]
    lda ($90),y ; $b1 [6]
    ldy $80,x   ; $b4 [4]
    lda $80,x   ; $b5 [4]
    ldx $80,y   ; $b6 [4]
    clv         ; $b8 [2]
    ldy #$ff    ;     [2]
    lda $1000,y ; $b9 [4]
    lda $1080,y ; $b9 [5]
    tsx         ; $ba [2]
    ldx #$ff    ;     [2]
    ldy $1000,x ; $bc [4]
    ldy $1001,x ; $bc [5]
    lda $1000,x ; $bd [4]
    lda $1001,x ; $bd [5]
    ldx $1000,y ; $be [4]
    lda $1001,y ; $be [5]

                ; $c0-$cf, documented opcodes --------------------------------

    cpy #$5a    ; $c0 [2]
    cmp ($80,x) ; $c1 [6]
    cpy $80     ; $c4 [3]
    cmp $80     ; $c5 [3]
    dec $80     ; $c6 [5]
    iny         ; $c8 [2]
    cmp #$5a    ; $c9 [2]
    dex         ; $ca [2]
    cpy $1000   ; $cc [4]
    cmp $1000   ; $cd [4]
    dec $1000   ; $ce [6]

                ; $d0-$df, documented opcodes --------------------------------

    ldx #0      ;     [2]
    bne @+      ; $d0 [2] not taken
@:
    dex         ;     [2]
    bne @+      ; $d0 [3] taken
    .align $3690,$ea
@:
    bne @+      ; $d0 [4] taken, page cross
    .align $3700,$ea
@:
    ldy #0      ;     [2]
    cmp ($90),y ; $d1 [5]
    dey         ;     [2]
    cmp ($90),y ; $d1 [6]
    cmp $80,x   ; $d5 [4]
    dec $80,x   ; $d6 [6]
    cld         ; $d8 [2]
    cmp $1000,y ; $d9 [4]
    cmp $1080,y ; $d9 [5]
    cmp $1000,x ; $dd [4]
    cmp $1080,x ; $dd [5]
    dec $1000,x ; $de [7]

                ; $e0-$ef, documented opcodes --------------------------------

    cpx #$ff    ; $e0 [2]
    cld         ;     [2]
    sbc ($80,x) ; $e1 [6]
    sbc $80     ; $e5 [3]
    sbc #$ff    ; $e9 [2]
    sbc $1000   ; $ed [4]

    sed         ;     [2] same in decimal mode
    sbc ($80,x) ; $e1 [7]
    sbc $80     ; $e5 [4]
    sbc #$ff    ; $e9 [3]
    sbc $1000   ; $ed [5]

    cpx $80     ; $e4 [3]
    inc $80     ; $e6 [5]
    inx         ; $e8 [2]
    nop         ; $ea [2]
    cpx $1000   ; $ec [4]
    inc $1000   ; $ee [6]

                ; $f0-$ff, documented opcodes --------------------------------

    ldx #$ff    ;     [2]
    beq @+      ; $f0 [2] not taken
@:
    inx         ;     [2]
    beq @+      ; $f0 [3] taken
    .align $3790,$ea
@:
    beq @+      ; $f0 [4] taken, page cross
    .align $3800,$ea
@:
    cld         ;     [2]
    ldy #0      ;     [2]
    sbc ($90),y ; $f1 [5]
    dey         ;     [2]
    sbc ($90),y ; $f1 [6]
    sbc $80,x   ; $f5 [4]
    sbc $1000,y ; $f9 [4]
    sbc $1080,y ; $f9 [5]
    ldx #0      ;     [2]
    sbc $1080,x ; $fd [4]
    dex         ;     [2]
    sbc $1080,x ; $fd [5]

    sed         ;     [2] same, BCD
    ldy #0      ;     [2]
    sbc ($90),y ; $f1 [6]
    dey         ;     [2]
    sbc ($90),y ; $f1 [7]
    sbc $80,x   ; $f5 [5]
    sbc $1000,y ; $f9 [5]
    sbc $1080,y ; $f9 [6]
    ldx #0      ;     [2]
    sbc $1080,x ; $fd [5]
    dex         ;     [2]
    sbc $1080,x ; $fd [6]

    inc $80,x   ; $f6 [6]
    sed         ; $f8 [2]
    inc $1000,x ; $fe [7]

    jmp endless ;     [3]

.ifndef ATARI
    org $fffa
    dta a(nmi),a(main),a(irq)
.else
    run main
.endif

    .print "endless = ",endless

