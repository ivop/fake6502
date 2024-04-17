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

    dta $12, $34, $56, $78

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

    jmp endless ;     [3]

.ifndef ATARI
    org $fffa
    dta a(nmi),a(main),a(irq)
.else
    run main
.endif

    .print "endless = ",endless

