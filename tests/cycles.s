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

    jmp timing

nmi:
irq:
    rti         ; $40 [6]

endless:
    jmp *       ;     [3]

    org $3000

timing:
    dta 0,0     ; $00 [7] brk is a two byte instruction!
                ; $40 [6] rti
    ora ($80,x) ; $01 [6]
    ora $80     ; $05 [3]
    asl $80     ; $06 [5]
    php         ; $08 [3]
    ora #$ff    ; $09 [2]

    jmp endless ;     [3]

.ifndef ATARI
    org $fffa
    dta a(nmi),a(main),a(irq)
.else
    run main
.endif

    .print "endless = ",endless

