
.ifndef ATARI
    opt h-
    opt f+
    org 0
    dta 0
    .align $0801,0
.else
    org $0801
.endif

main
         lda #%00011011
         sta db
         lda #%11000110
         sta ab
         lda #%10110001
         sta xb
         lda #%01101100
         sta yb
         lda #0
         sta pb
         tsx
         stx sb

         stx saves+1
         ldx #0
s0       lda $0100,x
         sta $1000,x
         inx
         bne s0

         lda #0
         sta sb
         sta db
         sta yb

next     lda db
         sta da
         sta dr
         and sb
         sta ar
         sta xr
         sta sr

         lda yb
         sta yr

         lda pb
         ora #%00110000
         and #%01111101
         tax
         lda ar
         cmp #0
         bne nozero
         txa
         ora #%00000010
         tax
nozero   lda ar
         bpl noneg
         txa
         ora #%10000000
         tax
noneg    stx pr

waitborder
         lda $d011
         bmi isborder
         lda $d012
         cmp #30
         bcs waitborder
isborder

         ldx sb
         txs
         lda pb
         pha
         lda ab
         ldx xb
         ldy yb
         plp

cmd      .byte $bb
         .word da

         php
         cld
         sta aa
         stx xa
         sty ya
         pla
         sta pa
         tsx
         stx sa
         jsr check

         inc cmd+1
         bne noinc
         inc cmd+2
noinc    lda yb
         bne nodec
         dec cmd+2
nodec    dec yb

         clc
         lda db
         adc #17
         sta db
         bcc jmpnext
         lda #0
         sta db
         clc
         lda sb
         adc #17
         sta sb
         bcc jmpnext
         lda #0
         sta sb
         inc pb
         beq nonext
jmpnext  jmp next
nonext

saves    ldx #$11
         txs
         ldx #0
s1       lda $1000,x
         sta $0100,x
         inx
         bne s1

success jmp *

db       .byte 0
ab       .byte 0
xb       .byte 0
yb       .byte 0
pb       .byte 0
sb       .byte 0
da       .byte 0
aa       .byte 0
xa       .byte 0
ya       .byte 0
pa       .byte 0
sa       .byte 0
dr       .byte 0
ar       .byte 0
xr       .byte 0
yr       .byte 0
pr       .byte 0
sr       .byte 0

check
         lda da
         cmp dr
         bne error
         lda aa
         cmp ar
         bne error
         lda xa
         cmp xr
         bne error
         lda ya
         cmp yr
         bne error
         lda pa
         cmp pr
         bne error
         lda sa
         cmp sr
         bne error
         rts

error    jmp *

.ifndef ATARI
    org $fffa
    dta a(0),a(main),a(0)
.endif
