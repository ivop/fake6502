
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

         lda #0
         sta db
         sta ab

next     lda db
         sta da
         sta dr
         sta cmd+1

         lda #%00001000
         bit pb
         bne decimal

         lda pb
         lsr
         lda ab
         and db
         ror
         sta ar

         lda pb
         ora #%00110000
         and #%00111100
         ldx ar
         bne nozero
         ora #%00000010
nozero
         ldx ar
         bpl nominus
         ora #%10000000
nominus
         tax
         lda ar
         and #%01000000
         beq nocarry
         inx
nocarry
         lda ar
         and #%01100000
         beq noover
         cmp #%01100000
         beq noover
         txa
         ora #%01000000
         tax
noover
         stx pr
         jmp nodecimal

decimal
         lda pb
         lsr
         lda ab
         and db
         sta aa
         ror
         sta ar

         lda pb
         ora #%00110000
         and #%00111100
         ldx ar
         bne dnozero
         ora #%00000010
dnozero
         ldx ar
         bpl dnominus
         ora #%10000000
dnominus
         tax
         lda ar
         eor aa
         and #%01000000
         beq dnoover
         txa
         ora #%01000000
         tax
dnoover
         lda aa
         and #$0f
         cmp #$05
         bcc noadjustlow
         lda ar
         and #$f0
         sta andlow+1
         lda ar
         clc
         adc #$06
         and #$0f
andlow   ora #$11
         sta ar
noadjustlow
         lda aa
         and #$f0
         cmp #$50
         bcc noadjusthigh
         inx
         lda ar
         clc
         adc #$60
         sta ar
noadjusthigh
         stx pr

nodecimal
         lda xb
         sta xr

         lda yb
         sta yr

         lda sb
         sta sr

         ldx sb
         txs
         lda pb
         pha
         lda ab
         ldx xb
         ldy yb
         plp

cmd      .byte $6b
         .byte 0

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

         clc
         lda db
         adc #17
         sta db
         bcc jmpnext
         lda #0
         sta db
         clc
         lda ab
         adc #17
         sta ab
         bcc jmpnext
         lda #0
         sta ab
         inc pb
         beq nonext
jmpnext  jmp next
nonext
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
