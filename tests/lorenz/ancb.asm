
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
         lda #$0b       ; ANC #imm
         sta cmd

next     lda db
         sta da
         sta dr
         sta cmd+1
         eor #$ff
         sta cmdr+1

         lda ab
         eor #$ff
cmdr     ora #0
         eor #$ff
         sta ar

         lda xb
         sta xr

         lda yb
         sta yr

         lda pb
         ora #%00110000
         and #%01111100
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
         ora #%10000001
         tax
noneg    stx pr

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

cmd      .byte 0
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
         lda #$2b       ; ANC #imm (2nd version)
         cmp cmd
         beq end
         sta cmd
         jmp next
end
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
