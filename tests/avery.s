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

d0 = $e0
d1 = $e1

    org     $2000

main:

    ; ========== FLAGS ==========

    ;push flags and check unused bit 5
    php
    pla
    tax
    and     #$20
    bne     bit5_ok
    jmp *
    dta     c"Unused P bit 5 was not set.",0
bit5_ok:

    ;check bit 4 (break bit)
    txa
    and     #$10
    bne     bit4_ok
    jmp *
    dta     c"Break bit (P bit 4) was not set.",0
bit4_ok:

    ;see if we can clear those bits
    txa
    and     #$bf
    pha
    plp
    php
    pla
    tax
    
    ;recheck bit 5
    and     #$20
    bne     bit5_still_ok
    jmp *
    dta     c"Was able to clear P bit 5.",0
bit5_still_ok:
    ;recheck bit 4
    txa
    and     #$10
    bne     bit4_still_ok
    jmp *
    dta     c"Was able to clear break bit (P bit 4).",0
bit4_still_ok:

    ; ========== BCD ==========

    sed
    clc
    lda     #$06
    adc     #$19
    sta     d0
    php
    pla
    and     #$c3
    sta     d1

    lda d0
    cmp #$25
    bne *       ; error
    lda d1
    cmp #$00
    bne *       ; error

    sec
    lda     #$7e
    adc     #$11
    sta     d0
    php
    pla
    and     #$c3
    sta     d1

    lda d0
    cmp #$96
    bne *
    lda d1
    cmp #$c0
    bne *

success
    jmp *

.ifndef ATARI
    org $fffa
    dta a(0),a(main),a(0)
.else
    run main
.endif
