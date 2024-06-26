
.ifndef ATARI
    opt h-
    opt f+
    org $0000
    dta 0
.endif

; CRC-32 based on my implementation in cc65 / zlib

D_FLAG	equ	8
C_FLAG	equ	1
CRC_POLY3	equ	$ed
CRC_POLY2	equ	$b8
CRC_POLY1	equ	$83
CRC_POLY0	equ	$20

OK3	equ	$3d
OK2	equ	$d8
OK1	equ	$4d
OK0	equ	$2a

crc	equ	$80	; 4 bytes
zpdata	equ	$84
zpflags	equ	$85
scrptr	equ	$86	; 2 bytes

	org	$3000
main
	jsr	init_crc
	lda	#0
	jsr	test_adc_sbc
	lda	#C_FLAG
	jsr	test_adc_sbc
	lda	#D_FLAG
	jsr	test_adc_sbc
	lda	#D_FLAG|C_FLAG
	jsr	test_adc_sbc

	lda	crc+3
	cmp	#OK3
	bne	error
	lda	crc+2
	cmp	#OK2
	bne	error
	lda	crc+1
	cmp	#OK1
	bne	error
	lda	crc
	cmp	#OK0
	bne	error
	jmp	*
    .byte "PASS"

error
	jmp	*
    .byte "FAIL"

test_adc_sbc
	sta	zpflags
	mva	#0	zpdata
	ldy	#0
test_adc_sbc_byte
	mva	zpdata	arr+1

	lda:pha	zpflags
	tya
	plp
	adc	zpdata
	php
	jsr	do_crc
	pla
	jsr	do_crc

	lda:pha	zpflags
	tya
	plp
	sbc	zpdata
	php
	jsr	do_crc
	pla
	jsr	do_crc

	lda:pha	zpflags
	tya
	plp
arr	dta	$6b,0
	php
	jsr	do_crc
	pla
	jsr	do_crc

	iny
	bne	test_adc_sbc_byte
	inc	zpdata
	bne	test_adc_sbc_byte
	rts

init_crc
	ldx	#0
init_crc_entry
	lda	#0
	sta	crc+1
	sta	crc+2
	sta	crc+3
	ldy	#8
	txa
init_crc_bit
	sta	crc
	lsr	@
	bcc	init_crc_noxor
	lda	crc+3
	lsr	@
	eor	#CRC_POLY3
	sta	crc+3
	lda	crc+2
	ror	@
	eor	#CRC_POLY2
	sta	crc+2
	lda	crc+1
	ror	@
	eor	#CRC_POLY1
	sta	crc+1
	lda	crc
	ror	@
	eor	#CRC_POLY0
	bcs	init_crc_nextbit ; branch always
init_crc_noxor
	rol	@
	lsr	crc+3
	ror	crc+2
	ror	crc+1
	ror	@
init_crc_nextbit
	dey
	bne	init_crc_bit
	sta	crc_table_0,x
	mva	crc+1	crc_table_1,x
	mva	crc+2	crc_table_2,x
	mva	crc+3	crc_table_3,x
	inx
	bne	init_crc_entry
	mwa	#0	crc
	sta	crc+2
	sta	crc+3
	rts

; crc = (crc >> 8) ^ crc_table[(crc & 0xff) ^ input];
do_crc
	eor	crc
	tax
	lda	crc_table_0,x
	eor	crc+1
	sta	crc
	lda	crc_table_1,x
	eor	crc+2
	sta	crc+1
	lda	crc_table_2,x
	sta	crc+3
	sta	crc+2
	mva	crc_table_3,x	crc+3
	rts

title	dta	c'CPU: Decimal mode...',0
pass	dta	c'Pass',$9b,0
fail	dta	c'FAIL.',$9b,0

crc_table_0	org	*+256
crc_table_1	org	*+256
crc_table_2	org	*+256
crc_table_3	org	*+256

.ifdef ATARI
    run main
.else
    org $fffa
    dta a(0),a(main),a(0)
.endif
