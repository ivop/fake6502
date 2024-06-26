s	equ	$80
y	equ	$81
addr	equ	$82	; 2 bytes
data	equ	$84
exp	equ	$86
expnz	equ	$87
pos	equ	$89
scrptr	equ	$8a	; 2 bytes
printptr	equ	$8c	; 2 bytes

area	equ	$2e00

.ifndef ATARI
    opt h-
    opt f+
    org $0000
    dta 0
    .align $3000,0
.endif

	org	$3000
main
	mva	>area	addr+1
	ldx	#0
loop
	stx	pos
	mva	input,x	s
	mva	input+1,x	y
	mva	input+2,x	addr
	sta	las+1
	mva	input+3,x	data

	lda	s
	and	data
	sta	exp
	php:pla
	sta	expnz

	ldy	y
	mva	data	(addr),y
	ldx	s
	txs
las	dta	$bb,a(area)
	php
	cmp	exp
	bne	error
	cpx	exp
	bne	error
	pla
	eor	expnz
	and	#$80	; TODO: #$82, but Z is unstable
	bne	error
	tsx
	cpx	exp
	bne	error

	ldx	pos
	inx
	bne	loop

success
	jmp	*

error
	jmp	*

; just some random data
input
	dta	$73,$c3,$26,$17,$3b,$9b,$82,$06,$6e,$f8,$c6,$74,$83,$6c,$d6,$7c
	dta	$5b,$4f,$33,$72,$ef,$55,$69,$3f,$64,$f1,$02,$21,$ea,$51,$ad,$d8
	dta	$55,$41,$bd,$cc,$c9,$b3,$a7,$30,$78,$41,$ab,$ac,$bc,$61,$49,$94
	dta	$95,$a0,$b4,$37,$da,$aa,$e2,$50,$0f,$5f,$66,$12,$4d,$c4,$b7,$f4
	dta	$1b,$1a,$18,$a2,$a2,$df,$b6,$36,$27,$f7,$33,$3a,$33,$e2,$49,$6e
	dta	$4d,$25,$94,$f2,$b4,$c4,$50,$be,$f8,$0d,$10,$13,$e3,$82,$32,$cb
	dta	$9a,$1a,$1e,$2a,$52,$bb,$14,$25,$90,$1d,$96,$b9,$54,$e8,$2d,$45
	dta	$19,$5b,$9b,$86,$0e,$34,$3a,$2c,$77,$35,$9b,$91,$9d,$f8,$17,$a9
	dta	$2a,$70,$7a,$9e,$6b,$ce,$6f,$35,$4e,$1d,$d2,$6c,$95,$53,$95,$77
	dta	$17,$27,$5a,$83,$7e,$76,$74,$65,$6e,$74,$6a,$a5,$75,$79,$ac,$02
	dta	$af,$b5,$a2,$e1,$89,$87,$be,$c3,$87,$cd,$ae,$41,$74,$ea,$69,$8e
	dta	$ed,$d6,$2a,$1d,$a3,$eb,$17,$5a,$43,$d2,$a7,$0e,$6b,$43,$7b,$73
	dta	$92,$ec,$d3,$7a,$50,$3b,$3e,$57,$e6,$65,$b9,$c9,$75,$5f,$d8,$3a
	dta	$ca,$1e,$2c,$33,$26,$dd,$85,$28,$e9,$bd,$45,$34,$8a,$79,$59,$c1
	dta	$c7,$7c,$10,$9d,$6b,$28,$75,$9e,$a0,$89,$4a,$40,$26,$49,$5b,$54
	dta	$64,$1a,$48,$49,$b5,$7e,$68,$0f,$d6,$0e,$00,$27,$e2,$26,$62,$d7

.ifndef ATARI
    org $fffa
    dta a(0),a(main),a(0)
.else
    run main
.endif
