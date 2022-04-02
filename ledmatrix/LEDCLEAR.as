
	psect	text, global, pure
	psect	data, global

_io	equ	0Ch
_sda	equ	7
_scl	equ	0
_idle	equ	10001101B
_addr	equ 070h

	psect	text
	defs	100h

main:
	ld	sp,stack

	call _i2c_start
	ld	c, 224			; E0 = 70 shifted left
	call _i2c_write		
	ld c, 21H
	call _i2c_write	
	ld (rc), a
	call _i2c_stop

	call _ack

	ld	bc,100
6:	nop
	dec	bc
	ld	a,c
	or	b
	jr	nz,6b

	call _i2c_start
	ld c, 224
	call _i2c_write
	ld c, 81h			; switch display on
	call _i2c_write
	ld (rc), a
	call _i2c_stop
	
	call _i2c_start
	ld c, 224
	call _i2c_write
	ld c, 00h			; display data address pointer to 0000
	call _i2c_write
	
	ld c, 00			; data byte 1
	call _i2c_write
	ld c, 00h			; data byte 2
	call _i2c_write
	ld c, 00			; data byte 3
	call _i2c_write
	ld c, 00h			; data byte 4
	call _i2c_write
	ld c, 00			; data byte 5
	call _i2c_write
	ld c, 00h			; data byte 6
	call _i2c_write
	ld c, 00			; data byte 7
	call _i2c_write
	ld c, 00h			; data byte 8
	call _i2c_write
	ld c, 00			; data byte 9
	call _i2c_write
	ld c, 00h			; data byte 10
	call _i2c_write
	ld c, 00			; data byte 11
	call _i2c_write
	ld c, 00h			; data byte 12
	call _i2c_write
	ld c, 00			; data byte 13
	call _i2c_write
	ld c, 00h			; data byte 14
	call _i2c_write
	ld c, 00			; data byte 15
	call _i2c_write
	ld c, 00h			; data byte 16
	call _i2c_write
	
	ld (rc), a
	call _i2c_stop
		
	call _ack

	jp 0

_ack:
	ld	a,(rc)
	or	a
	jp	z,4f
	ld	c,'-'
	call _cout
	jp 5f
	
4:	ld c, 'X'
	call _cout

5:
	ret

_cout:
	push	af
	push	bc
	push	de
	push	hl
	ld	e,c
	ld	c,02h
	call	5
	pop	hl
	pop	de
	pop	bc
	pop	af
	ret

_i2c_start:
	ld	a,_idle
	out	(_io),a

	res	_sda,a
	out	(_io),a
	nop
	nop
	res	_scl,a
	out	(_io),a

	ld	(oprval),a
	ret

_i2c_stop:
	ld	a,(oprval)
	res	_scl,a
	res	_sda,a
	out	(_io),a

	set	_scl,a
	out	(_io),a
	nop
	nop
	set	_sda,a
	out	(_io),a

	ld	(oprval),a
	ret

_i2c_write:
	ld	a,(oprval)

	ld	b,8
1:	res	_sda,a
	rl	c
	jr	nc,2f
	set	_sda,a
2:	out	(_io),a
	set	_scl,a
	out	(_io),a

	res	_scl,a
	out	(_io),a
	djnz	1b

	set	_sda,a
	out	(_io),a
	set	_scl,a
	out	(_io),a

	ld	d,a
4:	in	a,(_io)

	ld	c,a
	ld	a,d

	res	_scl,a
	out	(_io),a
	ld	(oprval),a
	
	xor	a
	bit	_sda,c
	ret	z
	inc	a

	ret
	
psect data
	
oprval:	defb	0
rc:	defb	0

defs	128
stack: