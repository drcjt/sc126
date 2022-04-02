; INPUT FILE C:\USERS\DRCJT\SOURCE\REPOS\CSHARP-80\SAMPLES\FIB\BIN\DEBUG\NET6.0\FIB.DLL
; 21/11/2021 20:27:23

	ORG 100H
	JP START
START:	
	CALL M2
	RET 
; System.Void MiniBCL.Program::Main()
M2:	
BB0:	
	;CALL CLS
	LD HL, 9
	PUSH HL
	LD HL, 0
	PUSH HL
	CALL M0
	CALL M1
	POP BC
	PUSH BC
	RET 
; System.Int16 MiniBCL.Program::Fibonacci(System.Int16)
M0:	
	PUSH IX
	LD IX, 0
	ADD IX, SP
	LD HL, -24
	ADD HL, SP
	LD SP, HL
BB1:	
	LD HL, 0
	PUSH HL
	LD HL, 0
	LD (IX-3), H
	LD (IX-4), L
	POP HL
	LD (IX-1), H
	LD (IX-2), L
	LD HL, 1
	PUSH HL
	LD HL, 0
	LD (IX-7), H
	LD (IX-8), L
	POP HL
	LD (IX-5), H
	LD (IX-6), L
	LD HL, 0
	PUSH HL
	LD HL, 0
	LD (IX-11), H
	LD (IX-12), L
	POP HL
	LD (IX-9), H
	LD (IX-10), L
	JP BB2
BB3:	
	LD H, (IX-1)
	LD L, (IX-2)
	PUSH HL
	LD H, (IX-3)
	LD L, (IX-4)
	LD (IX-15), H
	LD (IX-16), L
	POP HL
	LD (IX-13), H
	LD (IX-14), L
	LD H, (IX-5)
	LD L, (IX-6)
	PUSH HL
	LD H, (IX-7)
	LD L, (IX-8)
	LD (IX-3), H
	LD (IX-4), L
	POP HL
	LD (IX-1), H
	LD (IX-2), L
	LD H, (IX-13)
	LD L, (IX-14)
	PUSH HL
	LD H, (IX-15)
	LD L, (IX-16)
	PUSH HL
	LD H, (IX-5)
	LD L, (IX-6)
	PUSH HL
	LD H, (IX-7)
	LD L, (IX-8)
	PUSH HL
	CALL I_ADD
	POP HL
	POP DE
	LD H, D
	ADD HL, HL
	SBC HL, HL
	PUSH DE
	LD (IX-7), H
	LD (IX-8), L
	POP HL
	LD (IX-5), H
	LD (IX-6), L
	LD H, (IX-9)
	LD L, (IX-10)
	PUSH HL
	LD H, (IX-11)
	LD L, (IX-12)
	PUSH HL
	LD HL, 1
	PUSH HL
	LD HL, 0
	PUSH HL
	CALL I_ADD
	POP HL
	POP DE
	LD H, D
	ADD HL, HL
	SBC HL, HL
	PUSH DE
	LD (IX-11), H
	LD (IX-12), L
	POP HL
	LD (IX-9), H
	LD (IX-10), L
BB2:	
	LD H, (IX-9)
	LD L, (IX-10)
	PUSH HL
	LD H, (IX-11)
	LD L, (IX-12)
	PUSH HL
	LD H, (IX+7)
	LD L, (IX+6)
	PUSH HL
	LD H, (IX+5)
	LD L, (IX+4)
	PUSH HL
	CALL I_LT
	LD HL, 0
	ADC HL, HL
	PUSH HL
	LD HL, 0
	LD (IX-19), H
	LD (IX-20), L
	POP HL
	LD (IX-17), H
	LD (IX-18), L
	LD HL, 0
	PUSH HL
	LD HL, 0
	PUSH HL
	LD H, (IX-17)
	LD L, (IX-18)
	PUSH HL
	LD H, (IX-19)
	LD L, (IX-20)
	PUSH HL
	CALL I_NEQ
	LD HL, 0
	ADC HL, HL
	PUSH HL
	LD HL, 0
	POP HL
	LD A, 0
	ADD A, L
	JP NZ , BB3
BB4:	
	LD H, (IX-1)
	LD L, (IX-2)
	PUSH HL
	LD H, (IX-3)
	LD L, (IX-4)
	LD (IX-23), H
	LD (IX-24), L
	POP HL
	LD (IX-21), H
	LD (IX-22), L
	JP BB5
BB5:	
	LD H, (IX-21)
	LD L, (IX-22)
	PUSH HL
	LD H, (IX-23)
	LD L, (IX-24)
	PUSH HL
	POP DE
	POP IY
	LD SP, IX
	POP IX
	LD HL, 0
	ADD HL, SP
	LD BC, 6
	ADD HL, BC
	POP BC
	LD SP, HL
	PUSH IY
	PUSH DE
	PUSH BC
	RET 
; System.Void System.Console::WriteLine(System.Int32)
M1:	
	PUSH IX
	LD IX, 0
	ADD IX, SP
BB6:	
	LD H, (IX+7)
	LD L, (IX+6)
	PUSH HL
	LD H, (IX+5)
	LD L, (IX+4)
	PUSH HL
	POP DE
	POP HL
	CALL LTOA
	;CALL M3
	POP IX
	LD HL, 0
	ADD HL, SP
	LD BC, 6
	ADD HL, BC
	POP BC
	LD SP, HL
	PUSH BC
	RET 
; System.Void System.Console::WriteLine()
M3:	
BB7:	
	CALL M4
	POP DE
	POP HL
	LD A, L
	CALL 33H
	POP BC
	PUSH BC
	RET 
; System.Char System.Environment::get_NewLine()
M4:	
BB8:	
	LD HL, 10
	PUSH HL
	LD HL, 0
	PUSH HL
	POP DE
	POP IY
	POP BC
	PUSH IY
	PUSH DE
	PUSH BC
	RET 

; **** Runtime starts here
; This routine performs the operation DEHL = DEHL << C
;
; Uses: HL, DE, BC, AF, AF'


i_lsh:	
	POP AF		; Save return address
	EX AF, AF'

	POP BC
	POP BC

	POP DE
	POP HL

	LD A, C

	OR A
	JR Z, i_lsh_end

	LD B, A
	LD A, E

i_lsh_loop:

	ADD HL, HL
	RLA
	RL D

	DJNZ i_lsh_loop

	LD E, A

i_lsh_end:

	PUSH HL		; Put result back on stack
	PUSH DE

	EX AF, AF'	; Restore return address
	PUSH AF

	RET
; This routine performs the operation DEHL = DEHL >> C
;
; Uses: HL, DE, BC, AF, AF'


i_rsh:	
	POP AF		; Save return address
	EX AF, AF'

	POP BC
	POP BC

	POP DE
	POP HL

	LD A, C

	OR A
	JR Z, i_rsh_end

	LD B, A
	LD A, E

i_rsh_loop:

	SRL D
	RRA
	RR H
	RR L

	DJNZ i_lsh_loop

	LD E, A

i_rsh_end:

	PUSH HL		; Put result back on stack
	PUSH DE

	EX AF, AF'	; Restore return address
	PUSH AF

	RET
; Fast keyboard test routine.
; Returns non-zero int32 on stack if key pressed

KEYAVAIL:
	POP BC	; return address

	LD HL, (387FH)
	LD H, 0

	PUSH HL

	LD HL, 0
	PUSH HL

	PUSH BC
	RET
SETRES:
	POP BC		; SAVE RETURN ADDRESS

	POP HL		; Set or Reset
	POP HL

	POP DE		; Get y-coordinate into DE
	POP DE

	LD A, E		; Save y into A

	POP DE		; get x-coordinate into HL
	POP DE

	LD D, E		; Move x to D
	LD E, A		; Set E as y

	PUSH BC		; push return address

	LD a, L	; SET/RESET

	LD HL, SETRESDATA
	PUSH AF
	LD A, D		; X Coordinate
	PUSH AF
	LD A, E		; Y coordinate
	JP 150H
	
SETRESDATA:
	DB 29H
KBDSCAN:
	POP BC	; return address

	CALL 2BH
	
	LD H, 0
	LD L, A		; The key that was pressed if any
	PUSH HL

	LD HL, 0
	PUSH HL

	PUSH BC
	RET
DELAY:
	POP HL	; return address

	POP BC
	POP BC   ; delay required

DELAY1:			; should be 14.65 microseconds per loop
	DEC BC
	LD A, B
	OR C
	JR NZ, DELAY1

	PUSH HL
	RET
TICKS:
	POP BC	; return address

	; Push ticks as 32 bit number onto stack
	LD HL, (4040H)	; 25ms hearbeat counter on TRS-80 Model 1
	LD DE, 0
	PUSH HL
	PUSH DE

	PUSH BC
	RET
CLS:
	PUSH AF
	CALL 01C9H
	POP AF
	RET
; display number in HL as ascii
ITOA:
	PUSH IX

	BIT 7, H
	JR Z, CONV2

	LD A, '-'
	CALL 33H

	XOR A
	SUB L
	LD L, A
	LD A, 0
	SBC A, H
	LD H, A
    JP CONV2

UITOA:
    PUSH IX

CONV2:

	CALL B2D16
	CALL PRINT
	POP IX
	RET

; Converts unsigned int in DE:HL to ascii and prints it
LTOA:
    PUSH AF
	PUSH BC
	PUSH DE
    PUSH IY
	PUSH IX

	BIT 7, D        ; Test if long is negative
	JR Z, LTOA2

    PUSH DE
    PUSH HL
	LD A, '-'       ; Print -ve sign
	CALL 33H
    POP HL
    POP DE

    LD A, L         ; invert dehl
    CPL
    LD L, A
    LD A, H
    CPL
    LD H, A
    LD A, E
    CPL
    LD E, A
    LD A, D
    CPL
    LD D, A

    call l_inc_dehl     ; inc dehl

    JP LTOA2

ULTOA:
    PUSH AF
	PUSH BC
	PUSH DE
    PUSH IY
	PUSH IX

LTOA2:
	CALL B2D32
	CALL PRINT
	POP IX
	POP IY
	POP DE
	POP BC
	POP AF
	RET
	
; Combined routine for conversion of different sized binary numbers into
; directly printable ASCII(Z)-string
; Input value in registers, number size and -related to that- registers to fill
; is selected by calling the correct entry:
;
;  entry  inputregister(s)  decimal value 0 to:
;   B2D8             A                    255  (3 digits)
;   B2D16           HL                  65535   5   "
;   B2D24         E:HL               16777215   8   "
;   B2D32        DE:HL             4294967295  10   "
;   B2D48     BC:DE:HL        281474976710655  15   "
;   B2D64  IX:BC:DE:HL   18446744073709551615  20   "
;
; The resulting string is placed into a small buffer attached to this routine,
; this buffer needs no initialization and can be modified as desired.
; The number is aligned to the right, and leading 0's are replaced with spaces.
; On exit HL points to the first digit, (B)C = number of decimals
; This way any re-alignment / postprocessing is made easy.
; Changes: AF,BC,DE,HL,IX

; by Alwin Henseler

B2D8:    LD H,0
         LD L,A
B2D16:   LD E,0
B2D24:   LD D,0
B2D32:   LD BC,0
B2D48:   LD IX,0          ; zero all non-used bits
B2D64:   LD (B2DINV),HL
         LD (B2DINV+2),DE
         LD (B2DINV+4),BC
         LD (B2DINV+6),IX ; place full 64-bit input value in buffer
         LD HL,B2DBUF
         LD DE,B2DBUF+1
         LD (HL)," "
B2DFILC: EQU $-1         ; address of fill-character
         LD BC,18
         LDIR            ; fill 1st 19 bytes of buffer with spaces
         LD (B2DEND-1),BC ;set BCD value to "0" & place terminating 0
         LD E,1          ; no. of bytes in BCD value
         LD HL,B2DINV+8  ; (address MSB input)+1
         LD BC,0909H
         XOR A
B2DSKP0: DEC B
         JR Z,B2DSIZ     ; all 0: continue with postprocessing
         DEC HL
         OR (HL)         ; find first byte <>0
         JR Z,B2DSKP0
B2DFND1: DEC C
         RLA
         JR NC,B2DFND1   ; determine no. of most significant 1-bit
         RRA
         LD D,A          ; byte from binary input value
B2DLUS2: PUSH HL
         PUSH BC
B2DLUS1: LD HL,B2DEND-1  ; address LSB of BCD value
         LD B,E          ; current length of BCD value in bytes
         RL D            ; highest bit from input value -> carry
B2DLUS0: LD A,(HL)
         ADC A,A
         DAA
         LD (HL),A       ; double 1 BCD byte from intermediate result
         DEC HL
         DJNZ B2DLUS0    ; and go on to double entire BCD value (+carry!)
         JR NC,B2DNXT
         INC E           ; carry at MSB -> BCD value grew 1 byte larger
         LD (HL),1       ; initialize new MSB of BCD value
B2DNXT:  DEC C
         JR NZ,B2DLUS1   ; repeat for remaining bits from 1 input byte
         POP BC          ; no. of remaining bytes in input value
         LD C,8          ; reset bit-counter
         POP HL          ; pointer to byte from input value
         DEC HL
         LD D,(HL)       ; get next group of 8 bits
         DJNZ B2DLUS2    ; and repeat until last byte from input value
B2DSIZ:  LD HL,B2DEND    ; address of terminating 0
         LD C,E          ; size of BCD value in bytes
         OR A
         SBC HL,BC       ; calculate address of MSB BCD
         LD D,H
         LD E,L
         SBC HL,BC
         EX DE,HL        ; HL=address BCD value, DE=start of decimal value
         LD B,C          ; no. of bytes BCD
         SLA C           ; no. of bytes decimal (possibly 1 too high)
         LD A,"0"
         RLD             ; shift bits 4-7 of (HL) into bit 0-3 of A
         CP "0"          ; (HL) was > 9h?
         JR NZ,B2DEXPH   ; if yes, start with recording high digit
         DEC C           ; correct number of decimals
         INC DE          ; correct start address
         JR B2DEXPL      ; continue with converting low digit
B2DEXP:  RLD             ; shift high digit (HL) into low digit of A
B2DEXPH: LD (DE),A       ; record resulting ASCII-code
         INC DE
B2DEXPL: RLD
         LD (DE),A
         INC DE
         INC HL          ; next BCD-byte
         DJNZ B2DEXP     ; and go on to convert each BCD-byte into 2 ASCII
         SBC HL,BC       ; return with HL pointing to 1st decimal
         RET

B2DINV:  DS 8            ; space for 64-bit input value (LSB first)
B2DBUF:  DS 20           ; space for 20 decimal digits
B2DEND:  DS 1            ; space for terminating 0
; This routine performs the operation DEHL = DEHL + BCAF
;
; Uses: HL, DE, BC, AF, AF'


i_add:	
	POP AF		; Save return address
	EX AF, AF'

	POP BC
	POP AF

	POP DE
	POP HL

	PUSH BC		; Put BC back
	PUSH AF		; Swap AF and BC

	OR A

	POP BC
	ADC HL, BC	; Add LSW

	POP BC		; Add MSW
	EX DE, HL
	ADC HL, BC
	EX DE, HL	

	PUSH HL		; Put result back on stack
	PUSH DE

	EX AF, AF'	; Restore return address
	PUSH AF

	RET
   ; signed division of 32-bit numbers
   ;
   ; enter : dehl = 32-bit divisor
   ;         dehl'= 32-bit dividend
   ;
   ; exit  : success
   ;
   ;            dehl = 32-bit quotient
   ;            dehl'= 32-bit remainder
   ;            carry reset
   ;
   ;         divide by zero
   ;
   ;            dehl = $ffffffff = ULONG_MAX
   ;            dehl'= dividend
   ;            carry set, errno = EDOM
   ;
   ; uses  : af, bc, de, hl, bc', de', hl'

i_div:
    pop af
    ex af, af'

    pop de
    pop hl
    exx
    pop de
    pop hl
    exx

   ld a,d
   or e
   or h
   or l
   jr z, divide_zero_s  

   call l0_small_div_32_32x32

   push hl
   push de

   ex af, af'
   push af
   ret

l0_small_div_32_32x32:

   ld a,d
   
   exx
   
   ld b,d                      ; b = MSB of dividend
   ld c,a                      ; c = MSB of divisor
   
   push bc                     ; save sign info

   bit 7,d
   call nz, i_neg_dehl         ; take absolute value of dividend

   exx

   bit 7,d
   call nz, i_neg_dehl         ; take absolute value of divisor

   call l0_small_divu_32_32x32

   ; dehl = unsigned quotient
   ; dehl'= unsigned remainder

   pop bc                      ; bc = sign info
   
   ld a,b
   xor c
   call m, i_neg_dehl          ; negate quotient if signs different
   
   bit 7,b
   jp z, i_div_end             ; if dividend > 0

   exx
   call i_neg_dehl             ; make remainder negative
   exx
   
   ; quotient = dehl
   ; remainder = dehl'

i_div_end:
    ret

divide_zero_s:

; TODO: this should be putting some values back on the stack too!
    dec de
    scf
    ret
   ; unsigned division of 32-bit numbers
   ;
   ; enter : dehl = 32-bit divisor
   ;         dehl'= 32-bit dividend
   ;
   ; exit  : success
   ;
   ;            dehl = 32-bit quotient
   ;            dehl'= 32-bit remainder
   ;            carry reset
   ;
   ;         divide by zero
   ;
   ;            dehl = $ffffffff = ULONG_MAX
   ;            dehl'= dividend
   ;            carry set, errno = EDOM
   ;
   ; uses  : af, bc, de, hl, bc', de', hl'

i_div_un:
    pop af
    ex af, af'

    pop de
    pop hl
    exx
    pop de
    pop hl
    exx

   ld a,d
   or e
   or h
   or l
   jr z, divide_zero  

   call l0_small_divu_32_32x32

   push hl
   push de

   ex af, af'
   push af
   ret


l0_small_divu_32_32x32:

   xor a
   push hl
   exx
   ld c,l
   ld b,h
   pop hl
   push de
   ex de,hl
   ld l,a
   ld h,a
   exx
   pop bc
   ld l,a
   ld h,a

 l1_small_divu_32_32x32:
   
   ; dede' = 32-bit divisor
   ; bcbc' = 32-bit dividend
   ; hlhl' = 0

   ld a,b
   ld b,32

loop_0:

   exx
   rl c
   rl b
   exx
   rl c
   rla
   
   exx
   adc hl,hl
   exx
   adc hl,hl
   
   exx
   sbc hl,de
   exx
   sbc hl,de
   jr nc, loop_1

   exx
   add hl,de
   exx
   adc hl,de

loop_1:

   ccf
   djnz loop_0

   exx
   rl c
   rl b
   exx
   rl c
   rla

   ; quotient  = acbc'
   ; remainder = hlhl'
   
   push hl
   exx
   pop de
   push bc
   exx
   pop hl
   ld e,c
   ld d,a

   ret

divide_zero:

; TODO: this should be putting some values back on the stack too!
    dec de
    scf
    ret
;==================================================
; MULTIPLY ROUTINE 32*32BIT=32BIT
; H'L'HL = B'C'BC * D'E'DE
; NEEDS REGISTER A, CHANGES FLAGS
;

i_mul:
    pop af                  ; save return address in a'f'
    ex af, af'

    exx                     ; populate b'c'bc
    pop bc                  ; from stack
    exx
    pop bc

    exx                     ; populate d'e'de
    pop de                  ; from stack
    exx
    pop de

    AND     A               ; RESET CARRY FLAG
    SBC     HL,HL           ; LOWER RESULT = 0
    EXX
    SBC     HL,HL           ; HIGHER RESULT = 0
    LD      A,B             ; MPR IS AC'BC
    LD      B,32            ; INITIALIZE LOOP COUNTER
i_mul_1:
    SRA     A               ; RIGHT SHIFT MPR
    RR      C
    EXX
    RR      B
    RR      C               ; LOWEST BIT INTO CARRY
    JR      NC, i_mul_2
    ADD     HL,DE           ; RESULT += MPD
    EXX
    ADC     HL,DE
    EXX
i_mul_2:
    SLA     E               ; LEFT SHIFT MPD
    RL      D
    EXX
    RL      E
    RL      D
    DJNZ    i_mul_1
    EXX

    push hl                 ; put h'l'hl
    exx                     ; onto stack
    push hl                 ; as result
    exx

    ex af, af'
    push af
       
    RET
i_neg:
	pop bc	; save return address

	pop de
	pop hl
	call i_neg_dehl
	push hl
	push de

	push bc
	ret

; negate dehl
;
; enter : dehl = long
;
; exit  : dehl = -long
;
; uses  : af, de, hl, carry unaffected

i_neg_dehl:
   ld a,l
   cpl
   ld l,a
   
   ld a,h
   cpl
   ld h,a
   
   ld a,e
   cpl
   ld e,a
   
   ld a,d
   cpl
   ld d,a
   
   inc l
   ret nz
   
   inc h
   ret nz
   
   inc de
   ret   ; unsigned division of 32-bit numbers
   ;
   ; enter : dehl = 32-bit divisor
   ;         dehl'= 32-bit dividend
   ;
   ; exit  : success
   ;
   ;            dehl = 32-bit quotient
   ;            dehl'= 32-bit remainder
   ;            carry reset
   ;
   ;         divide by zero
   ;
   ;            dehl = $ffffffff = ULONG_MAX
   ;            dehl'= dividend
   ;            carry set, errno = EDOM
   ;
   ; uses  : af, bc, de, hl, bc', de', hl'

i_rem:
    pop af
    ex af, af'

    pop de
    pop hl
    exx
    pop de
    pop hl
    exx

   ld a,d
   or e
   or h
   or l
   jr z, i_rem_divide_zero  

   call l0_small_div_32_32x32

   exx
   push hl
   push de
   exx

   ex af, af'
   push af
   ret

i_rem_divide_zero:

    dec de
    scf
    ret
   ; unsigned division of 32-bit numbers
   ;
   ; enter : dehl = 32-bit divisor
   ;         dehl'= 32-bit dividend
   ;
   ; exit  : success
   ;
   ;            dehl = 32-bit quotient
   ;            dehl'= 32-bit remainder
   ;            carry reset
   ;
   ;         divide by zero
   ;
   ;            dehl = $ffffffff = ULONG_MAX
   ;            dehl'= dividend
   ;            carry set, errno = EDOM
   ;
   ; uses  : af, bc, de, hl, bc', de', hl'

i_rem_un:
    pop af
    ex af, af'

    pop de
    pop hl
    exx
    pop de
    pop hl
    exx

   ld a,d
   or e
   or h
   or l
   jr z, i_rem_un_divide_zero  

   call l0_small_divu_32_32x32

   exx
   push hl
   push de
   exx

   ex af, af'
   push af
   ret

i_rem_un_divide_zero:

    dec de
    scf
    ret
; This routine performs the operation DEHL = DEHL - BCAF
;
; Uses: HL, DE, BC, AF, AF'

i_sub:	
	POP AF		; Save return address
	EX AF, AF'

	POP BC
	POP AF
	POP DE
	POP HL

	PUSH BC
	PUSH AF

	OR A 

	POP BC
	SBC HL, BC

	POP BC
	EX DE, HL
	SBC HL, BC
	EX DE, HL

	PUSH HL		; Put result back on stack
	PUSH DE

	EX AF, AF'	; Restore return address
	PUSH AF

	RET
l_inc_dehl:

   ; increment 32-bit value
   ;
   ; enter : dehl = 32 bit number
   ;
   ; exit  : dehl = dehl + 1
   ;
   ; uses  : f, de, hl

   inc l
   ret nz
   
   inc h
   ret nz
   
   inc de
   ret
PRINT:
	LD A, (HL)	
	CP 0
	JR Z, PRINTEND
	
	LD C, 2
	LD D, 0
	LD E, A
	PUSH HL
	CALL 5	; CALL bdos
	POP HL
	;CALL 33H
	
	INC HL
	JR PRINT
PRINTEND:
	RET
SETXY:
	POP BC   ; return address

	POP HL
	POP HL   ; y

	POP DE
	POP DE   ; x

	ADD HL, HL ; multiply y by 64
	ADD HL, HL
	ADD HL, HL
	ADD HL, HL
	ADD HL, HL
	ADD HL, HL
	ADD HL, DE ; add x
	LD DE, 3C00H
	ADD HL, DE

	PUSH BC  ; restore return address

	LD (4020H), HL	

	RET; This routine widens a 16 bit signed int to a 32 bit signed int
;
; Uses: HL, DE, BC


stoi:	
	POP BC		; Save return address

	POP DE

	LD H, D
	ADD HL, HL
	SBC HL, HL

	PUSH DE
	PUSH HL

	PUSH BC		; Restore return address
	RET
; 32 bit comparison routine
;
; Entry dehl = secondary
;       on stack = primary (under two return addresses)
;
; Exit: z = number is zero
;       nz = number is non zero
;       c = number is negative
;       nc = number is positive

i_cmp:
	POP BC
	EXX

	POP BC

	POP HL
	POP DE

	PUSH BC
	LD A, L

	EXX

	PUSH BC

	SUB L
	LD L, A

	EXX
	LD A, H
	EXX

	SBC A, H
	LD H, A

	EXX
	LD A, E
	EXX

	SBC A, E
	LD E, A

	EXX 
	LD A, D
	EXX

	SBC A, D
	LD D, A

	ADD A, A
	JR C, i_cmp_1

	LD A, H
	OR L
	OR D
	OR E

i_cmp_1:
	LD HL, 1
	RET
; 32 bit signed equal comparison
; Args from Stack, DEHL == BCAF
; Carry set if true

i_eq:
	POP DE
	EXX

	POP HL
	POP DE

	POP BC
	LD A, C
	CP L
	JR NZ, i_eq_1

	LD A, B
	CP H
	JR NZ, i_eq_1

	POP BC

	LD A, C
	CP E
	JR NZ, i_eq_2

	LD A, B
	CP D
	JR NZ, i_eq_2

	SCF

	JP i_eq_3

i_eq_1:

	POP BC

i_eq_2:

	XOR A

i_eq_3
	EXX
	PUSH DE

	RET
; 32 bit signed greater than equal comparison
; Entry: primary, secondary on stack
; Exit: Carry set if true
i_ge:
	POP BC

	POP HL
	POP DE

	PUSH BC

	CALL i_cmp

	CCF
	RET C

	SCF
	RET Z

	DEC L
	CCF
	RET
; 32 bit signed greater than comparison
; Entry: primary, secondary on stack
; Exit: Carry set if true
i_gt:
	POP BC

	POP HL
	POP DE

	PUSH BC

	CALL i_cmp
	JR Z, i_gt_1

	CCF
	RET C

i_gt_1:
	DEC L
	RET
; 32 bit signed less than equal comparison
; Entry: primary, secondary on stack
; Exit: Carry set if true
i_le:
	POP BC

	POP HL
	POP DE

	PUSH BC

	CALL i_cmp
	RET C
	
	SCF
	RET Z

	DEC L

	OR A
	RET
; 32 bit signed less than comparison
; Entry: primary, secondary on stack
; Exit: Carry set if true
i_lt:
	POP BC

	POP HL
	POP DE

	PUSH BC

	CALL i_cmp
	RET C
	
	DEC L
	RET
; 32 bit signed not equals comparison
; Args from Stack, DEHL != BCAF
; Carry set if true

i_neq:
	POP DE
	EXX

	POP HL
	POP DE

	POP BC
	LD A, C
	CP L
	JR NZ, i_neq_1

	LD A, B
	CP H
	JR NZ, i_neq_1

	POP BC

	LD A, C
	CP E
	JR NZ, i_neq_2

	LD A, B
	CP D
	JR NZ, i_neq_2

	JP i_neq_3

i_neq_1:

	POP BC

i_neq_2:
	SCF

i_neq_3
	EXX
	PUSH DE

	RET
; Write a character to current cursor position
; Top of stack contains character to write (low byte)
WRITE:
	POP BC	; return address

	POP DE
	POP HL

	PUSH BC	; put return address back

	LD A, L
	JP 33H
	END START
