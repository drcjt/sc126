        org 	100h
bdos:   equ     5
start:  ld      de,msg
        ld      c,9
        call    bdos
        ret
msg:    defb    "Hello,world!",0dh,0ah,"$"
        end