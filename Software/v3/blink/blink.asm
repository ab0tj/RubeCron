MID_BASE:   equ 010h
include 'tp3465v.inc'

    org 04000h

    xor a
    out (MID_PD),a
    inc a
    out (MID_CS),a

    xor a
    ld b,a
    ld c,a

main_loop:
    xor a
    out (MID_CS),a
    inc a
    out (MID_CS),a
delay:
    inc bc
    xor a
    cp b
    jp nz,delay
    cp c
    jp nz,delay

    jp main_loop