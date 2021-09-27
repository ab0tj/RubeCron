    org 09000h

    ld a, 00000000b
    out (040h), a
    ld a, 00000001b
    out (041h), a
    ld a, 00100010b
    out (042h), a
    ld a, 00100011b
    out (043h), a
    ld a, 1
    out (048h), a

    jp 08000h
