UART_BASE: equ 020h
include 'uart.inc'
    
    org 09000h

loop:
    in a, (UART_SRB)
    bit 0, a            ; check bit 0 (RxRDYB)
    jp z, loop         ; loop until a byte comes in
    in a, (UART_FIFOB)  ; get the byte
    out (UART_FIFOA), a
    jp loop