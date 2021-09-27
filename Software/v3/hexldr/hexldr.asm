UART_BASE:  equ 020h
include 'uart.inc'

; at boot, RAM occupies the top 3 pages
startmem:   equ 04000h
endmem:     equ 0ffffh

; ascii
BS:         equ 08h
CR:         equ 0Dh
LF:         equ 0Ah
SPC:        equ 020h
CtrlC:      equ 03h

    org 0

    ; initialize UART
    ld a, 10111010b         ; reset MRA pointer, disable TX and RX
    out (UART_CRA), a
    ld a, 00000001b         ; MR0A: Extended mode 1 baud rates
    out (UART_MRA), a
    ld a, 10010011b         ; MR1A: RTS, no parity, 8 bits
    out (UART_MRA), a
    ld a, 00010111b         ; MR2A: normal mode, CTS, 1 stop bit
    out (UART_MRA), a
    ld a, 11001100b         ; CSRA: 115200 baud
    out (UART_CSRA), a
    ld a, 10110000b         ; ACR: Select second baud rate option
    out (UART_ACR), a
    ld a, 10000101b         ; Set RTS, Enable TX and RX
    out (UART_CRA), a

; Main program starts here
signon:
    ld sp, stack            ; Initialize stack pointer
    xor a
    ld (error), a           ; Reset error flag
    ld hl, str_ready
    call outstr             ; Show signon message

memin_intel:
    call crlf
memin_intel_start:
    call chrin
    cp ':'                  ; wait for start code
    jp z, memin_intel_1
    cp CtrlC                ; bail if user sends Ctrl-C
    jp z, signon
    jp memin_intel_start
memin_intel_1:
    call in_hex             ; byte count
    ld b, a
    call in_hex             ; address high
    ld h, a
    call in_hex             ; address low
    ld l, a

    ld a, b
    add a, h
    add a, l                ; start of checksum
    ld e, a

    push de
    call in_hex             ; record type
    pop de
    cp 0                    ; data
    jp z, memin_intel_2
    cp 1                    ; eof
    jp z, memin_intel_eof
    push hl
    ld hl, str_unsup
    call outstr
    pop hl
    jp memin_intel          ; not supported, wait for another record
memin_intel_2:
    ld a, b
    and a
    jp z, memin_intel_cksum ; b=0, checksum comes next
    push de                 ; e will get clobbered by input
    call in_hex
    pop de
    ld (hl), a              ; save byte
    add a, e                ; add to checksum
    ld e, a
    inc hl                  ; increment pointer
    dec b                   ; decrement counter
    jp memin_intel_2        ; next byte
memin_intel_cksum:
    push de                 ; input clobbers e
    call in_hex             ; get checksum
    pop de
    add a, e
    jp z, memin_intel       ; checksum is good, get next record
    ld hl, str_cksum
    call outstr             ; show error on console
    ld a, 1                 ; set error flag
    ld (error), a
    jp memin_intel          ; get next record
memin_intel_eof:
    call in_hex             ; checksum

	ld hl, str_addr  		; ask the user where to jump to
	call outstr
	call in_hexaddr         ; get the address
	jp (hl)     			; jump

; Hex functions
in_hexaddr: ; get an address from the console in hex, return in hl
    call in_hex             ; high byte
    ld h, a
    call in_hex             ; low byte
    ld l, a
    ret

in_hex: ; get a byte from the console in ascii hex
    call in_hexnib          ; get high nibble
    rla
    rla
    rla
    rla                     ; shift to the high nibble
    and 0f0h                ; mask off low nibble
    ld e, a                 ; store the first nibble
    call in_hexnib          ; get low nibble
    or e                    ; combine high and low nibble
    ret                     ; return with byte in a

in_hexnib: ; get a single hex digit from the console, return in a
    call chrin              ; get digit from console
    cp CtrlC                ; ctrl-c?
    jp z, signon            ; bail if so
    cp 061h                 ; lowercase?
    jp c, hexnib1           ; nope
    sub 20h                 ; lower -> upper
hexnib1:
    ld c, a                 ; save ascii version
    call hex2bin            ; convert to bin
    cp 010h                 ; carry if 0-F
    jp nc, in_hexnib        ; try again
    ld d, a                 ; a will get clobbered by outc
    call outc               ; echo
    ld a, d
    ret

hex2bin: ; convert ascii hex digit to binary nibble in a
    cp 041h                 ; >=A?
    jp c, hex2bin1          ; <A.
    sub 7                   ; correct for A-F
hex2bin1:
    sub 030h                ; get binary value
    ret

; UART functions
outc:   ; sent a single byte to the UART
    in a, (UART_SRA)    ; get UART status
    bit 2, a            ; check bit 2 (TxRDYA)
    jp z, outc          ; loop if not ready
    ld a, c             ; get byte to send
    out (UART_FIFOA), a ; send to UART
    ret                 ; done

outstr: ; send string at hl to the UART
    ld a, (hl)          ; get a byte to send
    or a                ; is it null?
    ret z               ; we're done if so
    ld c, a
    call outc           ; send this byte
    inc hl              ; increment the pointer
    jp outstr           ; loop until we find a null

crlf: ; send newline to UART
    ld c, CR
    call outc
    ld c, LF
    call outc
    ret

chrin: ; get a byte from the UART, store in a
    in a, (UART_SRA)    ; check fifo status
    bit 0, a            ; check bit 0 (RxRDYA)
    jp z, chrin         ; loop until a byte comes in
    in a, (UART_FIFOA)  ; get the byte
    ret
    
; Strings
str_ready: db CR,LF,'Z80 RubeCron Hex Loader',CR,LF,'2021, AB0TJ',CR,LF,CR,LF,'Ready. Send hex file now.',0
str_addr: db 'Enter start address: ',0
str_error: db 'Errors detected, try again.',CR,LF,0
str_ok: db ' OK',CR,LF,0
str_cksum: db ' CKSUM',CR,LF,0
str_unsup: db ' UNSUP',CR,LF,0

; Varable space
    org endmem - 0      ; update to subract any bytes used for variables
stack:                  ; stack top here

error: ds 1             ; keep track of any errors
