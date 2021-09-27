; RubeCron Hex Loader
; Switches low bank to memory and loads a program there

.hexfile rubeldr.hex
.binfile rubeldr.bin
.download hex

.org 8000h         ; after reset, 0-7fff is rom, 8000-ffff is ram
startmem                equ 08000h
endmem                  equ 0ffffh

stack   equ endmem - 010h ; stack pointer

; i/o ports
usb_data        equ 030h        ; usb module
usb_stat        equ 003h        ; 8155 port c
bnksel		equ 040h	; bank selection port

; Bit Masks
usb_rx_rdy     equ 20h
usb_tx_rdy     equ 10h

; ascii
BS equ 08h
CR equ 0Dh
LF equ 0Ah
SPC equ 020h
CtrlC equ 03h

; CODE STARTS HERE
;main program
        lxi sp, stack   ; initialize the stack pointer

; initialize hardware, show signon message

        call usb_out_crlf      ; start on a fresh line
	mvi a, 5		; ram1+ram0 config
	out bnksel		; select lower bank as ram
signon:
	lxi sp, stack   	; just in case
        lxi h, signon_str       ; show signon
        call usb_out_str

memin_intel:
        call usb_out_crlf
memin_intel_start:
        call usb_in
        cpi ':'                 ; start code
        jz memin_intel_1
        cpi CtrlC               ; bail
        jz signon
        jmp memin_intel_start
memin_intel_1:
        call usb_in_hex        ; byte count
        mov b, a
        call usb_in_hex        ; address high
        mov h, a
        call usb_in_hex        ; address low
        mov l, a

        mov a, b
        add h
        add l                   ; start of checksum
        mov e, a

        push d
        call usb_in_hex        ; record type
        pop d
        cpi 0                   ; data
        jz memin_intel_2
        cpi 1                   ; eof
        jz memin_intel_eof
        jmp memin_intel ; not supported, wait for another record
memin_intel_2:
        mov a, b
        ana a
        jz memin_intel_cksum    ; b=0, checksum comes next
        push d                  ; e will get clobbered by input
        call usb_in_hex
        pop d
        mov m, a                ; save byte
        add e                   ; add to checksum
        mov e, a
        inx h                   ; increment pointer
        dcr b                   ; decrement counter
        jmp memin_intel_2       ; next byte
memin_intel_cksum:
        push d                  ; input clobbers e
        call usb_in_hex        ; get checksum
        pop d
        add e
        jz memin_intel          ; checksum is good, get next record
        lxi h, ckerr_str
        call usb_out_str       ; show error on console
        jmp memin_intel         ; get next record
memin_intel_eof:
        call usb_in_hex        ; checksum

	lxi h, addr_str		; ask the user where to jump to
	call usb_out_str
	call usb_in_hexaddr	; get the address
	pchl			; jump

; hardware support routines
usb_in_hexaddr: ; get an address from the console in hex, return in hl
        call usb_in_hex        ; high byte
        mov h, a
        call usb_in_hex        ; low byte
        mov l, a
        ret

usb_in_hex: ; get a byte from the console in ascii hex
        call usb_in_hexnib     ; get high nibble
        ral
        ral
        ral
        ral                     ; shift to the high nibble
        ani 0f0h                ; mask off low nibble
        mov e, a                ; store the first nibble
        call usb_in_hexnib     ; get low nibble
        ora e                   ; combine high and low nibble
        ret                     ; return with byte in a

usb_in_hexnib: ; get a single hex digit from the console, return in a
        call usb_in            ; get digit from console
        cpi CtrlC               ; ctrl-z?
        jz signon               ; bail if so
        cpi 061h                ; lowercase?
        jc hexnib1              ; nope
        sui 20h                 ; lower -> upper
hexnib1:
        mov c, a                ; save ascii version
        call hex2bin            ; convert to bin
        out 0ffh                ; DEBUG
        cpi 010h                ; carry if 0-F
        jnc usb_in_hexnib      ; try again
        mov d, a                ; a will get clobbered by usb_out
        call usb_out           ; echo
        mov a, d
        ret

hex2bin: ; convert ascii hex digit to binary nibble in a
        cpi 041h                ; >=A?
        jc hex2bin1             ; <A.
        sui 7                   ; correct for A-F
hex2bin1:
        sui 030h                ; get binary value
        ret

usb_out_crlf: ; newline without clobbering de
        mvi c, CR
        call usb_out
        mvi c, LF
        call usb_out
        ret

usb_out_str: ; send string at hl to the console
        mov a, m                ; get a byte to send
        ora a                   ; is it null?
        rz                      ; we're done if so
        mov c, a
        call usb_out           ; send this byte
        inx h                   ; increment the pointer
        jmp usb_out_str        ; loop until we find a null

usb_out: ; send byte in c to the console
        in usb_stat             ; check fifo status
        ani usb_tx_rdy
        jnz usb_out             ; loop until we can send this byte
        mov a, c
        out usb_data           ; send the byte
        ret

usb_in: ; get a byte from the console, store in a
        in usb_stat             ; check fifo status
        ani usb_rx_rdy
        jnz usb_in              ; loop until a byte comes in
        in usb_data            ; get the byte
        ret

; strings (null terminated)
crlf_str: db CR,LF,0
signon_str: db CR,LF,"Ready, send file now.",0
addr_str:db CR,LF,"Start address? ",0
ckerr_str: db " cksum err",0

end: