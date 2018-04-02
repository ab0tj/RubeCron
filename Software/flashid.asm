; RubeCron Flash ID Check

.hexfile rubefid.hex
.binfile rubefid.bin
.download hex

.org 8000h         ; 0-7fff is rom, 8000-ffff is ram
startmem                equ 08000h
endmem                  equ 0ffffh

stack   equ endmem - 010h ; stack pointer

; i/o ports
usb_data        equ 030h        ; usb module
usb_stat        equ 003h        ; 8155 port c

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

	mvi a, 0aah	; enter Software ID mode
	sta 5555h
	mvi a, 55h
	sta 2aaah
	mvi a, 90h
	sta 5555h

	lxi h, manf_str
	call usb_out_str

	lda 0			; get manufacturer id
	mov d,a			; save in d
	call usb_out_hex	; send to console

	lxi h, dev_str
	call usb_out_str
	
	lda 1			; get device id
	mov d,a			; save in d
	call usb_out_hex	; send to console

	mvi a, 0f0h		; exit id mode
	sta 0

	lxi h, key_str
	call usb_out_str

wait:	in usb_stat	; get usb status
	ani usb_rx_rdy	; mask out ready bit
	jnz wait	; loop until ready
	in usb_data	; get the byte from the buffer
	jmp 0		; return to monitor
	

; hardware support routines
usb_out_hex: ; send byte in d as ascii hex digits
        mov a, d
        rar
        rar
        rar
        rar                     ; get the high nibble
        call bin2hex
        mov c, a
        call usb_out           ; send first digit
        mov a, d
        call bin2hex
        mov c, a
        call usb_out           ; send second digit
        ret

bin2hex: ; convert low nibble of a to ascii hex digit
        ani 0fh                 ; mask off high nibble
        adi 90h
        daa
        aci 40h
        daa
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

; strings (null terminated)
manf_str: db CR,LF,"Manufacturer ID: ",0
dev_str:  db CR,LF,"Device ID: ",0
key_str: db CR,LF,"Press any key to continue.",CR,LF,0
end: