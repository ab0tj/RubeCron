; RubeCron Flash Programmer

.hexfile rubeprg.hex
.binfile rubeprg.bin
.download hex

.org 8000h         ; after reset, 0-7fff is rom, 8000-ffff is ram
startmem                equ 08000h
endmem                  equ 0ffffh

stack   equ endmem - 010h ; stack pointer

; i/o ports
usb_data        equ 030h        ; usb module
usb_stat        equ 003h        ; 8155 port c
banksel		equ 040h	; bank select port

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

prompt:	; ask user what bank we will be writing to
	lxi h, bank_str
	call usb_in_prompt
	sui 30h			; ascii adjust
	cpi 4
	jnc prompt		; check input
	mov d,a			; save bank number
	
erase_p: ; make sure the user knows what they are doing
	lxi h, confirm_str	; ask
	call usb_in_prompt
	cpi 'y'			; yes?
	jz erase		;   go!
	cpi 'n'			; no?
	jz prompt		;   try again
	jmp erase_p		; user didn't enter y or n

erase: ; erase the entire bank
	mov a,d			; d=bank number
	out banksel		; set bank

	lxi h, erase_str
	call usb_out_str	; "Erasing"

	xra a			; a=0
	mov h,a			; h=0
	mov l,a			; l=0

erase_loop: ; tell the chip to erase a sector
	mvi a, 0aah		; send
	sta 5555h		;  the
	mvi a, 55h		;  sector
	sta 2aaah		;  erase
	mvi a, 80h		;  command
	sta 5555h
	mvi a, 0aah
	sta 5555h
	mvi a, 55h
	sta 2aaah
	mvi m, 30h
	
	mvi c, '.'		; show progress
	call usb_out

erase_wait: ; wait for the sector erase to complete
	mov a,m			; read the byte at hl
	ani 80h			; mask out "DQ7"
	jz erase_wait		; DQ7 will be 0 during erase operation

	; erase done, start next sector
	mov a,h			; get current sector number
	adi 10h			; increment it
	cpi 80h			; last sector?
	mov h,a			; update sector number
	jnz erase_loop		; keep going if not.

; ready to write new data
	lxi h, send_str		; "Ready..."
	call usb_out_str

memin_intel: ; read data from the console, write it to flash
        call usb_out_crlf

memin_intel_start:
        call usb_in
        cpi ':'                 ; start code
        jz memin_intel_1
        cpi CtrlC               ; user wants to quit?
        jz prompt		;   then go back to start
        jmp memin_intel_start

memin_intel_1: ; get first part of header
        call usb_in_hex        ; byte count
        mov b, a
        call usb_in_hex        ; address high
        mov h, a
        call usb_in_hex        ; address low
        mov l, a

	mov a,h			; check address
	cpi 80h			; make sure it is in lower 32k
	jc memin_intel_2	; keep going if so

	lxi h, rangeerr_str	; out of range message
	call usb_out_str
	jmp memin_intel		; wait for next record

memin_intel_2: ; get second part of header
        mov a, b
        add h
        add l                   ; start of checksum
        mov e, a

        push d			; usb_in_hex clobbers e
        call usb_in_hex        ; record type
        pop d
        ana a                   ; 0=data
        jz memin_intel_3
        cpi 1                   ; 1=eof
        jz memin_intel_eof
	lxi h, unsupp_str	; unsupported message
	call usb_out_str
        jmp memin_intel 	; not supported, wait for another record

memin_intel_3: ; get and program data
        mov a, b		; check byte counter
        ana a
        jz memin_intel_cksum    ; b=0, checksum comes next
        push d                  ; e will get clobbered by input
        call usb_in_hex
        pop d
        push a                  ; save byte

	mvi a, 0aah		; send
	sta 5555h		;   the
	mvi a, 55h		;   program
	sta 2aaah		;   command
	mvi a, 0a0h
	sta 5555h

	pop a			; get the data byte again
	mov m,a			; program the byte
	mov c,a			; save for the wait that follows

program_wait: ; wait for the program operation to finish
	mov a,m			; read current byte
	cmp c			; check if programming has finished
	jnz program_wait	; loop until it finishes

        add e                   ; add to checksum
        mov e, a
        inx h                   ; increment pointer
        dcr b                   ; decrement counter
        jmp memin_intel_3       ; next byte

memin_intel_cksum:
        push d                  ; input clobbers e
        call usb_in_hex        ; get checksum
        pop d
        add e
        jz memin_intel          ; checksum is good, get next record
        lxi h, ckerr_str
        call usb_out_str       ; show error on console
        jmp memin_intel         ; get next record

memin_intel_eof: ; done!
        call usb_in_hex        ; checksum

	lxi h, done_str		; tell the user
	call usb_out_str	;   that we're done
	hlt			;   and halt

; hardware support routines
usb_in_prompt: ; ask the user a question
	call usb_out_str	; prompt string address in hl
	call usb_in		; get the answer
	mov c,a			; echo it
	call usb_out
	ret			; answer in a and c at this point

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
        jz prompt               ; bail if so
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
bank_str: db CR,LF,"Bank to program (0-3)? ",0
confirm_str: db CR,LF,"About to erase. Are you sure (y/n)? ",0
erase_str: db CR,LF,"Erasing",0
send_str: db CR,LF,"Ready. Send data now.",0
ckerr_str: db " cksum err",0
rangeerr_str: db " address >32K",0
unsupp_str: db " unsupported",0
done_str: db CR,LF,"EOF.",0

end: